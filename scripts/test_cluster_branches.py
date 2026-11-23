"""Tests of clusters with epistemic branches and segmentation models (Phase 5).

Test C (tests/cluster/TC_Geometries): six segments (two geometries of each of
NMS, RFT and NMN), eight segmentation models that pick one geometry of each
fault, and branches matched by index in every member: three thicknesses, two
rates times two rupture sets, two mechanisms, and two GMMs. HAZ is run on the
Members and Cluster inputs, and the Cluster run is compared with the
reference calculation in cluster_reference.py, which uses only the members'
branch curves from the Members run. Set HAZ_BIN to the executable; the tests
are skipped when it cannot be found. Run with pytest from scripts/.
"""

import os
import shutil
import subprocess

import numpy as np
import pytest

import cluster_reference as cr
import io_tools

HERE = os.path.dirname(os.path.abspath(__file__))
CASE = os.path.join(HERE, "..", "tests", "cluster", "TC_Geometries")

HAZ_BIN = os.environ.get("HAZ_BIN", os.path.join(HERE, "..", "build", "HAZ"))
pytestmark = pytest.mark.skipif(not os.path.isfile(HAZ_BIN), reason="set HAZ_BIN to the HAZ executable")

# Test C's branches, as written by tests/cluster/make_inputs.py
MEMBERS = [1, 2, 3, 4, 5, 6]          # BA-BL, BA-BFZ, RFT-S, RFT-L, NMN-S, NMN-L
N_FLT = 6
FLAGS = np.array([[1 - a, a, 1 - b, b, 1 - c, c] for a in (0, 1) for b in (0, 1) for c in (0, 1)])
SEG_WEIGHTS = np.array([round(x * y * z, 6) for x in (0.6, 0.4) for y in (0.7, 0.3) for z in (0.7, 0.3)])
WIDTH_WEIGHTS = np.array([0.3, 0.5, 0.2])
GMM_WEIGHTS = np.array([0.5, 0.5])
FTYPE_WEIGHTS = np.array([0.5, 0.5])
# Parameter branches: rate (outer) x rupture set (inner), each 0.5 x 0.5
PARAM_RATES = np.array([3.7e-3, 3.7e-3, 1.4e-3, 1.4e-3])
PARAM_WEIGHTS = np.array([0.25, 0.25, 0.25, 0.25])

RTOL_OUT1 = 2e-5
RTOL_OUT3 = 2e-3


def run_haz(folder, run_name):
    p = subprocess.run([os.path.abspath(HAZ_BIN)], cwd=folder, input=f"0\n0\n{run_name}\n".encode(),
                       capture_output=True, timeout=1800)
    out = p.stdout.decode(errors="replace") + p.stderr.decode(errors="replace")
    assert p.returncode == 0, out
    assert "Normal termination" in out
    return out


@pytest.fixture(scope="module")
def runs(tmp_path_factory):
    root = tmp_path_factory.mktemp("cluster_branches")
    out = {}
    for kind in ("Members", "Cluster"):
        folder = root / kind
        shutil.copytree(os.path.join(CASE, kind, "Input"), folder)
        run_haz(folder, f"Run_TC_{kind}.txt")
        out[kind] = folder
    return out


def branch_weights():
    """Weights of the branches, shape (nWidth, nAtten, nFtype, nParam, 1)."""
    return (WIDTH_WEIGHTS[:, None, None, None, None] * GMM_WEIGHTS[None, :, None, None, None]
            * FTYPE_WEIGHTS[None, None, :, None, None] * PARAM_WEIGHTS[None, None, None, :, None])


def reference(runs, site, iprob):
    members = cr.read_out1(os.path.join(runs["Members"], f"TC_Members_Site{site}.out1"))
    # The Members run has activity rate 1 on every branch, so its curves are P_k
    p = cr.member_branch_probabilities(members, MEMBERS, iprob)
    assert p.shape[:5] == (6, 3, 2, 2, 4)
    pc, s, factor = cr.cluster_segmentation(p, FLAGS, SEG_WEIGHTS)
    return p, pc, s, factor


def cluster_blocks(runs, site, iprob):
    blocks = cr.read_out1(os.path.join(runs["Cluster"], f"TC_Cluster_Site{site}.out1"))
    return {(b["iflt"], b["iwidth"]): b["curves"] for b in blocks if b["iprob"] == iprob}


SITES_PERIODS = [(1, 1), (1, 2), (2, 1), (2, 2)]


@pytest.mark.parametrize("site, iprob", SITES_PERIODS)
def test_out1_cluster_blocks(runs, site, iprob):
    """Members' blocks are zero; after them, one set of blocks per segmentation
    model, numbered nFlt + 1, ..., holding alpha * P_c,s on every branch."""
    p, pc, s, factor = reference(runs, site, iprob)
    blocks = cluster_blocks(runs, site, iprob)
    assert sorted(blocks) == [(k, w) for k in range(1, N_FLT + len(FLAGS) + 1) for w in (1, 2, 3)]
    for k in MEMBERS:
        for w in (1, 2, 3):
            assert np.all(blocks[k, w] == 0.0)
    for i_seg in range(len(FLAGS)):
        for w in range(3):
            got = blocks[N_FLT + 1 + i_seg, w + 1]
            exp = PARAM_RATES[None, None, :, None] * pc[i_seg, w]
            big = exp > 1e-14
            np.testing.assert_allclose(got[big], exp[big], rtol=RTOL_OUT1,
                                       err_msg=f"model {i_seg + 1}, width {w + 1}")
            assert np.all(got[~big] <= 2e-14)


@pytest.mark.parametrize("site, iprob", SITES_PERIODS)
def test_out3_total_and_member_shares(runs, site, iprob):
    """The mean is sum_s w_s sum_b w_b alpha_b P_c,s; member k's out3 line is
    its allocated share, and the shares add up to the mean."""
    p, pc, s, factor = reference(runs, site, iprob)
    bw = branch_weights()
    alpha = PARAM_RATES[None, None, None, :, None]
    total = np.einsum("s,s...->...", SEG_WEIGHTS, pc)
    total = (bw * alpha * total).sum(axis=(0, 1, 2, 3))
    seg_wt = FLAGS.T @ SEG_WEIGHTS
    shares = np.array([seg_wt[k] * (bw * alpha * factor[k] * p[k]).sum(axis=(0, 1, 2, 3)) for k in range(6)])
    np.testing.assert_allclose(shares.sum(axis=0), total, rtol=1e-12)

    out3 = io_tools.read_out3(os.path.join(runs["Cluster"], f"TC_Cluster_Site{site}.out3"))
    model = out3["models"][iprob - 1]
    got_total = np.array(model["rate"])
    big = total > 1e-14
    np.testing.assert_allclose(got_total[big], total[big], rtol=RTOL_OUT3)
    for k, fault in enumerate(model["faults"]):
        got = np.array(fault["rate"])
        big = shares[k] > 1e-14
        np.testing.assert_allclose(got[big], shares[k][big], rtol=RTOL_OUT3, err_msg=fault["name"])


@pytest.mark.parametrize("site, iprob", SITES_PERIODS)
def test_exact_not_above_approximation(runs, site, iprob):
    """Averaging each fault's curve over the branches and geometries before
    combining (the other code) overestimates the cluster hazard (A2.3)."""
    p, pc, s, factor = reference(runs, site, iprob)
    bw = branch_weights()
    alpha = PARAM_RATES[None, None, None, :, None]
    exact = (bw * alpha * np.einsum("s,s...->...", SEG_WEIGHTS, pc)).sum(axis=(0, 1, 2, 3))
    approx = cr.approximate_cluster_rate(p, FLAGS, SEG_WEIGHTS, bw, alpha)
    ok = exact > 1e-14
    # Where the P_k are small, both are sum_k P_k and agree to the 6 digits of out1
    assert np.all(approx[ok] >= exact[ok] * (1 - 1e-5))
    # At the lowest level every member is all but certain to exceed: both tend to alpha
    assert exact[0] == pytest.approx((PARAM_RATES * PARAM_WEIGHTS).sum(), rel=1e-3)


def test_summary_lists_segmentation_models(runs):
    with open(os.path.join(runs["Cluster"], "Run_TC_Cluster.txt.log"), errors="replace") as f:
        log = f.read()
    assert "Segmentation models:    8" in log
