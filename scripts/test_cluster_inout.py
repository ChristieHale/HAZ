"""Tests of a fault system with in- and out-of-cluster branches (Phase 6, A9).

Test D (tests/cluster/TD_InOut) is test C's cluster plus RFT-S and RFT-L as
ordinary segments in the same fault system, with eleven segmentation models
as in HID Figure H-5.5-1: the eight geometry combinations of the cluster
(0.9 x the geometry weight), RFT-S or RFT-L alone (0.05 x 0.7 / 0.3), and no
segment at all (0.05). HAZ is run on the Members and Cluster inputs and the
Cluster run is compared with the reference calculation, which uses only the
branch curves of the Members run. Set HAZ_BIN to the executable; the tests are
skipped when it cannot be found. Run with pytest from scripts/.
"""

import os
import shutil
import subprocess

import numpy as np
import pytest

import cluster_reference as cr
import io_tools
from test_cluster_branches import (GMM_WEIGHTS, FTYPE_WEIGHTS, PARAM_RATES, PARAM_WEIGHTS, WIDTH_WEIGHTS,
                                   FLAGS as FLAGS_C, SEG_WEIGHTS as SEG_WEIGHTS_C)

HERE = os.path.dirname(os.path.abspath(__file__))
CASE = os.path.join(HERE, "..", "tests", "cluster", "TD_InOut")

HAZ_BIN = os.environ.get("HAZ_BIN", os.path.join(HERE, "..", "build", "HAZ"))
pytestmark = pytest.mark.skipif(not os.path.isfile(HAZ_BIN), reason="set HAZ_BIN to the HAZ executable")

MEMBERS = [1, 2, 3, 4, 5, 6]
ORDINARY = [7, 8]                                  # RFT-S and RFT-L, out of cluster
N_FLT = 8
N_SEG = 11
SEG_WEIGHTS_IN = 0.9 * SEG_WEIGHTS_C               # models 1-8
ORDINARY_WEIGHTS = np.array([0.035, 0.015])        # models 9 and 10 (model 11 is empty)
OUT_RATES = np.array([7.2e-4, 7.2e-4, 2.2e-4, 2.2e-4])   # rate (outer) x rupture set (inner)

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
    root = tmp_path_factory.mktemp("cluster_inout")
    out = {}
    for kind in ("Members", "Cluster"):
        folder = root / kind
        shutil.copytree(os.path.join(CASE, kind, "Input"), folder)
        run_haz(folder, f"Run_TD_{kind}.txt")
        out[kind] = folder
    return out


def branch_weights():
    return (WIDTH_WEIGHTS[:, None, None, None, None] * GMM_WEIGHTS[None, :, None, None, None]
            * FTYPE_WEIGHTS[None, None, :, None, None] * PARAM_WEIGHTS[None, None, None, :, None])


def reference(runs, site, iprob):
    members = cr.read_out1(os.path.join(runs["Members"], f"TD_Members_Site{site}.out1"))
    p = cr.member_branch_probabilities(members, MEMBERS, iprob)
    q = cr.member_branch_probabilities(members, ORDINARY, iprob)   # ordinary segments' P (rate 1)
    pc, s, factor = cr.cluster_segmentation(p, FLAGS_C, SEG_WEIGHTS_IN)
    return p, q, pc, factor


SITES_PERIODS = [(1, 1), (1, 2), (2, 1), (2, 2)]


@pytest.mark.parametrize("site, iprob", SITES_PERIODS)
def test_out1(runs, site, iprob):
    """Members' blocks are zero; the eleven models' cluster blocks follow the
    last member (zeros for the models with no member); the ordinary segments
    keep their own curves."""
    p, q, pc, factor = reference(runs, site, iprob)
    blocks = cr.read_out1(os.path.join(runs["Cluster"], f"TD_Cluster_Site{site}.out1"))
    order = [b["iflt"] for b in blocks if b["iprob"] == iprob and b["iwidth"] == 1]
    assert order == MEMBERS + list(range(N_FLT + 1, N_FLT + N_SEG + 1)) + ORDINARY
    got = {(b["iflt"], b["iwidth"]): b["curves"] for b in blocks if b["iprob"] == iprob}
    for w in range(3):
        for k in MEMBERS:
            assert np.all(got[k, w + 1] == 0.0)
        for i_seg in range(N_SEG):
            block = got[N_FLT + 1 + i_seg, w + 1]
            if i_seg < 8:
                exp = PARAM_RATES[None, None, :, None] * pc[i_seg, w]
                big = exp > 1e-14
                np.testing.assert_allclose(block[big], exp[big], rtol=RTOL_OUT1)
            else:
                assert np.all(block == 0.0)
        for j, k in enumerate(ORDINARY):
            exp = OUT_RATES[None, None, :, None] * q[j, w]
            big = exp > 1e-14
            np.testing.assert_allclose(got[k, w + 1][big], exp[big], rtol=RTOL_OUT1)


@pytest.mark.parametrize("site, iprob", SITES_PERIODS)
def test_out3(runs, site, iprob):
    """Mean = 0.9-weighted cluster + 0.05-weighted ordinary RFT + nothing for
    'All Out'; each line of out3 is the source's share."""
    p, q, pc, factor = reference(runs, site, iprob)
    bw = branch_weights()
    alpha = PARAM_RATES[None, None, None, :, None]
    rate_out = OUT_RATES[None, None, None, :, None]
    cluster = (bw * alpha * np.einsum("s,s...->...", SEG_WEIGHTS_IN, pc)).sum(axis=(0, 1, 2, 3))
    seg_wt = FLAGS_C.T @ SEG_WEIGHTS_IN
    member_shares = [seg_wt[k] * (bw * alpha * factor[k] * p[k]).sum(axis=(0, 1, 2, 3)) for k in range(6)]
    ordinary_shares = [ORDINARY_WEIGHTS[j] * (bw * rate_out * q[j]).sum(axis=(0, 1, 2, 3)) for j in range(2)]
    total = cluster + sum(ordinary_shares)
    np.testing.assert_allclose(sum(member_shares), cluster, rtol=1e-12)

    out3 = io_tools.read_out3(os.path.join(runs["Cluster"], f"TD_Cluster_Site{site}.out3"))
    model = out3["models"][iprob - 1]
    big = total > 1e-14
    np.testing.assert_allclose(np.array(model["rate"])[big], total[big], rtol=RTOL_OUT3)
    for k, share in enumerate(member_shares + ordinary_shares):
        got = np.array(model["faults"][k]["rate"])
        big = share > 1e-14
        np.testing.assert_allclose(got[big], share[big], rtol=RTOL_OUT3, err_msg=model["faults"][k]["name"])
