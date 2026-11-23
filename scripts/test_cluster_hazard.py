"""Tests of the mean hazard of clustered sequences (source type 8).

These run the HAZ executable on the Members and Cluster inputs of tests A and
B (tests/cluster) and compare the cluster's hazard with the reference
calculation in cluster_reference.py, which uses only the members' exceedance
probabilities from the Members runs. Set HAZ_BIN to the executable; the tests
are skipped when it cannot be found. Run with pytest from scripts/.

See docs/Cluster_Source_Spec.md, Phase 2.
"""

import os
import shutil
import subprocess

import numpy as np
import pytest

import cluster_reference as cr
import io_tools

HERE = os.path.dirname(os.path.abspath(__file__))
CASES = os.path.join(HERE, "..", "tests", "cluster")

HAZ_BIN = os.environ.get("HAZ_BIN", os.path.join(HERE, "..", "build", "HAZ"))
pytestmark = pytest.mark.skipif(not os.path.isfile(HAZ_BIN), reason="set HAZ_BIN to the HAZ executable")

# Rate of the sequence in the test inputs (mean Poisson in-cluster rate, HID Table H-5.5-2)
ALPHA = 2.65702e-3
MEMBERS = [1, 2, 3]

# out1 has 6 significant digits, out3 has 4
RTOL_OUT1 = 2e-5
RTOL_OUT3 = 2e-3

SOURCE_8 = "8  1  1  1.0  0  0 "
SOURCE_1 = "1  1  1  1.0  0  0 "


def run_haz(folder, run_name):
    p = subprocess.run(
        [os.path.abspath(HAZ_BIN)],
        cwd=folder,
        input=f"0\n0\n{run_name}\n".encode(),
        capture_output=True,
        timeout=600,
    )
    out = p.stdout.decode(errors="replace") + p.stderr.decode(errors="replace")
    assert p.returncode == 0, out
    assert "Normal termination" in out
    return out


def run_case(root, case, kind, tag):
    folder = root / f"{case}_{kind}"
    shutil.copytree(os.path.join(CASES, case, kind, "Input"), folder)
    run_haz(folder, f"Run_{tag}_{kind}.txt")
    return folder


@pytest.fixture(scope="module")
def runs(tmp_path_factory):
    """HAZ runs of the Members and Cluster inputs of tests A and B."""
    root = tmp_path_factory.mktemp("cluster_hazard")
    out = {}
    for case, tag in (("TA_SingleRupture", "TA"), ("TB_NMFS", "TB")):
        for kind in ("Members", "Cluster"):
            out[tag, kind] = run_case(root, case, kind, tag)
    return out


def member_and_cluster(runs, tag, site):
    """Run-file periods, the Members out1 blocks and the Cluster out1 blocks."""
    periods = cr.read_run_file(os.path.join(runs[tag, "Cluster"], f"Run_{tag}_Cluster.txt"))
    members = cr.read_out1(os.path.join(runs[tag, "Members"], f"{tag}_Members_Site{site}.out1"))
    cluster = cr.read_out1(os.path.join(runs[tag, "Cluster"], f"{tag}_Cluster_Site{site}.out1"))
    return periods, members, cluster


def cluster_out1(blocks, iprob):
    """The cluster's curves and the members' blocks.

    The members' blocks hold zeros; the cluster's curves follow them as
    source nFlt + 1 (one segmentation model).
    """
    found = [b for b in blocks if b["iprob"] == iprob]
    assert [b["iflt"] for b in found] == MEMBERS + [len(MEMBERS) + 1]
    return found[-1]["curves"][:, 0, 0, :], [b["curves"] for b in found[:-1]]


def assert_close(actual, expected, rtol, floor):
    """Relative comparison of the values above floor; the others must be below it too."""
    actual = np.asarray(actual)
    expected = np.asarray(expected)
    big = expected > floor
    assert np.all(actual[~big] <= 2.0 * floor)
    np.testing.assert_allclose(actual[big], expected[big], rtol=rtol)


# ---------------------------------------------------------------------------
# Test A: one rupture per member, the spreadsheet formula with HAZ's medians
# ---------------------------------------------------------------------------


def test_ta_matches_spreadsheet_formula(runs):
    periods, members, cluster = member_and_cluster(runs, "TA", 1)
    levels = np.array(periods[0]["levels"])
    sigma = 0.503442
    p = cr.member_probabilities(members, MEMBERS, 1)[:, 0, :]

    # Each member has one rupture, so its P_k is a single lognormal exceedance.
    # Its median is the same at every level, and agrees with HAZ's attenuation
    # mode (tests/cluster/README.md).
    # Only levels with 0.01 < P_k < 0.99 are used: out1's six digits do not
    # resolve the median where P_k is close to 0 or 1.
    medians = []
    for pk in p:
        ok = (pk > 0.01) & (pk < 0.99)
        m = cr.implied_medians(levels[ok], pk[ok], sigma)
        assert len(m) >= 2
        np.testing.assert_allclose(m, np.median(m), rtol=1e-4)
        medians.append(np.median(m))
    np.testing.assert_allclose(medians, [0.01953, 0.02010, 0.01728], rtol=5e-4)

    # alpha * (1 - prod_k (1 - P_k)) with those medians and an untruncated
    # lognormal. Far in the tail a 1e-4 error in the median grows to about
    # 1e-3 in P_k, so the levels compared are those with P_c > 1e-6; the exact
    # comparison with the Members run is test_each_gmm_matches_reference.
    pk = np.array([cr.lognormal_exceedance(levels, m, sigma) for m in medians])
    expected = ALPHA * (1.0 - np.prod(1.0 - pk, axis=0))
    curve, others = cluster_out1(cluster, 1)
    big = expected > 1e-6 * ALPHA
    assert big.sum() >= 8
    np.testing.assert_allclose(curve[0][big], expected[big], rtol=2e-3)
    assert all(np.all(o == 0.0) for o in others)


# ---------------------------------------------------------------------------
# Test B: NMFS traces, boxcar magnitudes, two GMMs, two periods, two sites
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("tag, site", [("TA", 1), ("TB", 1), ("TB", 2)])
def test_each_gmm_matches_reference(runs, tag, site):
    """Cluster run against the reference from the Members run, on each GMM branch."""
    periods, members, cluster = member_and_cluster(runs, tag, site)
    for iprob, per in enumerate(periods, 1):
        weights = [w for _, w in per["gmms"][0]]
        ref = cr.cluster_hazard(cr.member_probabilities(members, MEMBERS, iprob), ALPHA, weights)
        curve, others = cluster_out1(cluster, iprob)
        for a in range(len(weights)):
            assert_close(curve[a], ref["rate_by_gmm"][a], RTOL_OUT1, 1e-14)
        assert all(np.all(o == 0.0) for o in others)


@pytest.mark.parametrize("site", [1, 2])
def test_tb_mean_and_member_shares(runs, site):
    periods, members, _ = member_and_cluster(runs, "TB", site)
    out3 = io_tools.read_out3(os.path.join(runs["TB", "Cluster"], f"TB_Cluster_Site{site}.out3"))
    for iprob, (per, model) in enumerate(zip(periods, out3["models"]), 1):
        weights = [w for _, w in per["gmms"][0]]
        ref = cr.cluster_hazard(cr.member_probabilities(members, MEMBERS, iprob), ALPHA, weights)
        total = np.array(model["rate"])
        assert_close(total, ref["rate"], RTOL_OUT3, 1e-14)
        # The members' allocated shares add up to the cluster hazard
        shares = np.sum([f["rate"] for f in model["faults"]], axis=0)
        assert_close(shares, total, RTOL_OUT3, 1e-14)


@pytest.mark.parametrize("site", [1, 2])
def test_tb_limits(runs, site):
    """The cluster tends to alpha at low levels and to the independent sum at high levels."""
    periods, members, cluster = member_and_cluster(runs, "TB", site)
    for iprob in range(1, len(periods) + 1):
        p = cr.member_probabilities(members, MEMBERS, iprob)
        curve, _ = cluster_out1(cluster, iprob)
        independent = ALPHA * p.sum(axis=0)
        for a in range(curve.shape[0]):
            ok = independent[a] > 1e-12
            ratio = curve[a][ok] / independent[a][ok]
            assert np.all(ratio <= 1.0 + RTOL_OUT1)
            assert np.all(ratio >= 1.0 / len(MEMBERS) - RTOL_OUT1)
            # Lowest level: every member is all but certain to exceed
            assert curve[a][0] == pytest.approx(ALPHA, rel=1e-3)
            # Highest level with hazard: the members' probabilities are small
            assert ratio[-1] == pytest.approx(1.0, abs=0.01)


# ---------------------------------------------------------------------------
# A cluster of one member is an ordinary source with the same rate
# ---------------------------------------------------------------------------


def one_member(text, source_type):
    """Test A's fault file cut down to its first member."""
    head, rest = text.split("TA_RFT_S\n", 1)
    head = head.replace("3                           Number of segments",
                        "1                           Number of segments", 1)
    head = head.replace("1 1 1                             flags", "1                                 flags", 1)
    return head.replace(SOURCE_8, source_type, 1)


def test_one_member_cluster_is_ordinary_source(tmp_path):
    src = os.path.join(CASES, "TA_SingleRupture", "Cluster", "Input")
    with open(os.path.join(src, "Source_TA_Cluster.txt")) as f:
        text = f.read()
    folders = {}
    for label, source_type in (("cluster", SOURCE_8), ("ordinary", SOURCE_1)):
        folder = tmp_path / label
        shutil.copytree(src, folder)
        with open(folder / "Source_TA_Cluster.txt", "w") as f:
            f.write(one_member(text, source_type))
        run_haz(folder, "Run_TA_Cluster.txt")
        folders[label] = folder

    # Allocation factor 1: the hazard and disaggregation outputs are identical
    for ext in ("out2", "out3", "out4", "out5", "out6", "out7"):
        name = f"TA_Cluster_Site1.{ext}"
        with open(folders["cluster"] / name) as f1, open(folders["ordinary"] / name) as f2:
            assert f1.read() == f2.read(), name

    # out1: the member's block is zero and the cluster's curve, alpha * P_c
    # (summed in a different order), follows it as source 2
    a = cr.read_out1(folders["cluster"] / "TA_Cluster_Site1.out1")
    b = cr.read_out1(folders["ordinary"] / "TA_Cluster_Site1.out1")
    assert [x["iflt"] for x in a] == [1, 2] and len(b) == 1
    assert np.all(a[0]["curves"] == 0.0)
    np.testing.assert_allclose(a[1]["curves"], b[0]["curves"], rtol=2e-6, atol=0.0)
