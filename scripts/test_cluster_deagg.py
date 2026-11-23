"""Tests of the disaggregation of clustered sequences (source type 8).

Proportional allocation (docs/Cluster_Source_Spec.md, A5): on each GMM
branch g and at each level z, every rupture of every member keeps its usual
relative contribution, and all of the cluster's contributions are scaled by
s_g(z) = P_c / sum_k P_k. These tests run HAZ on the Cluster inputs and on
Members runs with a single GMM each, and compare the Cluster run's out3
member shares, out4 bins and means, and out7 means with the reference
calculation in cluster_reference.py. Set HAZ_BIN to the executable; the tests
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
CASES = os.path.join(HERE, "..", "tests", "cluster")

HAZ_BIN = os.environ.get("HAZ_BIN", os.path.join(HERE, "..", "build", "HAZ"))
pytestmark = pytest.mark.skipif(not os.path.isfile(HAZ_BIN), reason="set HAZ_BIN to the HAZ executable")

ALPHA = 2.65702e-3
MEMBERS = [1, 2, 3]

# out3 has 4 significant digits, out4 3 and out7 2 decimals. The reference
# values are built from these printed numbers too (see assert_printed_close).
RTOL_OUT3 = 2e-3
ATOL_OUT7 = 0.015


def last_digit(values, digits):
    """One unit in the last printed digit of values written with `digits` significant digits."""
    v = np.abs(np.asarray(values, dtype=float))
    with np.errstate(divide="ignore"):
        return np.where(v > 0.0, 10.0 ** (np.floor(np.log10(np.where(v > 0.0, v, 1.0))) + 1 - digits), 0.0)


def assert_printed_close(got, exp, inputs, digits, err_msg=""):
    """got agrees with exp to within the rounding of the printed numbers.

    got is printed with `digits` significant digits (half a unit of rounding).
    exp is a weighted average of the printed numbers in `inputs` (one array
    per run, same shape as exp), so its error is at most half a unit in the
    last digit of the coarsest of them.
    """
    got = np.asarray(got, dtype=float)
    exp = np.asarray(exp, dtype=float)
    input_unit = np.max([last_digit(x, digits) for x in inputs], axis=0)
    tol = 0.5 * (last_digit(got, digits) + input_unit) * 1.001 + 1e-30
    bad = np.abs(got - exp) > tol
    assert not bad.any(), f"{err_msg}: {bad.sum()} values differ by more than one unit in the last digit, " \
        f"e.g. {got[bad][:3]} vs {exp[bad][:3]}"


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


def single_gmm(text, keep):
    """A run file with only GMM number `keep` (1-based), at weight 1, for every period."""
    lines = text.splitlines(keepends=True)
    out = []
    i = 0
    while i < len(lines):
        line = lines[i]
        if "Number of GM Models" in line:
            n = int(line.split()[0])
            model = lines[i + keep].split()
            model[3] = "1.0"
            out.append(line.replace(str(n), "1", 1))
            out.append("  ".join(model[:6]) + "\n")
            i += n + 1
        else:
            out.append(line)
            i += 1
    return "".join(out)


@pytest.fixture(scope="module")
def runs(tmp_path_factory):
    """Cluster runs of tests A and B, the Members run of test A (one GMM),
    and Members runs of test B with each GMM on its own."""
    root = tmp_path_factory.mktemp("cluster_deagg")
    out = {}
    for case, tag in (("TA_SingleRupture", "TA"), ("TB_NMFS", "TB")):
        folder = root / f"{tag}_Cluster"
        shutil.copytree(os.path.join(CASES, case, "Cluster", "Input"), folder)
        run_haz(folder, f"Run_{tag}_Cluster.txt")
        out[tag, "Cluster"] = folder

    folder = root / "TA_Members"
    shutil.copytree(os.path.join(CASES, "TA_SingleRupture", "Members", "Input"), folder)
    run_haz(folder, "Run_TA_Members.txt")
    out["TA", "Members", 1] = folder

    src = os.path.join(CASES, "TB_NMFS", "Members", "Input")
    with open(os.path.join(src, "Run_TB_Members.txt")) as f:
        text = f.read()
    for g in (1, 2):
        folder = root / f"TB_Members_GMM{g}"
        shutil.copytree(src, folder)
        with open(folder / "Run_TB_Members.txt", "w") as f:
            f.write(single_gmm(text, g))
        run_haz(folder, "Run_TB_Members.txt")
        out["TB", "Members", g] = folder
    return out


CASES_SITES = [("TA", 1, [1.0]), ("TB", 1, [0.5, 0.5]), ("TB", 2, [0.5, 0.5])]


def load(runs, tag, site, weights):
    """Single-GMM member runs (out1, out4, out7) and the Cluster run's outputs."""
    members = []
    for g in range(1, len(weights) + 1):
        folder = runs[tag, "Members", g]
        base = os.path.join(folder, f"{tag}_Members_Site{site}")
        members.append((cr.read_out1(base + ".out1"), cr.read_out4(base + ".out4"), cr.read_out7(base + ".out7")))
    base = os.path.join(runs[tag, "Cluster"], f"{tag}_Cluster_Site{site}")
    cluster = dict(out3=io_tools.read_out3(base + ".out3"), out4=cr.read_out4(base + ".out4"),
                   out7=cr.read_out7(base + ".out7"))
    return members, cluster


def test_single_gmm_run_file():
    with open(os.path.join(CASES, "TB_NMFS", "Members", "Input", "Run_TB_Members.txt")) as f:
        text = f.read()
    one = single_gmm(text, 2)
    assert one.count("Number of GM Models") == 2
    assert "102  0.0" not in one
    assert one.count("120  0.0  0.0  1.0  0.0  0") == 2


def test_single_gmm_runs_have_one_gmm(runs):
    for g in (1, 2):
        blocks = cr.read_out1(os.path.join(runs["TB", "Members", g], "TB_Members_Site1.out1"))
        assert all(b["curves"].shape[0] == 1 for b in blocks)


@pytest.mark.parametrize("tag, site, weights", CASES_SITES)
def test_out3_member_shares(runs, tag, site, weights):
    """Member k's out3 line is sum_g w_g alpha s_g P_kg."""
    members, cluster = load(runs, tag, site, weights)
    for iprob, model in enumerate(cluster["out3"]["models"], 1):
        p = np.array([cr.member_probabilities(m[0], MEMBERS, iprob)[:, 0, :] for m in members])  # (G, K, nInten)
        s = cr.allocation_factor(np.moveaxis(p, 1, 0))  # (G, nInten)
        expected = ALPHA * np.einsum("g,gn,gkn->kn", weights, s, p)
        for k, fault in enumerate(model["faults"]):
            got = np.array(fault["rate"])
            big = expected[k] > 1e-14
            np.testing.assert_allclose(got[big], expected[k][big], rtol=RTOL_OUT3)
            assert np.all(got[~big] < 1e-13)


@pytest.mark.parametrize("tag, site, weights", CASES_SITES)
def test_out4_bins_and_means(runs, tag, site, weights):
    members, cluster = load(runs, tag, site, weights)
    gmm_runs = [(m[0], m[1]) for m in members]
    for iprob, per in enumerate(cluster["out4"]["periods"], 1):
        ref = cr.cluster_deagg(gmm_runs, ALPHA, weights, MEMBERS, iprob)
        ok = np.isfinite(np.array(per["m_bar"], dtype=float)) & (ref["rate"] > 1e-14)
        assert ok.sum() >= 5
        for key in ("mr", "mre"):
            got = np.array([r[6] for r in per[key]])[:, ok]
            exp = ref[key][:, ok]
            inputs = [np.array([r[6] for r in run[1]["periods"][iprob - 1][key]])[:, ok] for run in gmm_runs]
            assert_printed_close(got, exp, inputs, 3, key)
        for key in ("m_bar", "d_bar", "eps_bar"):
            got = np.array(per[key], dtype=float)[ok]
            inputs = [np.array(run[1]["periods"][iprob - 1][key], dtype=float)[ok] for run in gmm_runs]
            assert_printed_close(got, ref[key][ok], inputs, 3, key)


@pytest.mark.parametrize("tag, site, weights", CASES_SITES)
def test_out7_member_means(runs, tag, site, weights):
    members, cluster = load(runs, tag, site, weights)
    gmm_runs = [(m[0], m[2]) for m in members]
    for iprob, per in enumerate(cluster["out7"], 1):
        ref = cr.cluster_source_deagg(gmm_runs, weights, MEMBERS, iprob)
        assert [name for name, _ in per["sources"]] == [name for name, _ in members[0][2][iprob - 1]["sources"]]
        for k, (name, got) in enumerate(per["sources"]):
            assert set(got) == set(ref[k])
            for label, values in got.items():
                ok = np.isfinite(values) & np.isfinite(ref[k][label])
                assert ok.sum() >= 5, (name, label)
                np.testing.assert_allclose(values[ok], ref[k][label][ok], rtol=1e-3, atol=ATOL_OUT7,
                                           err_msg=f"{name} {label}")


def test_ta_single_gmm_same_as_members(runs):
    """With one GMM, s(z) is one number per level, so the cluster's out4
    fractions and means, and each member's out7 means, are those of the
    Members run (tests/cluster/README.md)."""
    members, cluster = load(runs, "TA", 1, [1.0])
    m4 = members[0][1]["periods"][0]
    c4 = cluster["out4"]["periods"][0]
    ok = np.isfinite(np.array(c4["m_bar"], dtype=float))
    for a, b in zip(c4["mre"], m4["mre"]):
        assert_printed_close(a[6][ok], b[6][ok], [b[6][ok]], 3, "mre")
    for key in ("m_bar", "d_bar", "eps_bar"):
        exp = np.array(m4[key], dtype=float)[ok]
        assert_printed_close(np.array(c4[key], dtype=float)[ok], exp, [exp], 3, key)
    for (_, got), (_, exp) in zip(cluster["out7"][0]["sources"], members[0][2][0]["sources"]):
        for label in got:
            ok = np.isfinite(got[label]) & np.isfinite(exp[label])
            np.testing.assert_allclose(got[label][ok], exp[label][ok], atol=0.011)
