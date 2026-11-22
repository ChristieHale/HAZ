"""Tests of the cluster reference calculations (run with pytest from scripts/)."""

import os

import numpy as np
import pytest

import cluster_reference as cr

HERE = os.path.dirname(os.path.abspath(__file__))
CLUSTER = os.path.join(HERE, "..", "tests", "cluster")
TA = os.path.join(CLUSTER, "TA_SingleRupture", "Members")
TB = os.path.join(CLUSTER, "TB_NMFS", "Members")

# Mean Poisson in-cluster rate, HID Table H-5.5-2
ALPHA = 2.65702e-3


def test_cluster_probability():
    assert cr.cluster_probability([[0.3]]) == pytest.approx([0.3])
    assert cr.cluster_probability([[0.5], [0.5]]) == pytest.approx([0.75])
    assert cr.cluster_probability([[0.0], [0.0], [0.0]]) == pytest.approx([0.0])
    assert cr.cluster_probability([[1.0], [0.2]]) == pytest.approx([1.0])
    # Tiny probabilities are kept (1 - prod(1 - p) would round them to 0)
    assert cr.cluster_probability([[1e-20], [2e-20]]) == pytest.approx([3e-20])


def test_allocation_adds_up_to_cluster():
    rng = np.random.default_rng(1)
    p = rng.uniform(0.0, 1.0, size=(3, 5, 7)) ** 4
    p[:, 0, 0] = 0.0
    s = cr.allocation_factor(p)
    assert np.allclose(s * p.sum(axis=0), cr.cluster_probability(p))
    assert s[0, 0] == 1.0
    assert np.all(s <= 1.0) and np.all(s >= 1.0 / 3.0 - 1e-15)


# Values cached in New_Madrid_clustering.xlsx ("Correct" and "Incorrect" rows)
# at 0.001 to 1 g. Above 1 g Excel's NORM.DIST no longer resolves the tail.
EXCEL_CORRECT = [
    2.6570200000000004e-3, 2.6570200000000004e-3, 2.6570200000000004e-3, 2.657019999999418e-3,
    2.6570191861408346e-3, 2.6503612162294163e-3, 2.5085414792831622e-3, 1.4944384972909834e-3,
    1.5565608284966487e-4, 2.6386756005493147e-6, 1.0819244299486719e-7, 8.1287922819625055e-10,
    2.2087350793453812e-13,
]
EXCEL_INCORRECT = [
    7.9710599999718492e-3, 7.9710598527560489e-3, 7.9710504597556523e-3, 7.9702884929441602e-3,
    7.9036879826226581e-3, 6.8063358200591492e-3, 4.8394937882739325e-3, 1.8938085600200226e-3,
    1.5839880755540351e-4, 2.6393539000213513e-6, 1.0819349630993091e-7, 8.1287928158916521e-10,
    2.2087350793453812e-13,
]


def test_spreadsheet():
    z, p, alpha, rate, indep = cr.spreadsheet_table()
    assert alpha == pytest.approx(ALPHA, rel=1e-12)
    n = len(EXCEL_CORRECT)
    assert rate[:n] == pytest.approx(EXCEL_CORRECT, rel=1e-9)
    assert indep[:n] == pytest.approx(EXCEL_INCORRECT, rel=1e-9)


# Medians from HAZ attenuation mode (run option 1) for jcalc 102 at the
# member distances of test A (Rrup 351.00, 327.53, 326.65 km); printed to 4 digits.
TA_MEDIANS = [0.1953e-1, 0.2010e-1, 0.1728e-1]
TA_SIGMA = 0.503442


def test_ta_members_single_lognormal():
    """Each test A member is one rupture: P_k is one lognormal exceedance."""
    periods = cr.read_run_file(os.path.join(TA, "Input", "Run_TA_Members.txt"))
    out1 = cr.read_out1(os.path.join(TA, "Output", "TA_Members_Site1.out1"))
    p = cr.member_probabilities(out1, [1, 2, 3], 1)[:, 0, :]
    for k, median in enumerate(TA_MEDIANS):
        implied = cr.implied_medians(periods[0]["levels"], p[k], TA_SIGMA)
        ok = (p[k] > 1e-3) & (p[k] < 1.0 - 1e-3)
        assert ok.sum() >= 3
        # One median at every level (out1 has 6 significant digits)
        assert implied[ok] == pytest.approx(np.full(ok.sum(), implied[ok].mean()), rel=2e-4)
        # ... and it is HAZ's median for that magnitude and distance
        assert implied[ok].mean() == pytest.approx(median, rel=5e-4)


def test_tb_members_reference():
    periods = cr.read_run_file(os.path.join(TB, "Input", "Run_TB_Members.txt"))
    assert [p["specT"] for p in periods] == [0.0, 1.0]
    assert [w for _, w in periods[0]["gmms"][0]] == [0.5, 0.5]
    for site in (1, 2):
        out1 = cr.read_out1(os.path.join(TB, "Output", f"TB_Members_Site{site}.out1"))
        for iprob, per in enumerate(periods, 1):
            p = cr.member_probabilities(out1, [1, 2, 3], iprob)
            assert p.shape == (3, 2, len(per["levels"]))
            assert np.all((p >= 0.0) & (p <= 1.0))
            assert np.all(np.diff(p, axis=-1) <= 0.0)
            res = cr.cluster_hazard(p, ALPHA, [w for _, w in per["gmms"][0]])
            rate, indep = res["rate"], res["rate_independent"]
            assert np.all(rate <= ALPHA * (1 + 1e-12))
            assert np.all(rate <= indep * (1 + 1e-12))
            # The cluster matters most where the members' probabilities are large
            big = res["p_members"].max(axis=(0, 1)) > 0.5
            if big.any():
                assert np.all(indep[big] > 1.2 * rate[big])


def test_read_out4_all_periods():
    out4 = cr.read_out4(os.path.join(TB, "Output", "TB_Members_Site1.out4"))
    n_m = len(out4["edges"]["mag"]) - 1
    n_r = len(out4["edges"]["dist"]) - 1
    n_e = len(out4["edges"]["eps"]) - 1
    assert len(out4["periods"]) == 2
    for per in out4["periods"]:
        assert len(per["mr"]) == n_m * n_r
        assert len(per["mre"]) == n_m * n_r * n_e
        has_hazard = np.nan_to_num(np.array(per["m_bar"])) > 0.0
        total = np.sum([r[6] for r in per["mr"]], axis=0)
        assert total[has_hazard] == pytest.approx(np.ones(has_hazard.sum()), abs=5e-3)
