#!/usr/bin/env python3
"""Reference calculations for clustered earthquake sequences (source type 8).

A cluster is a sequence of K earthquakes on K member faults that occur
together with one rate, alpha. Following USGS OFR 2008-1128, Appendix F,
on each epistemic branch b and at each test level z:

    P_c(z | b)  = 1 - prod_k [1 - P_k(z | b)]
    nu_c(z | b) = alpha_b * P_c(z | b)

where P_k(z | b) is the probability that member k's earthquake exceeds z given
that the sequence occurs. The mean hazard is the weighted sum over branches.

Disaggregation uses proportional allocation: every member rupture keeps its
usual relative contribution, and all of the cluster's contributions on a
branch are scaled by s(z | b) = P_c / sum_k P_k, so they add up to nu_c.

This module does not use any of the HAZ Fortran. Member probabilities come
from HAZ runs in which each member is an ordinary source with an activity rate
of 1, so that its hazard equals P_k (see tests/cluster/README.md). The
spreadsheet command reproduces New_Madrid_clustering.xlsx.

See docs/Cluster_Source_Spec.md.
"""

import argparse
import math
import re
import sys

import numpy as np

# ---------------------------------------------------------------------------
# Cluster formulas
# ---------------------------------------------------------------------------


def cluster_probability(p_members):
    """Probability that at least one member exceeds: 1 - prod_k (1 - P_k).

    p_members has the members on axis 0.
    """
    p = np.asarray(p_members, dtype=float)
    # Same as 1 - prod(1 - p), without losing the small probabilities to
    # round-off when every P_k is below about 1e-16.
    with np.errstate(divide="ignore"):
        return 0.0 - np.expm1(np.sum(np.log1p(-np.minimum(p, 1.0)), axis=0))


def allocation_factor(p_members):
    """Proportional allocation factor s = P_c / sum_k P_k (1 where the sum is 0)."""
    p = np.asarray(p_members, dtype=float)
    total = p.sum(axis=0)
    pc = cluster_probability(p)
    with np.errstate(divide="ignore", invalid="ignore"):
        s = np.where(total > 0.0, pc / np.where(total > 0.0, total, 1.0), 1.0)
    return s


def lognormal_exceedance(z, median, sigma):
    """P(Y > z) for a lognormal Y without truncation."""
    z = np.asarray(z, dtype=float)
    x = (np.log(z) - math.log(median)) / sigma
    return np.array([0.5 * math.erfc(v / math.sqrt(2.0)) for v in np.ravel(x)]).reshape(x.shape)


# ---------------------------------------------------------------------------
# Concept spreadsheet (New_Madrid_clustering.xlsx)
# ---------------------------------------------------------------------------

# Inputs of the spreadsheet: NMS, RFT and NMN at R = 350 km, one GMM with a
# lognormal sigma, and the Poisson in-cluster rates of HID Table H-5.5-2.
SPREADSHEET = dict(
    names=["NMS", "RFT", "NMN"],
    mags=[7.9, 7.8, 7.6],
    medians=[3.9254483228487382e-2, 3.5789723896412481e-2, 2.9289929033250225e-2],
    sigma=0.503442,
    levels=[1e-3, 2e-3, 3e-3, 5e-3, 0.01, 0.02, 0.03, 0.05, 0.1, 0.2, 0.3, 0.5, 1.0, 2.0, 3.0, 5.0],
    rates=[6.0e-3, 3.7e-3, 2.4e-3, 1.4e-3, 6.2e-4],
    rate_weights=[0.101, 0.244, 0.310, 0.244, 0.101],
)


def spreadsheet_table(inputs=SPREADSHEET):
    """Return (levels, P_k, alpha, cluster rate, independent-sum rate)."""
    z = np.array(inputs["levels"])
    p = np.array([lognormal_exceedance(z, m, inputs["sigma"]) for m in inputs["medians"]])
    alpha = float(np.dot(inputs["rates"], inputs["rate_weights"]))
    return z, p, alpha, alpha * cluster_probability(p), alpha * p.sum(axis=0)


# ---------------------------------------------------------------------------
# HAZ file readers
# ---------------------------------------------------------------------------


class ListDirected:
    """Minimal emulation of Fortran list-directed reads of a text file.

    Each read starts on a new record and continues onto following records
    until enough values are found; the rest of the last record is skipped.
    """

    def __init__(self, path):
        with open(path) as f:
            self.lines = f.read().splitlines()
        self.i = 0

    def record(self):
        line = self.lines[self.i]
        self.i += 1
        return line

    def values(self, n):
        out = []
        while len(out) < n:
            tokens = self.record().replace(",", " ").split("/")[0].split()
            out.extend(tokens[: n - len(out)])
        return out

    def backspace(self):
        self.i -= 1


def read_run_file(path):
    """Read the periods, test levels and GMM weights from a HAZ run file.

    Returns a list (one entry per period) of dicts with keys specT, sigtrunc,
    levels and gmms; gmms is a list (one per attenuation type) of lists of
    (jcalc, weight).
    """
    rd = ListDirected(path)
    rd.record()  # fault file name
    rd.values(1)  # fault file version
    rd.values(4)  # grid limits
    rd.record()  # max distance(s)
    rd.record()  # title
    n_prob, n_atten_type = (int(v) for v in rd.values(2))
    periods = []
    for _ in range(n_prob):
        spec_t, sig_trunc, _, _ = (float(v) for v in rd.values(4))
        n_inten = int(rd.values(1)[0])
        rd.backspace()
        levels = [float(v) for v in rd.values(n_inten + 1)[1:]]
        gmms = []
        for _ in range(n_atten_type):
            n_atten = int(rd.values(1)[0])
            models = []
            for _ in range(n_atten):
                v = rd.values(6)
                jcalc = int(v[0])
                if 10000 < abs(jcalc) < 12000:
                    raise NotImplementedError("common functional form GMMs are not supported")
                models.append((jcalc, float(v[3])))
            gmms.append(models)
        periods.append(dict(specT=spec_t, sigtrunc=sig_trunc, levels=levels, gmms=gmms))
    return periods


def read_out1(path):
    """Read a HAZ out1 file (unweighted branch hazard curves).

    Returns a list of blocks in file order. Each block is a dict with keys
    iflt, iwidth, iprob (1-based, as written by HAZ) and curves, an array of
    shape (nAtten, nFtype, nParamVar, nInten).
    """
    with open(path) as f:
        tokens = f.read().split("\n", 1)[1].split()
    blocks = []
    i = 0
    while i < len(tokens):
        iflt, iwidth, iprob, n_atten, _, n_ftype, n_param, n_inten = (int(t) for t in tokens[i : i + 8])
        i += 8
        n = n_atten * n_ftype * n_param * n_inten
        curves = np.array([float(t) for t in tokens[i : i + n]]).reshape(n_atten, n_ftype, n_param, n_inten)
        i += n
        blocks.append(dict(iflt=iflt, iwidth=iwidth, iprob=iprob, curves=curves))
    return blocks


def _floats(text):
    return [float(t) for t in text.split()]


def read_out4(path):
    """Read every period of a HAZ out4 file.

    Returns a dict with the bin edges and a list of periods, each with the
    test levels, M_bar, D_bar, Eps_bar, and the bins: 'mr' (magnitude-distance,
    summed over epsilon) and 'mre' (magnitude-distance-epsilon). Each bin row
    is (eps_lo, eps_hi, m_lo, m_hi, r_lo, r_hi, fractions[nInten]).
    """
    with open(path) as f:
        lines = f.read().splitlines()
    edges = {}
    for key, label in (("mag", "Magnitude"), ("dist", "Distance"), ("eps", "Epsilon")):
        for j, line in enumerate(lines):
            if f"Number of {label}" in line:
                edges[key] = _floats(lines[j + 1])
                break
    n_mr = (len(edges["mag"]) - 1) * (len(edges["dist"]) - 1)
    row = re.compile(r"^\s+-?\d+\.\d{3}\s+-?\d+\.\d{3}\s+\d+\.\d{2}\s+\d+\.\d{2}\s+\d+\.\d\s+\d+\.\d\s")
    periods = []
    current = None
    for line in lines:
        if line.startswith(" AMP:"):
            current = dict(levels=_floats(line[5:]), rows=[])
            periods.append(current)
        elif current is None:
            continue
        elif line.startswith("  M_bar"):
            current["m_bar"] = _floats(line[7:])
        elif line.startswith("  D_bar"):
            current["d_bar"] = _floats(line[7:])
        elif line.startswith("  Eps_bar"):
            current["eps_bar"] = _floats(line[9:])
        elif row.match(line):
            v = _floats(line)
            current["rows"].append((v[0], v[1], v[2], v[3], v[4], v[5], np.array(v[6:])))
    for p in periods:
        rows = p.pop("rows")
        p["mr"], p["mre"] = rows[:n_mr], rows[n_mr:]
    return dict(edges=edges, periods=periods)


# ---------------------------------------------------------------------------
# Cluster hazard from member runs
# ---------------------------------------------------------------------------


def member_probabilities(out1_blocks, members, iprob):
    """P_k for each member, GMM and level: array (K, nAtten, nInten).

    The member runs must have a single SSC branch (nWidth = nFtype =
    nParamVar = 1) and an activity rate of 1, so the out1 curves are P_k.
    """
    p = []
    for iflt in members:
        found = [b for b in out1_blocks if b["iflt"] == iflt and b["iprob"] == iprob]
        if len(found) != 1:
            raise ValueError(f"expected one out1 block for source {iflt}, period {iprob}")
        c = found[0]["curves"]
        if c.shape[1:3] != (1, 1):
            raise ValueError("member runs must have a single SSC branch")
        p.append(c[:, 0, 0, :])
    return np.array(p)


def cluster_hazard(p_members, alpha, gm_weights):
    """Cluster hazard from member probabilities P_k (K, nAtten, nInten).

    Returns a dict with per-GMM P_c, s and cluster rates (nAtten, nInten),
    and the GMM-weighted mean cluster rate and independent-sum rate (nInten).
    """
    w = np.asarray(gm_weights, dtype=float)
    pc = cluster_probability(p_members)
    s = allocation_factor(p_members)
    return dict(
        p_members=p_members,
        p_cluster=pc,
        s=s,
        rate_by_gmm=alpha * pc,
        rate=alpha * np.dot(w, pc),
        rate_independent=alpha * np.dot(w, p_members.sum(axis=0)),
    )


def cluster_deagg(gmm_runs, alpha, gm_weights, members, iprob):
    """Allocated cluster disaggregation from one member run per GMM.

    gmm_runs is a list (one per GMM, same order as gm_weights) of
    (out1_blocks, out4) from runs with a single GMM in which the members are
    ordinary sources with rate 1. On GMM branch g the cluster's binned
    contribution is alpha * s_g(z) * sum_k B_kg(z), and sum_k B_kg is the
    out4 fraction times that run's total hazard, sum_k P_kg.

    Returns the cluster rate and, as fractions of it, the M-R and M-R-eps
    bins, plus M_bar, D_bar and Eps_bar.
    """
    rate = 0.0
    mr = mre = None
    bars = dict(m_bar=0.0, d_bar=0.0, eps_bar=0.0)
    for (out1, out4), w in zip(gmm_runs, gm_weights):
        p = member_probabilities(out1, members, iprob)[:, 0, :]
        total = p.sum(axis=0)
        weight = w * alpha * allocation_factor(p) * total  # weighted cluster rate on this branch
        per = out4["periods"][iprob - 1]
        rate = rate + weight
        frac_mr = np.array([r[6] for r in per["mr"]])
        frac_mre = np.array([r[6] for r in per["mre"]])
        mr = weight * frac_mr if mr is None else mr + weight * frac_mr
        mre = weight * frac_mre if mre is None else mre + weight * frac_mre
        for key in bars:
            bars[key] = bars[key] + weight * np.nan_to_num(np.array(per[key]))
    with np.errstate(divide="ignore", invalid="ignore"):
        out = dict(rate=rate, mr=mr / rate, mre=mre / rate)
        out.update({k: v / rate for k, v in bars.items()})
    return out


def implied_medians(levels, p, sigma):
    """Median implied at each level by P(Y > z) for a single lognormal."""
    from statistics import NormalDist

    nd = NormalDist()
    out = []
    for z, pz in zip(levels, p):
        if 1e-6 < pz < 1.0 - 1e-6:
            out.append(z / math.exp(sigma * nd.inv_cdf(1.0 - pz)))
        else:
            out.append(float("nan"))
    return np.array(out)


# ---------------------------------------------------------------------------
# Command line
# ---------------------------------------------------------------------------


def _print_table(levels, columns, names):
    print("  " + "z (g)".rjust(10) + "".join(n.rjust(13) for n in names))
    for i, z in enumerate(levels):
        print("  " + f"{z:10.4g}" + "".join(f"{c[i]:13.5e}" for c in columns))


def cmd_spreadsheet(args):
    z, p, alpha, rate, indep = spreadsheet_table()
    print(f"New_Madrid_clustering.xlsx inputs: alpha = {alpha:.6e} /yr")
    names = SPREADSHEET["names"]
    _print_table(z, list(p) + [rate, indep], [f"P_{n}" for n in names] + ["cluster", "independent"])


def cmd_members(args):
    periods = read_run_file(args.run)
    blocks = read_out1(args.out1)
    members = args.members or sorted({b["iflt"] for b in blocks})
    for iprob, per in enumerate(periods, 1):
        gmms = per["gmms"][args.atten_type - 1]
        weights = [w for _, w in gmms]
        p = member_probabilities(blocks, members, iprob)
        res = cluster_hazard(p, args.alpha, weights)
        print(f"\nPeriod {per['specT']:g} s, alpha = {args.alpha:.6e} /yr, members {members}")
        if args.by_gmm:
            for a, (jcalc, w) in enumerate(gmms):
                print(f"\n  GMM {a + 1}: jcalc {jcalc}, weight {w:g}")
                cols = [p[k, a] for k in range(len(members))] + [res["p_cluster"][a], res["s"][a], res["rate_by_gmm"][a]]
                _print_table(per["levels"], cols, [f"P_{m}" for m in members] + ["P_c", "s", "cluster"])
        print("\n  GMM-weighted mean")
        _print_table(per["levels"], [res["rate"], res["rate_independent"]], ["cluster", "independent"])


def cmd_deagg(args):
    periods = read_run_file(args.run)
    weights = args.weights
    runs = [(read_out1(o1), read_out4(o4)) for o1, o4 in zip(args.out1, args.out4)]
    if not (len(runs) == len(weights)):
        sys.exit("give one out1, one out4 and one weight per GMM run")
    members = args.members or sorted({b["iflt"] for b in runs[0][0]})
    for iprob, per in enumerate(periods, 1):
        d = cluster_deagg(runs, args.alpha, weights, members, iprob)
        print(f"\nPeriod {per['specT']:g} s, alpha = {args.alpha:.6e} /yr, members {members}")
        _print_table(per["levels"], [d["rate"], d["m_bar"], d["d_bar"], d["eps_bar"]], ["cluster", "M_bar", "D_bar", "Eps_bar"])


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    sub = parser.add_subparsers(dest="command", required=True)

    sub.add_parser("spreadsheet", help="reproduce New_Madrid_clustering.xlsx")

    p = sub.add_parser("members", help="cluster hazard from a run with the members as ordinary sources (rate 1)")
    p.add_argument("run", help="HAZ run file of the member run")
    p.add_argument("out1", help="out1 file of the member run")
    p.add_argument("--alpha", type=float, required=True, help="rate of the sequence (/yr)")
    p.add_argument("--members", type=int, nargs="+", help="source numbers of the members (default: all)")
    p.add_argument("--atten-type", type=int, default=1)
    p.add_argument("--by-gmm", action="store_true", help="also print each GMM branch")

    p = sub.add_parser("deagg", help="allocated cluster disaggregation from one member run per GMM")
    p.add_argument("run", help="HAZ run file (for the periods and levels)")
    p.add_argument("--out1", nargs="+", required=True, help="out1 file of each single-GMM member run")
    p.add_argument("--out4", nargs="+", required=True, help="out4 file of each single-GMM member run")
    p.add_argument("--weights", type=float, nargs="+", required=True, help="weight of each GMM")
    p.add_argument("--alpha", type=float, required=True)
    p.add_argument("--members", type=int, nargs="+")

    args = parser.parse_args(argv)
    dict(spreadsheet=cmd_spreadsheet, members=cmd_members, deagg=cmd_deagg)[args.command](args)


if __name__ == "__main__":
    main()
