#!/usr/bin/env python3
"""Write the CEUS SSC New Madrid Fault System (NMFS) RLME source for HAZ 45.4.

The NMFS is one fault system with eight segments and eleven segmentation
models, following the logic tree of HID Figure H-5.5-1 as corrected on June 27,
2012 (CEUS SSC Category 1 update):

- In cluster (0.9): the six members of the clustered sequence (source type 8),
  two alternative geometries of each of NMS, RFT and NMN. Each segmentation
  model picks one geometry of each fault; there are 2 x 2 x 2 = 8 models,
  weighted 0.9 x the product of the geometry weights.
- Out of cluster, RFT only (0.05): RFT-S or RFT-L as an ordinary segment
  (source type 1), weighted 0.05 x 0.7 and 0.05 x 0.3.
- Out of cluster, no activity (0.05): a model with no segment.

Every member has the same branches, matched by index (docs/Cluster_Source_Spec.md,
A8): thickness 13 / 15 / 17 km (0.3 / 0.5 / 0.2), the rate of the sequence
(Poisson 0.75 with Table H-5.5-2; BPT 0.25 with alpha 0.3 / 0.5 / 0.7 at
0.2 / 0.5 / 0.3 and Tables H-5.5-3 to H-5.5-5) times the six rupture sets of
Table H-5.5-1 (each member's own magnitude), and one mechanism. Magnitudes have
the +/-0.25 uniform variability of HID Section H.5.5.4 (boxcar, model 8).

The traces, the rupture sets and the rate tables are read from NMFS_RLME.xls
(CEUS SSC HID database). The rupture-scaling relations and step sizes follow
the other RLME faults of the rev23 CEUS model: PEER Report Eq. 13-8 for rupture
area and Eq. 13-9 for rupture width (strike-slip or reverse coefficients),
1 km steps along strike and down dip, maximum distance type 1.

Usage:
    python make_nmfs_source.py NMFS_RLME.xls OUT_DIR [--run RUN_FILE]

writes OUT_DIR/NMFS_45.4.txt and, with --run, one run file per site of
RUN_FILE (a HAZ run file, such as the rev23 CEUS one) with the fault file
replaced by the NMFS source and the version by 45.4.
"""

import argparse
import os
import re

import xlrd

# ---------------------------------------------------------------------------
# HID Figure H-5.5-1 (corrected June 27, 2012)
# ---------------------------------------------------------------------------

W_IN, W_OUT_RFT, W_OUT_NONE = 0.9, 0.05, 0.05
# Source geometry: (sheet in NMFS_RLME.xls, weight)
GEOMETRY = {
    "NMS": [("BA-BL", 0.6), ("BA-BFZ", 0.4)],
    "RFT": [("RFT_S", 0.7), ("RFT_L", 0.3)],
    "NMN": [("NMN_S", 0.7), ("NMN_L", 0.3)],
}
THICKNESS = [(13.0, 0.3), (15.0, 0.5), (17.0, 0.2)]
# Recurrence model, in cluster: Poisson 0.75 (Table H-5.5-2); renewal (BPT) 0.25
# with alpha 0.3 / 0.5 / 0.7 at 0.2 / 0.5 / 0.3 (Tables H-5.5-3 to H-5.5-5)
RATE_TABLES_IN = [("Table H5.5-2", 0.75), ("Table H5.5-3", 0.25 * 0.2),
                  ("Table H5.5-4", 0.25 * 0.5), ("Table H5.5-5", 0.25 * 0.3)]
RATE_TABLE_OUT = "Table H5.5-6"     # out of cluster, Poisson
# Style of faulting (HID H.5.5.3): NMS and NMN vertical strike-slip, RFT
# reverse dipping 40 degrees southwest (the RFT traces run southeast, so a
# positive dip is to the southwest)
DIP = {"NMS": 90.0, "RFT": 40.0, "NMN": 90.0}
FTYPE = {"NMS": 0.0, "RFT": 1.0, "NMN": 0.0}
# Rupture width, PEER Report Eq. 13-9 (as in the rev23 CEUS RLME faults)
WIDTH_COEF = {0.0: "-777 -4.254 0.785 0.0         rupWidth coeff: flag, a, b, sigma (PEER Report Eq. 13-9, SS)",
              1.0: "-777 -1.931 0.349 0.0         rupWidth coeff: flag, a, b, sigma (PEER Report Eq. 13-9, RV)"}
AREA_COEF = "-4.25  1.0  0.0              rupArea coeff: a, b, sigma (PEER Report Eq. 13-8)"
FAULTS = ["NMS", "RFT", "NMN"]


# ---------------------------------------------------------------------------
# NMFS_RLME.xls
# ---------------------------------------------------------------------------


def read_workbook(path):
    wb = xlrd.open_workbook(path)
    traces = {}
    for fault in FAULTS:
        for sheet, _ in GEOMETRY[fault]:
            sh = wb.sheet_by_name(sheet)
            pts = []
            for r in range(2, sh.nrows):
                lon, lat = sh.cell_value(r, 0), sh.cell_value(r, 1)
                if lon == "" or lat == "":
                    continue
                pts.append((float(lon), float(lat)))
            # The sheet gives the number of points in its second row. Every
            # listed point is used; a different count is reported (sheet
            # BA-BL gives 3 but lists 4, the 4th being the start of NMN).
            n_listed = int(re.match(r"\s*(\d+)", str(sh.cell_value(1, 0))).group(1))
            if len(pts) != n_listed:
                print(f"warning: sheet {sheet} gives {n_listed} points but lists {len(pts)}; using all {len(pts)}")
            traces[sheet] = pts

    sh = wb.sheet_by_name("Table H.5.5-1")
    sets = []
    for r in range(sh.nrows):
        row = sh.row_values(r)
        if isinstance(row[0], float) and len(row) >= 5 and isinstance(row[4], float):
            sets.append(dict(NMS=row[1], RFT=row[2], NMN=row[3], weight=row[4]))
    assert len(sets) == 6, sets

    def rates(name):
        sh = wb.sheet_by_name(name)
        out = []
        for r in range(sh.nrows):
            row = sh.row_values(r)
            if len(row) >= 2 and isinstance(row[0], float) and isinstance(row[1], float):
                out.append((row[0], row[1]))
        assert len(out) == 5 and abs(sum(w for _, w in out) - 1.0) < 1e-6, (name, out)
        return out

    rates_in = []
    for name, w_table in RATE_TABLES_IN:
        rates_in += [(rate, w_table * w) for rate, w in rates(name)]
    return traces, sets, rates_in, rates(RATE_TABLE_OUT)


# ---------------------------------------------------------------------------
# HAZ 45.4 fault file
# ---------------------------------------------------------------------------


def fmt(values, spec="g"):
    return "  ".join(format(v, spec) for v in values)


def segment(name, fault, trace, rates, mags, mag_weights, member):
    source_type = "8" if member else "1"
    role = "source type (8 = cluster member)" if member else "source type (out of cluster, ordinary segment)"
    lines = [
        name,
        f"{source_type}  1  1  0.0  0  0          {role}, max dist type, atten type, sample step, dirflag, synflag",
        "1.0                        aleatory seg wt",
        f"{DIP[fault]:.1f}  0.0                  dip, depth to top",
        f"{len(trace)}                          Number of long/lat pts",
    ]
    lines += [f"{lon:.6f}  {lat:.6f}" for lon, lat in trace]
    rate_label = "rate of the sequence (in cluster)" if member else "annual frequency (out of cluster)"
    lines += [
        "1                          Number of dip variations",
        "0.0                          dip variations",
        "1.0                          wt for dip variations",
        "1                          Number of b-values",
        "0.9                          b-value (not used by the boxcar)",
        "1.0                          wt for b-value",
        f"{len(rates)}                         Number of activity rates ({rate_label})",
    ]
    lines += [f"0.9  {rate:.4e}  {w:.6g}" for rate, w in rates]
    lines += [
        "0.0  1.0  0.0  0.0         wts for sr, act. rate, rec int, MoRate branches",
        "0                          Number of slip-rates",
        "0                          Number of rec. int.",
        "0                          Number of moment rates",
        "1                          Number of mag recur models",
        "8                            mag recur model (8 = boxcar, +/-0.25 uniform, HID H.5.5.4)",
        "1.0                          wts for mag recur model",
        "0.5  0.0  0.0                mpdf1 = boxcar width, mpdf2, mpdf3",
        f"{len(THICKNESS)}                          Number of crustal thicknesses",
        f"{fmt([t for t, _ in THICKNESS], '.1f')}             crustal thickness (km)",
        f"{fmt([w for _, w in THICKNESS])}                 wts for crustal thickness",
        "0  0.0  0.0  0.0           Depth pdf model and params",
    ]
    for t, _ in THICKNESS:
        lines += [
            f"{len(mags)}                          Number of ref mags (rupture sets 1-6, thickness {t:.0f} km)",
            f"{fmt(mags, '.2f')}    ref mags (centre of the boxcar)",
            f"{fmt(mag_weights)}    wts (HID Table H-5.5-1)",
        ]
    min_mag = round(min(mags) - 0.25, 2)
    lines += [
        f"{min_mag:.2f}  0.1  1.0  1.0  1  1  0.0    minmag, magstep, hxStep, hzStep, nRupArea, nRupWidth, minDepth",
        AREA_COEF,
        WIDTH_COEF[FTYPE[fault]],
        "1                          Number of fault mech models",
        "1.0                          wts for fault mech models",
        "1                          Number of fault mech",
        f"{FTYPE[fault]:.1f}                          fault mech (0 = SS, 1 = RV)",
        "1.0                          wts for fault mech",
    ]
    return "\n".join(lines) + "\n"


def nmfs_fault_file(traces, sets, rates_in, rates_out):
    set_weights = [s["weight"] for s in sets]
    segments, names = [], []
    # Members: NMS (two geometries), RFT (two), NMN (two)
    for fault in FAULTS:
        for sheet, _ in GEOMETRY[fault]:
            name = f"01_NMFS_{fault}_{sheet}_in"
            names.append(name)
            segments.append(segment(name, fault, traces[sheet], rates_in,
                                    [s[fault] for s in sets], set_weights, member=True))
    # Out of cluster: RFT only, ordinary segments
    for sheet, _ in GEOMETRY["RFT"]:
        name = f"01_NMFS_RFT_{sheet}_out"
        names.append(name)
        segments.append(segment(name, "RFT", traces[sheet], rates_out,
                                [s["RFT"] for s in sets], set_weights, member=False))

    models = []
    for i_nms, (_, w_nms) in enumerate(GEOMETRY["NMS"]):
        for i_rft, (_, w_rft) in enumerate(GEOMETRY["RFT"]):
            for i_nmn, (_, w_nmn) in enumerate(GEOMETRY["NMN"]):
                flags = [0] * 8
                flags[i_nms] = flags[2 + i_rft] = flags[4 + i_nmn] = 1
                models.append((W_IN * w_nms * w_rft * w_nmn, flags,
                               f"in cluster: {GEOMETRY['NMS'][i_nms][0]}, {GEOMETRY['RFT'][i_rft][0]}, "
                               f"{GEOMETRY['NMN'][i_nmn][0]}"))
    for i_rft, (sheet, w_rft) in enumerate(GEOMETRY["RFT"]):
        flags = [0] * 8
        flags[6 + i_rft] = 1
        models.append((W_OUT_RFT * w_rft, flags, f"out of cluster, RFT only: {sheet}"))
    models.append((W_OUT_NONE, [0] * 8, "out of cluster, no activity"))
    assert abs(sum(w for w, _, _ in models) - 1.0) < 1e-9

    head = [
        "1                          iCoor (0=x,y 1=long,lat)",
        "1                          Number of fault systems",
        "01_NMFS",
        "1.0                        Prob Activity",
        f"{len(models)}                         Number of seg models (HID Figure H-5.5-1, June 2012)",
        f"{fmt([round(w, 6) for w, _, _ in models])}    wts for seg models",
        f"{len(segments)}                          Number of segments: {', '.join(names)}",
    ]
    head += [f"{' '.join(str(f) for f in flags)}            flags for seg model: {label}" for _, flags, label in models]
    return "\n".join(head) + "\n" + "".join(segments)


# ---------------------------------------------------------------------------
# Run files
# ---------------------------------------------------------------------------


def site_run_files(run_text, source_name, prefix):
    """One run file per site: the given run file with the fault file and its
    version replaced, and one site each, with outputs PREFIX_<site>.outN."""
    lines = run_text.splitlines()
    lines[0] = source_name
    lines[1] = re.sub(r"^\s*\S+", "45.4", lines[1], count=1)
    i_sites = next(i for i, ln in enumerate(lines) if re.search(r"\bnSites\b", ln))
    n_sites = int(lines[i_sites].split()[0])
    head = lines[:i_sites]
    out = {}
    for k in range(n_sites):
        site_line = lines[i_sites + 1 + 8 * k]
        old = lines[i_sites + 2 + 8 * k]
        label = re.sub(r"\.out1\s*$", "", old.strip()).split("_", 1)[-1]
        body = head + ["1     \t\t\t\t\tnSites", site_line]
        body += [f"{prefix}_{label}.out{j}" for j in range(1, 8)]
        out[label] = "\n".join(body) + "\n"
    return out


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("workbook", help="NMFS_RLME.xls (CEUS SSC HID database)")
    parser.add_argument("out_dir")
    parser.add_argument("--run", help="HAZ run file whose periods, GMMs and sites are used")
    parser.add_argument("--prefix", default="NMFS", help="prefix of the output file names")
    args = parser.parse_args()

    traces, sets, rates_in, rates_out = read_workbook(args.workbook)
    os.makedirs(args.out_dir, exist_ok=True)
    source = "NMFS_45.4.txt"
    with open(os.path.join(args.out_dir, source), "w") as f:
        f.write(nmfs_fault_file(traces, sets, rates_in, rates_out))
    print(f"wrote {source}: 8 segments, 11 segmentation models, {len(rates_in)} rates x {len(sets)} rupture sets")
    if args.run:
        with open(args.run) as f:
            runs = site_run_files(f.read(), source, args.prefix)
        for label, text in runs.items():
            name = f"Run_{args.prefix}_{label}.txt"
            with open(os.path.join(args.out_dir, name), "w") as f:
                f.write(text)
            print("wrote", name)


if __name__ == "__main__":
    main()
