"""Write the cluster test inputs in tests/cluster.

Each case has two runs over the same member faults:
  Members: each member is an ordinary source (type 1) in its own fault
           system with an activity rate of 1, so its hazard is P_k.
  Cluster: one fault system whose segments are the members (type 8),
           with the rate of the sequence as each member's activity rate.

Usage (from the repository root): python tests/cluster/make_inputs.py tests/cluster
"""
import os, sys

ROOT = sys.argv[1]  # tests/cluster

# Mean Poisson in-cluster rate of the NMFS sequence, HID Table H-5.5-2
ALPHA = 2.65702e-3

# Traces (lon, lat) from NMFS_RLME.xls (CEUS SSC HID Section H.5.5)
BA_BL = [(-90.3737, 35.5551), (-90.0972, 35.8038), (-89.7517, 36.4314), (-89.6209, 36.5653)]
RFT_S = [(-89.5905, 36.5662), (-89.4377, 36.4647), (-89.4140, 36.3308), (-89.3490, 36.2235)]
NMN_S = [(-89.6209, 36.5653), (-89.2882, 36.9946)]


SYSTEM_HEADER = """{name}
1.0                         Prob Activity
1                           Number of seg models
1.0                           wts for seg models
{n}                           Number of segments
{flags}                             flags for seg model
"""


def member_block(name, trace, dip, ftype, mag_model, mpdf1, ref_mag, min_mag,
                 step, n_area, sig_area, cluster=False):
    pts = "\n".join(f"{lon:.4f}  {lat:.4f}" for lon, lat in trace)
    if cluster:
        head = (f"{name}\n"
                "8  1  1  1.0  0  0          source type (8 = cluster member), max dist type, atten type, sample step, dirflag, synflag\n")
        rate = f"1.0  {ALPHA:.5e}  1.0           b-value (not used), activity rate = rate of the sequence, wt"
    else:
        head = SYSTEM_HEADER.format(name=name, n=1, flags="1") + (
            f"{name}\n"
            "1  1  1  1.0  0  0          source type, max dist type, atten type, sample step, dirflag, synflag\n")
        rate = "1.0  1.0  1.0                 b-value (not used), activity rate = 1 (hazard = P(exceedance | earthquake)), wt"
    return head + f"""1.0                         aleatory seg wt
{dip:.1f}  0.0                   dip, depth to top
{len(trace)}                           Number of long/lat pts
{pts}
1                           Number of dip variations
0.0                           dip variations
1.0                           wt for dip variations
0                           Number of b-values
1                           Number of act rates
{rate}
0.0  1.0  0.0  0.0          wts for sr, act. rate, rec int, MoRate branches
0                           Number of slip-rates
0                           Number of rec. int.
0                           Number of moment rates
1                           Number of mag recur models
{mag_model}                             mag recur model
1.0                           wts for mag recur model
{mpdf1:.2f}  0.0  0.0                mpdf1, mpdf2, mpdf3
1                           Number of fault widths
15.0                          seismogenic thickness (km)
1.0                           wts for fault widths
0  0.0  0.0  0.0            Depth pdf model and params
1                           Number of ref mags
{ref_mag:.2f}                          ref mag
1.0                           wts for ref mags
{min_mag:.2f}  0.1  {step:.1f}  {step:.1f}  {n_area}  1  0.0    minmag, magstep, hxStep, hzStep, nRupArea, nRupWidth, minDepth
-3.49  0.91  {sig_area:.2f}           rupArea coeff: a, b, sigma (log10 units; Wells and Coppersmith 1994, all)
-1.01  0.32  0.0            rupWidth coeff: a, b, sigma (log10 units; Wells and Coppersmith 1994, all)
1                           Number of fault mech models
1.0                           wts for fault mech models
1                           Number of fault mech
{ftype:.1f}                           fault mech
1.0                           wts for fault mech
"""


def fault_file(title, blocks):
    return (f"1                           iCoor (0=x,y 1=long,lat)\n"
            f"{len(blocks)}                           Number of fault systems ({title})\n"
            + "".join(blocks))


def cluster_file(title, name, blocks):
    return (f"1                           iCoor (0=x,y 1=long,lat)\n"
            f"1                           Number of fault systems ({title})\n"
            + SYSTEM_HEADER.format(name=name, n=len(blocks), flags=" ".join("1" * len(blocks)))
            + "".join(blocks))


def run_file(source, title, periods, sites, prefix, mag_bins, dist_bins):
    lines = [f"{source}",
             "45.4                        version of fault file",
             "30.0  45.0  -100.0  -80.0    min max lat long (grid sources only)",
             "1000.  1000.                 maxdist (types 1 and 2)",
             title,
             f"{len(periods)}  1                        nProb, nAttenType"]
    for per in periods:
        lines.append(f"{per['T']:.1f}  {per['eps']:.0f}  0  0                 period, max epsilon, dir flag, pcflag")
        lines.append(f"{len(per['levels'])}")
        lines.append("  ".join(f"{x:g}" for x in per['levels']))
        lines.append(f"{len(per['gmms'])}                           Number of GM Models for Atten Type 1")
        for g in per['gmms']:
            lines.append(g)
    lines += ["0                           psCorFlag",
              f"{len(mag_bins)}                           Number of deag mag values",
              "  ".join(f"{x:g}" for x in mag_bins),
              f"{len(dist_bins)}                          Number of deag dist values",
              "  ".join(f"{x:g}" for x in dist_bins),
              "6                           Number of deag eps values",
              "-10.  -2.  0.  1.  2.  10.",
              "4                           Number of deag dir values",
              "0.0  0.10  0.40  2.0",
              "0                           soil amp",
              f"{len(sites)}                           nSites"]
    for i, (lon, lat, label) in enumerate(sites, 1):
        lines.append(f"{lon:.2f}  {lat:.2f}  2000.0  0.0  0.0  0.0  0.8  0   site {i}: {label} (lon, lat, Vs30, Z1.0, Z1.5, Z2.5, Vrup, FABA)")
        for k in range(1, 8):
            lines.append(f"{prefix}_Site{i}.out{k}")
    return "\n".join(lines) + "\n"


def write(path, text):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w") as f:
        f.write(text)


def write_case(case, tag, members, title, periods, sites, mag_bins, dist_bins):
    """Write the Members and Cluster runs of one case.

    members: list of (name, member_block keyword arguments)
    """
    for kind in ("Members", "Cluster"):
        cluster = kind == "Cluster"
        blocks = [member_block(name, cluster=cluster, **kw) for name, kw in members]
        source = f"Source_{tag}_{kind}.txt"
        if cluster:
            text = cluster_file(f"cluster test {tag[-1]}: one clustered sequence", f"{tag} NMFS cluster", blocks)
            run_title = f"Cluster test {tag[-1]}: {title}, as one clustered sequence (source type 8), rate {ALPHA:.5e}"
        else:
            text = fault_file(f"cluster test {tag[-1]} members, each run as an ordinary source with rate 1", blocks)
            run_title = f"Cluster test {tag[-1]} members: {title}, as ordinary sources with rate 1"
        write(f"{ROOT}/{case}/{kind}/Input/{source}", text)
        write(f"{ROOT}/{case}/{kind}/Input/Run_{tag}_{kind}.txt",
              run_file(source, run_title, periods, sites, f"{tag}_{kind}", mag_bins, dist_bins))


# ---- Test A: one rupture per member, one GMM with a fixed sigma -------------
# Single magnitude (model 3 with mpdf2 = 0), rupture-area sigma 0, and magnitudes
# large enough that the rupture fills each fault, so each member has exactly one
# rupture and P_k(z) = 1 - Phi((ln z - ln median_k) / sigma), as in the
# New_Madrid_clustering.xlsx concept spreadsheet.
levels_ta = [0.001, 0.002, 0.003, 0.005, 0.01, 0.02, 0.03, 0.05, 0.1, 0.2, 0.3, 0.5, 1.0, 2.0, 3.0, 5.0]
write_case(
    "TA_SingleRupture", "TA",
    [("TA_NMS_BA-BL", dict(trace=BA_BL, dip=90.0, ftype=0.0, mag_model=3, mpdf1=0.0, ref_mag=7.9, min_mag=7.85, step=1.0, n_area=1, sig_area=0.0)),
     ("TA_RFT_S", dict(trace=RFT_S, dip=40.0, ftype=1.0, mag_model=3, mpdf1=0.0, ref_mag=7.8, min_mag=7.75, step=1.0, n_area=1, sig_area=0.0)),
     ("TA_NMN_S", dict(trace=NMN_S, dip=90.0, ftype=0.0, mag_model=3, mpdf1=0.0, ref_mag=7.6, min_mag=7.55, step=1.0, n_area=1, sig_area=0.0))],
    "NMS, RFT, NMN, one rupture each",
    [dict(T=0.0, eps=8, levels=levels_ta,
          gmms=["-102  0.0  0.0  1.0  0.0  0  -1  0.503442     jcalc, const1, const2, wt, SigVaradd, mixflag, j_sigma, fix_sigma (AB06 hard rock, fixed sigma)"])],
    [(-85.70, 36.40, "about 350 km east of the NMFS")],
    [6.5, 7.0, 7.5, 7.7, 7.85, 8.0],
    [0.0, 300.0, 325.0, 350.0, 375.0, 400.0, 1000.0])

# ---- Test B: NMFS-like members with aleatory variability, two GMMs ----------
# Rupture set 5 of HID Table H-5.5-1 (NMS 6.9, RFT 7.3, NMN 7.0) with the
# +/-0.25 uniform magnitude variability (model 8, boxcar width 0.5), rupture-area
# sigma 0.24 (5 samples) and 1 km rupture spacing, so ruptures float along strike.
gmms_tb = ["102  0.0  0.0  0.5  0.0  0             jcalc, const1, const2, wt, SigVaradd, mixflag (Atkinson and Boore 2006, hard rock)",
           "120  0.0  0.0  0.5  0.0  0             jcalc, const1, const2, wt, SigVaradd, mixflag (Campbell 2003, hard rock)"]
write_case(
    "TB_NMFS", "TB",
    [("TB_NMS_BA-BL", dict(trace=BA_BL, dip=90.0, ftype=0.0, mag_model=8, mpdf1=0.5, ref_mag=6.9, min_mag=6.65, step=1.0, n_area=5, sig_area=0.24)),
     ("TB_RFT_S", dict(trace=RFT_S, dip=40.0, ftype=1.0, mag_model=8, mpdf1=0.5, ref_mag=7.3, min_mag=7.05, step=1.0, n_area=5, sig_area=0.24)),
     ("TB_NMN_S", dict(trace=NMN_S, dip=90.0, ftype=0.0, mag_model=8, mpdf1=0.5, ref_mag=7.0, min_mag=6.75, step=1.0, n_area=5, sig_area=0.24))],
    "NMS, RFT, NMN",
    [dict(T=0.0, eps=3, gmms=gmms_tb,
          levels=[0.005, 0.01, 0.02, 0.03, 0.05, 0.07, 0.1, 0.15, 0.2, 0.3, 0.4, 0.5, 0.7, 1.0, 1.5, 2.0, 3.0, 5.0]),
     dict(T=1.0, eps=3, gmms=gmms_tb,
          levels=[0.001, 0.002, 0.005, 0.007, 0.01, 0.015, 0.02, 0.03, 0.05, 0.07, 0.1, 0.15, 0.2, 0.3, 0.5, 0.7, 1.0, 2.0])],
    [(-90.05, 35.15, "Memphis"), (-90.20, 38.63, "St. Louis")],
    [6.4, 6.6, 6.8, 7.0, 7.2, 7.4, 7.6],
    [0.0, 10.0, 20.0, 30.0, 50.0, 75.0, 100.0, 150.0, 200.0, 250.0, 300.0, 1000.0])
