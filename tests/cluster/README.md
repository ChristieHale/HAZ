# Cluster source tests (source type 8)

Test cases for clustered earthquake sequences. See
`docs/Cluster_Source_Spec.md` for the formulation and the implementation plan.

## How the reference values are built

A cluster's hazard on a branch is `alpha * (1 - prod_k (1 - P_k))`. Here
`P_k(z)` is the probability that member k's earthquake exceeds z, given that
the sequence occurs.

Each case has a **Members** run. It contains the same member faults, each as
an ordinary type 1 source in its own fault system with an activity rate of 1.
The hazard HAZ computes for each source is then exactly `P_k`.
`scripts/cluster_reference.py` reads those runs and computes the cluster
hazard and its disaggregation independently of the HAZ Fortran.

- **out1:** gives `P_k` for each GMM to 6 significant digits.
- **out3:** gives the GMM-weighted values to 4 significant digits.
- **out4:** gives the disaggregation fractions to 3 significant digits.

Each case also has a **Cluster** run: the same faults as the segments of one
fault system, as source type 8, with the sequence rate as each member's
activity rate. `scripts/test_cluster_hazard.py` runs HAZ on both and compares
the Cluster run with the reference values from the Members run. The Cluster
runs' outputs are also regression references for `scripts/run_tests.py`.

In the Cluster runs' out1, the members' blocks hold zeros, and the cluster's
branch curves, `alpha * ProbAct * P_c`, follow the last member, one set of
blocks for each segmentation model, numbered nFlt + 1, nFlt + 2, ...
(convention F3 in the spec). In out3, each member's line holds its allocated
share, and the shares add up to the cluster hazard.

All of the input files are written by `make_inputs.py`; edit it, not the inputs:

```
python tests/cluster/make_inputs.py tests/cluster
```

## Cases

### TA_SingleRupture

Each member has exactly one rupture:
- a single magnitude (model 3 with mpdf2 = 0);
- a rupture-area sigma of 0;
- magnitudes large enough that the rupture fills the fault.

**Faults and magnitudes**
- NMS (BA-BL trace), M 7.9
- RFT (RFT-S trace, dip 40), M 7.8
- NMN (NMN-S trace), M 7.6

**GMM and site**
- One GMM: Atkinson and Boore (2006) hard rock (jcalc 102) with a fixed sigma of 0.503442.
- One site about 350 km east of the faults.

`P_k` is therefore a single lognormal exceedance, the setting of
`New_Madrid_clustering.xlsx`, but with HAZ's medians. HAZ attenuation mode
(run option 1) gives these medians:

| Member | Rrup (km) | Median |
|---|---|---|
| NMS | 351.00 | 0.01953 g |
| RFT | 327.53 | 0.02010 g |
| NMN | 326.65 | 0.01728 g |

### TB_NMFS

**Faults and magnitudes**
- The BA-BL, RFT-S and NMN-S traces.
- Magnitudes from HID rupture set 5: NMS 6.9, RFT 7.3, NMN 7.0.
- The HID's ±0.25 uniform magnitude variability (model 8, boxcar width 0.5, 0.1 steps).

**Rupture variability**
- Rupture-area sigma of 0.24, with 5 samples.
- Ruptures spaced 1 km apart, so they float along strike.

**GMMs, periods and sites**
- Two GMMs, weighted 0.5 / 0.5: jcalc 102 (Atkinson and Boore 2006) and jcalc 120 (Campbell 2003), both hard rock, truncated at 3 sigma.
- Two periods: PGA and 1.0 s.
- Two sites: Memphis and St. Louis.

### TC_Geometries

Alternative geometries and epistemic branches (Phase 5).

**Segments and segmentation models**
- Six segments: the BA-BL and BA-BFZ traces of NMS, RFT-S and RFT-L, NMN-S and
  NMN-L (NMFS_RLME.xls).
- Eight segmentation models, each picking one geometry of each fault, with
  product weights 0.6/0.4 (NMS) × 0.7/0.3 (RFT) × 0.7/0.3 (NMN).

**Branches, matched by index in every member**
- Thickness 13 / 15 / 17 km, weights 0.3 / 0.5 / 0.2.
- Two rates of the sequence (3.7e-3 and 1.4e-3, 0.5 / 0.5) times rupture sets 1
  and 5 of HID Table H-5.5-1 (each member's own magnitude, 0.5 / 0.5), with the
  ±0.25 boxcar.
- Two mechanisms (0.5 / 0.5) and the two GMMs of test B.

The Members run has activity rate 1 on every branch, so each out1 curve is
P_k on that branch. Two periods (PGA and 1.0 s), two sites (Memphis and St.
Louis).

### TD_InOut

In- and out-of-cluster branches in one fault system (Phase 6). Test C's six
members plus RFT-S and RFT-L as ordinary segments (type 1) with out-of-cluster
rates (3 thicknesses, 2 rates × 2 rupture sets, 2 mechanisms), and eleven
segmentation models as in HID Figure H-5.5-1: the eight geometry combinations
of the cluster at 0.9 × the geometry weight, RFT-S or RFT-L alone at
0.05 × 0.7 / 0.3, and no segment at 0.05.

## Commands

From `scripts/`:

```
# New_Madrid_clustering.xlsx
python cluster_reference.py spreadsheet

# Cluster hazard from a member run (alpha = mean Poisson in-cluster rate, HID Table H-5.5-2)
python cluster_reference.py members ../tests/cluster/TB_NMFS/Members/Input/Run_TB_Members.txt \
    ../tests/cluster/TB_NMFS/Members/Output/TB_Members_Site1.out1 --alpha 2.65702e-3 --by-gmm

# Allocated disaggregation: one member run per GMM (the same faults, a single GMM each)
python cluster_reference.py deagg Run.txt --out1 g1.out1 g2.out1 --out4 g1.out4 g2.out4 \
    --weights 0.5 0.5 --alpha 2.65702e-3

# Unit tests
python -m pytest test_cluster_reference.py

# Reading of source type 8: runs HAZ on the Cluster inputs and on one variant per rule
HAZ_BIN=/path/to/HAZ python -m pytest test_cluster_reader.py

# Mean hazard: runs HAZ on the Members and Cluster inputs and compares them
HAZ_BIN=/path/to/HAZ python -m pytest test_cluster_hazard.py

# Disaggregation: also runs test B's members with each GMM on its own
HAZ_BIN=/path/to/HAZ python -m pytest test_cluster_deagg.py

# Branches and segmentation models: test C
HAZ_BIN=/path/to/HAZ python -m pytest test_cluster_branches.py

# In- and out-of-cluster branches in one fault system: test D
HAZ_BIN=/path/to/HAZ python -m pytest test_cluster_inout.py
```

`test_cluster_deagg.py` checks the Cluster runs' out3 member shares, out4 bins
and means, and out7 member means against the reference tool, applied to
Members runs with a single GMM each. Its comparisons allow for the rounding of
the printed numbers in both the Cluster run and the Members runs.

`test_cluster_hazard.py` checks:
- **Test A:** the cluster curve against `alpha * (1 - prod_k (1 - P_k))`, with
  the lognormal medians implied by the Members run.
- **Tests A and B:** the cluster curve on each GMM branch against the
  reference tool, applied to the Members run's out1; the GMM-weighted out3
  total; and the members' out3 shares adding up to it.
- **Limits:** the cluster tends to `alpha` at the lowest level and to the
  independent sum at the highest level with hazard.
- **One member:** a cluster of one member gives out2 to out7 identical to an
  ordinary type 1 source with the same rate.

**Disaggregation with one GMM.** If a case has a single GMM, the allocation
factor `s(z)` is one number per level. The cluster's disaggregation fractions
and its M, R and epsilon means are then the same as those of the member run.
