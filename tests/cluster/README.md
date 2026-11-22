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
activity rate. Since Phase 1, HAZ reads these runs and stops before the hazard
calculation, so the test runner skips them (`pending_cases` in
`scripts/run_tests.py`). Phase 2 adds their reference outputs, which are
compared against the reference values from the Members runs.

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
```

**Disaggregation with one GMM.** If a case has a single GMM, the allocation
factor `s(z)` is one number per level. The cluster's disaggregation fractions
and its M, R and epsilon means are then the same as those of the member run.
