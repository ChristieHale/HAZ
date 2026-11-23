# Clustered Earthquake Sequences in HAZ (Source Type 8)

Specification and implementation plan. See the Status section at the end for the phases done.

Branch: `cluster_implementation`

Sources used:
- CEUS SSC Hazard Input Document (HID), Appendix H, Section H.5.5, the New Madrid Fault System (NMFS) RLME
- CEUS SSC Category 1 update (June 27, 2012), which corrects Figure H-5.5-1
- NMFS_RLME.xls
- USGS Open-File Report 2008-1128 (Petersen et al., 2008), Appendix F, on temporal clustering
- New_Madrid_clustering.xlsx (the concept spreadsheet)
- nmfs.c, nmfs.h and nmfs_cluster.c from another hazard code
- The fractile program, ChristieHale/Fractiles, branch `compatibility_issues`
- USGS nshmp-haz (GitHub mirror) cluster and disaggregation code, and the USGS 2014 model's "SSCn New Madrid" cluster input (nshmp-model-cous-2014). The 5.3.1 model on code.usgs.gov could not be reached from this environment

---

# Part A: Specification

## A1. Purpose and scope

The CEUS SSC model treats the 1811–1812 New Madrid sequence as a **cluster**:
- Three large earthquakes occur on three nearby faults: NMS (southern), RFT (Reelfoot thrust) and NMN (northern).
- The three occur together, within months.
- The sequence as a whole has **one** rate of occurrence.

HAZ cannot represent this today:
- Every rupture's rate × P(exceedance) is added straight into the hazard (`01_HAZ_MAIN2.f:568-576`).
- Segments within a fault system are independent sources.
- The existing "synchronous rupture" option does something different. It raises the median of one rupture by taking the square root of the sum of squares (SRSS) with a second rupture's median.

**Goal:** add a new source type for a clustered sequence of K member ruptures. It should give the correct mean hazard, work with HAZ's disaggregation outputs, and work with the fractile program.

**First milestone, deliberately simple:**
- One cluster of three members.
- Each member has its own fault geometry and its own aleatory magnitude distribution.
- One cluster rate.
- No SSC epistemic branches: one geometry, one thickness, one rate, one magnitude branch.
- One or more GMMs.

Epistemic branches and the full NMFS logic tree come later (Part B, Phases 5–6).

## A2. Hazard formulation

### A2.1 Member exceedance probability

The member exceedance probability is the probability that member k's earthquake exceeds the ground-motion level, given that the sequence occurs.

For one set of epistemic choices b (a branch), member k and test level z:

```
P_k(z | b) = Σ_r  p_r · P(Y > z | rupture r, b)
```

- r runs over member k's aleatory alternatives: magnitude bin, rupture area and width, location along strike and down dip, and hypocentre when directivity is on.
- p_r is HAZ's usual `p1` = pMag·pArea·pWidth·pLocX·pLocY·pHypo, normalised so Σ p_r = 1.
- P(Y > z | …) is HAZ's usual `pRock`, the truncated lognormal exceedance probability.

This is exactly what HAZ already integrates for a source type 1 fault. The only difference is that the rate and Prob Activity are left out.

### A2.2 Cluster combination (OFR 2008-1128, Appendix F)

Given that the sequence occurs, the member earthquakes are treated as independent events. The probability that **at least one** of them exceeds z is:

```
P_c(z | b) = 1 − Π_k [ 1 − P_k(z | b) ]
```

The cluster rate of exceedance on branch b is:

```
ν_c(z | b) = α_b · ProbAct · P_c(z | b)
```

α_b is the rate of the sequence on branch b. ProbAct is the fault system's Prob Activity.

The mean hazard is the weighted sum over branches:

```
ν̄_c(z) = Σ_b  w_b · ν_c(z | b)
```

Your spreadsheet's "Correct" row is this formula with a single rupture per member. Its "Incorrect" row, α Σ_k P_k, is what HAZ would compute today if the three faults were entered as ordinary sources sharing the rate.

### A2.3 Apply the formula on each branch, then weight

The combination is nonlinear, so it has to be applied on each epistemic branch, and the branch results weighted afterwards.

GMM branches matter most here. In a given GMM branch the same GMM applies to all three earthquakes, so the members' probabilities move up and down together across branches.

Averaging each member's curve over the branches first and then combining (the approach in the other code) overestimates the hazard. For two members, with the covariance taken over the branches, the difference is:

```
exact = approximation − cov(P_1, P_2)
```

The covariance is positive because the GMM, thickness and rupture-set branches are shared. The difference is largest at low ground motions and small where the P_k are small.

HAZ will compute the exact form. The approximation can be reproduced in post-processing for comparison (Phase 6).

### A2.4 Size of the effect (spreadsheet case)

Inputs are from New_Madrid_clustering.xlsx:
- NMS, RFT, NMN at R = 350 km with M 7.9 / 7.8 / 7.6
- σ = 0.503
- α = 2.657e-3, the weighted mean of HID Table H-5.5-2

| z (g) | P_NMS | P_RFT | P_NMN | P_c | Cluster ν | Independent ν | Independent / cluster |
|---|---|---|---|---|---|---|---|
| 0.01 | 0.997 | 0.994 | 0.984 | 1.000 | 2.66e-3 | 7.90e-3 | 2.97 |
| 0.02 | 0.910 | 0.876 | 0.776 | 0.998 | 2.65e-3 | 6.81e-3 | 2.57 |
| 0.05 | 0.315 | 0.253 | 0.144 | 0.562 | 1.49e-3 | 1.89e-3 | 1.27 |
| 0.10 | 0.032 | 0.021 | 0.007 | 0.059 | 1.56e-4 | 1.58e-4 | 1.02 |
| 0.20 | 6e-4 | 3e-4 | 1e-4 | 1.0e-3 | 2.64e-6 | 2.64e-6 | 1.00 |

- At low ground motions the cluster rate tends to α. The independent treatment tends to Kα.
- At high ground motions the two converge.

### A2.5 Assumptions

1. Given that the sequence occurs, every member occurs: each member's event probability is 1. Its magnitude and location are aleatory and follow that member's own distributions.
2. Given that the sequence occurs, the members' ground motions are independent. Each member is a separate earthquake with its own between-event and within-event residuals. This is the OFR Appendix F assumption.
3. The sequence occurs in time as a Poisson process with rate α. The out3 conversion `1 − exp(−ν)` therefore still applies to α·P_c. The few months between members are ignored.

## A3. Input format: source type 8

### A3.1 How a cluster is defined

- **A cluster is a fault system whose segments are all source type 8.** Each segment is one member. The fault-system level of the 45.4 fault file already groups segments, so no new grouping line is needed.
- **A type 8 segment block has exactly the same lines as a type 1 block.** That block contains the trace, dip, thickness, rate, magnitude, rupture dimensions and mechanism. Keeping the format identical keeps the HAZ reader and the fractile program's reader changes small: they parse type 8 as type 1. Existing 45.4 files are unaffected.
- **Rate.** In a member block, the activity rate is the **rate of the sequence**. Every member must give identical activity-rate branches (values and weights). HAZ checks this and stops if they differ. Repeating the rate is redundant but keeps each block a complete, standard block. The b-value read with each activity rate is not used by the boxcar or single-magnitude models.
- **Magnitude.** Each member's magnitude distribution is its own. It is normalised over [minMag, maxMag], so `p1_sum` should be 1.0.
  - The NMFS ±0.25 uniform variability is magnitude model 8 (boxcar), width 0.5, centred on the reference magnitude.
  - A single magnitude is model 3 with σ = 0.
  - Setting minMag = refMag − 0.25 with magStep = 0.1 gives five magnitudes at refMag −0.2 … +0.2, each with weight 0.2. That is the same discretisation as the other code.
- **Prob Activity** of the fault system multiplies α, as it does for every other source.

### A3.2 Validation rules (first milestone)

HAZ stops with a clear message if any of these fails.

| Rule | Reason |
|---|---|
| The type 8 segments of a fault system are consecutive (Phase 6; until then, every segment of the system) | The members are one cluster; other segments of the system are ordinary (A9) |
| Every segmentation model includes a member, and every member is in a model (Phase 5; until then, one model with all flags 1) | Each model is one combination of members, such as one geometry of each fault (A8) |
| Each member has activity rates only: no slip rates, recurrence intervals or moment rates | α must be a rate of the sequence, not derived from fault area or slip. Recurrence interval is not handled for model 8 in `S31_Set_Rates` |
| Identical activity-rate values and weights in every member | They are one rate |
| Aleatory segment weight = 1.0 | Every member occurs in every sequence |
| `synflag` = 0 | Not combined with synchronous rupture |
| Same attenuation type in every member | GMM branches are matched across members |
| PC method off (`pcflag` = 0) for the periods run | The PC terms are linear in the rate |
| The same numbers and weights of thickness-and-dip, rate-and-magnitude and mechanism branches in every member (Phase 5; until then, one of each) | Branches are matched by index (A8) |

### A3.3 Example: the NMS member, 45.4 format

RFT and NMN follow the same pattern:
- RFT: dip 40, ftype 1.0, refMag 7.8, minMag 7.55
- NMN: refMag 7.6, minMag 7.35

```
New Madrid Fault System (in-cluster)        fault system name
1.0                 Prob Activity
1                   Number of seg models
1.0                   wts for seg models
3                   Number of segments (cluster members)
1 1 1                 flags for seg model
NMS (BA-BL)                                 member name
8  1  1  1.0  0  0  source type (8 = cluster member), max dist type, atten type, sample step, dirflag, synflag
1.0                 aleatory seg wt (must be 1.0)
90.0  0.0           dip, depth to top
4                   Number of long/lat pts
-90.3737  35.5551
-90.0972  35.8038
-89.7517  36.4314
-89.6209  36.5653
1                   Number of dip variations
0.0                   dip variations
1.0                   wt for dip variations
0                   Number of b-values
1                   Number of act rates
1.0  2.657E-3  1.0    b-value (unused), activity rate = rate of the SEQUENCE, wt
0.0  1.0  0.0  0.0  wts for sr, act. rate, rec int, MoRate branches
0                   Number of slip-rates
0                   Number of rec. int.
0                   Number of moment rates
1                   Number of mag recur models
8                     mag recur model (8 = boxcar)
1.0                   wts for mag recur model
0.5  0.0  0.0         mpdf1 = boxcar width, mpdf2, mpdf3
1                   Number of fault widths
15.0                  seismogenic thickness (km)
1.0                   wts
0  0.0  0.0  0.0    Depth pdf model and params
1                   Number of ref mags
7.9                   ref mag (centre of the boxcar)
1.0                   wts
7.65  0.1  1.0  1.0  1  1  0.0   minmag, magstep, hxStep, hzStep, nRupArea, nRupWidth, minDepth
 a  b  sigma        rupArea coeff (member's magnitude-area relation)
 a  b  sigma        rupWidth coeff
1                   Number of fault mech models
1.0                   wts
1                   Number of fault mech
0.0                   fault mech
1.0                   wts
```

Inside the reader, a type 8 segment is stored as geometry type 1, plus a cluster index. As a result, every existing type 1 code path (fault bottom, gridding, distances, depth pdf, rupture dimensions) is reused unchanged. There are about 40 `sourceType` branches in the code. A new number reaching them would silently fall into the `else` (areal or fault) cases, so it never reaches them.

## A4. How HAZ computes it: two passes over the members

The rate is currently multiplied in at the innermost loop and the result scattered into about 25 accumulators. A cluster needs every member's P_k before any member's contribution can be added. When the source loop reaches the first member of a cluster, it therefore processes the members twice.

**Pass 1 (probability pass).** For each member, run the normal type 1 integration. At the innermost level, add `p1 · pRock` into the member array:

```
Pmem(k, jInten, iProb, iAtten, iParam, iFtype, iFltWidth)
```

- Rate and Prob Activity are left out.
- Nothing else is accumulated, and nothing is written to out1, out2 or out5.
- `p1_sum` is still checked.

**Combination.** On every branch, compute the quantities below. When every P_k is below about 1e-16, the plain product `1 − Π(1 − P_k)` loses them to round-off, so P_c is computed in a form that keeps small probabilities (for example, the sum when all P_k are tiny).

- `P_c = 1 − Π_k (1 − P_k)`
- the allocation factor `s = P_c / Σ_k P_k` (s = 1 when Σ_k P_k = 0), described in A5

**Pass 2 (contribution pass).** Re-run the members through the normal code. The only change is the marginal rate (`01_HAZ_MAIN2.f:568`):

```
ordinary source:  mHaz = rate · pRock · p1 · ProbAct
cluster member:   mHaz = α · s(z | b) · pRock · p1 · ProbAct
```

All existing accumulators then work unchanged: Haz, HazBins, m_bar/d_bar/e_bar, the 13 source-deagg arrays, and tempHaz, tempHaz1 and tempHaz2. On every branch, the members' contributions add up exactly to α·ProbAct·P_c.

Notes:
- **Run time.** Only the cluster members are integrated twice. With three faults this is negligible.
- **Storage.** The Pmem array is too large to declare at HAZ's maximum dimensions: MAXPARAM = 300 and MAX_ATTEN = 100 would need gigabytes. It will be an allocatable array, sized when the cluster is reached from the actual numbers of levels, periods, GMMs and branches. HAZ has no allocatable arrays today; gfortran supports them in fixed form. The full NMFS tree needs a few tens of MB.
- **Loop control.** The `do 900 iFlt` loop becomes an explicitly controlled loop, so that it can step back to the first member for pass 2. The loop body is not moved or rewritten.

## A5. Disaggregation

Neither the HID nor the OFR defines disaggregation for a cluster. The cluster rate is not a sum over ruptures, so a rule is needed for attributing it to M, R and ε bins and to sources.

**Proposed rule: proportional allocation.** On each branch and at each test level z:
1. Every rupture of every member keeps its usual relative contribution, α·p_r·P(Y > z | r).
2. All of the cluster's contributions are scaled by the same factor s(z | b) = P_c / Σ_k P_k.

Properties:
- The allocated contributions add up **exactly** to the cluster hazard, per branch and in total.
- out3 per-source lines, out4 bins, M̄/D̄/ε̄ and out7 are all consistent with the mean curve.
- It does not depend on the order of the members.
- At low P_k (most design levels), s → 1, and it becomes ordinary disaggregation of the members as if they were independent.
- At low ground motions, s → 1/K. When all three members are certain to exceed, each is credited with a third.
- It needs nothing beyond P_k, which is already computed in pass 1.

Alternatives considered:

| Rule | Meaning | Why not proposed |
|---|---|---|
| Largest ground motion | Credit the member whose ground motion is the largest in the sequence | Clear physical meaning, but needs the full distribution of each member's ground motion and an integral against the other members' CDFs for every rupture. Much more code for small differences at design levels |
| Order of occurrence | Credit the first member in the sequence that exceeds z: member k gets α P_k Π_{j<k}(1 − P_j) | Exact and additive, but depends on an assumed order. Historically the order was NMS (Dec 1811), NMN (Jan 1812), RFT (Feb 1812); the SSC model does not specify one |

The USGS code (nshmp-haz, `Deaggregator.processClusterSources`) uses the same rule:
1. It bins the member ruptures as if they were independent.
2. It then scales all of a cluster's bins so that they add up to the cluster rate at the target level.

It does this separately for each GMM and each geometry variant. HAZ would apply the factor on every branch.

The other code's disaggregation routines (`newmadrid_deagg_sumaf*`) were not in the files received.

## A6. Output files

| File | Cluster members |
|---|---|
| out3 | Each member's line holds its allocated share. Shares add up to the cluster hazard, and the total is correct. No format change (the UHS_deagg companion program reads this file) |
| out4 | M–R–ε bins and bars include the allocated contributions. No format change |
| out7 | Per-member allocated disaggregation. A member with zero hazard gives NaN (as for any source today) |
| out2 | Each member's magnitude rates, α × pMag. This is the correct count of earthquakes: each member produces one event per sequence |
| out5, out6 | Allocated per-branch contributions. They sum correctly over the cluster's members. out6 (GMM tornado) is correct as is |
| out1 | Convention F3 (A7): zeros in the members' blocks; the cluster's branch curves follow the last member, one set per segmentation model |

## A7. Fractiles

The fractile program is in the separate repository ChristieHale/Fractiles (`fract_main.f`, `rd_flt.f`). It reads:
- the HAZ run file, for the GMM weights;
- the fault file, for the SSC weights;
- out1, for each segment's branch curves.

In each Monte Carlo realisation it:
1. Draws one GMM per attenuation type. That GMM is used for every source of that type.
2. Draws one segmentation model for each fault system.
3. For each segment flagged in that model, draws a mechanism, a thickness/dip branch and a rate × magnitude branch, **independently for each segment**. It then adds that out1 curve × the aleatory segment weight.

Prob Activity is not sampled; it is already inside the out1 curves.

What this means for a cluster:
- The shared GMM draw is already right.
- The independent draws for each segment are not. The cluster's curve on a branch is not a sum over members, so it cannot be split into member curves that are drawn separately. The cluster has to be **one draw**.

There are two ways to write out1:

| Option | out1 content | Effect on the fractile program |
|---|---|---|
| **F1** | The cluster's branch curves α·P_c(z \| b) in the first member's block, zeros in the other members' blocks | Correct as the program stands: the first member's draw picks the cluster's branch, and the zero blocks add nothing. Needs only the fault reader to accept type 8, a one-line change in `rd_flt.f` (type 8 joins type 1 in the geometry branch; the dip and thickness lines are already read for every type except 5) |
| **F2** | Allocated shares in each member's block | Wrong as the program stands: each member would be drawn on a different branch |

F1 was used in Phases 2 to 4. It cannot hold a cluster with several segmentation models: one member (such as BA-BL) is in several models, and the cluster's curve depends on the model. From Phase 5, out1 follows **F3** (agreed on November 23, 2026):

| Option | out1 content | Effect on the fractile program |
|---|---|---|
| **F3** | Zeros in every member's block. After the cluster's last member, one set of blocks for each segmentation model s, in the usual layout (one block per width branch and period), holding α·ProbAct·P_c,s(z \| b) on every branch. The source number in the block header is nFlt + 1, nFlt + 2, ... in the order the sets are written for the site, so it never clashes with a source | Draw a segmentation model for the cluster's fault system, then one branch with the cluster's matched weights, and add that model's curve. One code path for every cluster, with one or several segmentation models |

The segmentation-model draw is already a mutually exclusive choice for each fault system, which is what the alternative geometries and the in/out-of-cluster branches need (A8, A9).

**Problems already in the fractile repository.** These are not part of this work unless you want them fixed:
- **Tests don't run.** Neither test case runs as committed:
  - their run files lack the PC flag that `rd_input.f` now reads;
  - their out1 files come from an older HAZ version, which the out1 version check rejects.
- **Source types 6 and 7 are read differently from HAZ.**
  - Type 6 geometry is not read.
  - For types 6 and 7, it reads dip-variation and thickness lines that HAZ does not have.

  A fault file containing either type would be misread.

## A8. Epistemic branches (Phase 5)

The rule: **members are matched branch by branch, by index.**
- Each member must have the same number of branches, with the same weights, for each of:
  - thickness and dip (iFltWidth)
  - rate × magnitude (iParam)
  - mechanism (iFtype)
  - GMM (iAtten)
- The combination is applied on each matched branch.

How the NMFS logic tree maps onto this:

| NMFS node | HAZ input | How it is combined |
|---|---|---|
| Crustal thickness 13 / 15 / 17 km, weights 0.3 / 0.5 / 0.2 (corrected Figure H-5.5-1, June 2012) | Fault widths, same list in each member | Matched: one thickness for the whole sequence |
| Rupture sets (Table H-5.5-1, joint magnitudes) | Reference magnitudes, one per set in each member, same weights | Matched: rupture set i uses each member's i-th magnitude. The joint distribution is kept |
| Recurrence (Poisson / BPT) and rate tables | Activity-rate branches, identical in each member | Matched. Linear, so exact either way |
| GMM | Run file, attenuation type | Matched |
| Alternative geometries (BA-BL/BA-BFZ, NMN-S/L, RFT-S/L) | **Segmentation models** of the cluster's fault system. Six segments, 2 × 2 × 2 = 8 models with product weights, flags selecting one geometry per fault | Combination within each segmentation model, weighted by its weight |

With segmentation models, P_c,s and s_s are computed for each model s from the members it includes. Member k's factor in pass 2 is Σ over the models containing k of w_s · s_s(z | b), divided by `segWt1(k)` (the sum of those w_s), because pass 2 multiplies every contribution of a segment by `segWt1` as usual. On every branch the members' contributions then add up to α · ProbAct · Σ_s w_s P_c,s. With one model of weight 1 the factor is s itself, so a single-model cluster gives the same out2 to out7 as in Phase 2.

## A9. Out-of-cluster alternatives (Phase 6)

HID Figure H-5.5-1 has three mutually exclusive top-level branches. They map onto segmentation models of a single NMFS fault system:

| Branch | Weight | Segmentation models |
|---|---|---|
| All In | 0.9 | The eight geometry combinations of the cluster members (A8), weights 0.9 × geometry weight |
| All Out Except RFT | 0.05 | An ordinary type 1 RFT segment with the Table H-5.5-6 rates. Two models (RFT-S, RFT-L), weights 0.05 × 0.7 and 0.05 × 0.3 |
| All Out | 0.05 | One model with no segments flagged |

Prob Activity of the system is then 1.0.

This needs a fault system to hold both cluster members and ordinary segments, which Phase 6 allows (A3.2). Within a segmentation model:
- the flagged members combine as a cluster;
- the flagged ordinary segments add as usual.

With this design, both the HAZ mean (through `segWt1`) and the fractile program (through its segmentation-model draw) already treat the three branches as mutually exclusive.

## A10. Differences from the other implementation (for the Phase 6 comparison)

1. **Order of averaging.** Each fault's curve is averaged over all branches (geometry, magnitude, GMM) before the nonlinear combination. This gives the approximation described in A2.3.
2. **Magnitudes.** The joint rupture sets are broken up into marginal per-fault magnitude distributions, which loses the correlation between members.
3. **Crustal thickness.** Only 15 km is used. Their comment cites the HID text and the AMS–BYoungs emails over the original Figure H-5.5-1. The June 2012 correction of the figure now agrees with the text: 13 / 15 / 17 km at 0.3 / 0.5 / 0.2.
4. **Stale value.** In `nmfs_cluster()`, when a fault's curve is 0 at a ground-motion level, its probability keeps the previous level's value instead of being reset.
5. **Missing routines.** Their disaggregation and fractile routines were not provided. The code comment says the non-NGA-East fractiles are "not done yet".

### A10b. The USGS implementation

Code: nshmp-haz, `Transforms.ClusterGroundMotionsToCurves` and `ExceedanceModel.clusterExceedance`. Model: the USGS 2014 model's "SSCn New Madrid" input, marked as an example for review.

**What it does the same way as this spec**
- 1 − Π(1 − P_k) on each GMM branch, including each NGA-East sub-branch, before weighting.
- Each geometry combination is a separate cluster that is combined separately. There are eight, with weights 0.294 … 0.036, the products of 0.6/0.4, 0.7/0.3 and 0.7/0.3.
- Disaggregation by proportional scaling (A5).

**Differences from this spec and the HID**
- **Magnitudes.** Each member lists the six rupture-set magnitudes, with the set weights, as magnitude variants. These are summed within each fault before the combination. That gives marginal per-fault magnitude distributions, as in the other code, and the joint rupture-set correlation is lost. There is no ±0.25 aleatory spread.
- **Rate.** One rate, 0.002657 /yr: the Poisson in-cluster mean of Table H-5.5-2. No BPT branches, and no All In weight of 0.9 in this file. (The whole set has weight 0.45 in the USGS tree.)
- **Thickness.** One thickness: width 15 km, depth 10 km.
- **Mechanism.** RFT has dip 40° but rake 0°.
- **Fractiles.** None.

## A11. Not supported

- Clusters with source types other than 1-style geometry. A type 5 (custom geometry) member could be added later if needed.
- PC method, synchronous rupture and source type 7 combined with clusters.
- Partial participation, where a member ruptures only in some sequences. The aleatory segment weight could later carry this as P(member occurs | sequence), but it is fixed at 1.0 for now.

---

# Part B: Implementation plan

Each phase ends with a review stop:
- I show you the test results, and nothing is pushed until you approve.
- Every phase also re-runs the existing regression suite (974 output files), which must be byte-identical to before.
- A cluster changes nothing unless a fault file uses type 8.

### Phase 0: agree on the spec and build the reference tools (no Fortran changes)

- Settle the open decisions in Part C.
- Build a Python reference tool that:
  - reproduces your spreadsheet;
  - takes HAZ results for each member run alone (as an ordinary type 1 source with rate 1, which makes HAZ's output equal P_k) and computes the cluster hazard and the allocated disaggregation from them.
- Build the test inputs:
  - **T-A:** three members that each have only one rupture: a single magnitude, and a fault short enough that the rupture fills it. This is a direct check against the spreadsheet formula. HAZ's run option 1 gives the medians and σ.
  - **T-B:** the NMS / RFT / NMN traces with boxcar magnitudes, two sites (one near, one at about 350 km) and two GMMs.

### Phase 1: reader

- Change `S29_Rd_Fault_Data_45_4`:
  - accept type 8;
  - apply the validation rules in A3.2;
  - store a cluster index;
  - map the geometry to type 1.
- The hazard calculation stops with "source type 8: not yet implemented". It must not quietly run the members as independent sources.
- Tests:
  - T-A and T-B are read correctly; checked against an echo in the log file.
  - Every validation rule stops the run with its message.
  - Regression is unchanged.

### Phase 2: mean hazard

- Add the two-pass loop, the allocatable Pmem array, the combination and the pass 2 rate.
- Tests:
  - **T-A:** matches the spreadsheet formula with HAZ's medians.
  - **T-B:** HAZ cluster run versus the reference tool applied to the members run alone. Checked per GMM, so the combination on each GMM branch is tested.
  - **One-member cluster:** identical to an ordinary type 1 source with the same rate.
  - **Limits:** the cluster tends to α at low z, and to the independent sum at high z.
  - Regression is unchanged.

### Phase 3: disaggregation

Mostly verification and documentation. The allocation is already in place from Phase 2.

- **out3:** per-member shares add up to the total.
- **out4:** the M–R–ε bins and M̄/D̄/ε̄ match α·s(z)·Σ_k B_k(z) from the members run alone, where B_k is member k's own bins.
- **out7:** checked the same way.
- Add source type 8 and the disaggregation rule to the HAZ notes.

### Phase 4: fractiles (ChristieHale/Fractiles)

- First, with your agreement, make the fractile repository's two test cases run again (A7). They are the baseline for this phase.
- Accept type 8 in `rd_flt.f`. HAZ writes out1 as F1 (F3 from Phase 5).
- Tests:
  - the fractiles' mean converges to HAZ's mean;
  - with two GMMs weighted 0.5 / 0.5, the fractiles of a cluster-only run match the exact two-point distribution;
  - the existing fractile tests are unchanged.

### Phase 5: epistemic branches

- Lift the single-branch restriction: thickness, dip, rate, magnitude sets and mechanism, matched by index.
- Add segmentation models for alternative geometries (A8).
- Check memory and run time at the full NMFS branch counts.
- Tests:
  - every branch against the reference tool, using out1 from the members run alone;
  - the size of the exact-versus-approximation difference.

### Phase 6: the full NMFS model

- Fault-file inputs for the HID logic tree:
  - the in-cluster and out-of-cluster branches as segmentation models (A9);
  - BPT rates as alternative rate branches (HID Tables H-5.5-2 to H-5.5-6);
  - thickness weights 0.3 / 0.5 / 0.2 (June 2012 correction).
- Fractile changes for clusters with several segmentation models (A7).
- Comparison with the other code, explaining differences with A10.
- Final documentation.

---

# Part C: Decisions

Agreed on November 22, 2026 (5 on November 23):
1. **Disaggregation:** proportional allocation (A5).
2. **out1 for fractiles:** F1, the cluster's curves in the first member's block (A7). Replaced on November 23, 2026 by F3 (extra blocks after the members, one set per segmentation model), needed for alternative geometries.
3. **Rate input:** the sequence rate repeated in each member's activity-rate lines (A3.1).
4. **Fractile repository:** fix the problems listed in A7 as part of Phase 4.

5. **UHS_deagg:** the companion program is in scope. It is in ChristieHale/UHS_Deagg, branch `compatibility_issues`, which reads out3 and out4 from HAZ 45.2 to 45.4 and has a reference test of a cluster (HAZ test B). out3 and out4 keep their format, so clusters need no other change there.

# Status

- **Phase 0 (done):**
  - `scripts/cluster_reference.py` and `scripts/test_cluster_reference.py`.
  - The member runs in `tests/cluster/` (see its README).
  - A fix to `scripts/io_tools.py` `read_out3` for out3 files with more than one period.
- **Phase 1 (done):**
  - The 45.4 reader accepts source type 8 and checks the rules in A3.2 (`src/29_readFlt.f`, `src/36_Cluster.f`). Each member is stored as source type 1 with a cluster number (`clusterID`). The older readers stop on type 8.
  - HAZ writes a summary of the clusters to the screen and log file, then stops before the hazard calculation.
  - The Cluster runs of tests A and B (`tests/cluster/*/Cluster`), written by `tests/cluster/make_inputs.py`. The test runner skips them until Phase 2 adds their reference outputs.
  - `scripts/test_cluster_reader.py` runs HAZ on them and on one variant per rule.
  - There is no fixed limit on the number of members; the Phase 2 storage will be sized when the cluster is reached.
- **Phase 2 (done):**
  - The two-pass loop over a cluster's members is in `src/01_HAZ_MAIN2.f`; the sizing, the combination and the out1 curves are in `src/36_Cluster.f`.
  - The storage (`Pmem`, `Pclu`, `sClu`) is allocated when the first member is reached, from the cluster's numbers of members, levels, periods, GMMs and branches, and released after pass 2.
  - P_c is built up one member at a time as P_c + P_k (1 − P_c). This keeps small probabilities without the round-off of 1 − Π(1 − P_k).
  - out1 follows F1 (A7), written from Phase 2 on so that out1 is never in an interim form.
  - `scripts/test_cluster_hazard.py` has the Phase 2 tests. The Cluster runs of tests A and B now have reference outputs, and the test runner no longer skips them.
  - Two log-file formats in `src/21_FRISK_IO.f` lacked a comma (`2xi8`, `2xa80`). gfortran 16 stops on them at run time, so they were fixed first, in a separate commit; the output is unchanged.
- **Phase 3 (done):**
  - No Fortran changes: the allocation is in place from Phase 2.
  - `scripts/test_cluster_deagg.py` runs the Cluster inputs of tests A and B, and Members runs of test B with each GMM on its own, and checks against the reference tool:
    - out3: each member's share, Σ_g w_g α s_g P_kg;
    - out4: the M–R and M–R–ε bins and M̄, D̄, ε̄ (`cluster_reference.cluster_deagg`);
    - out7: each member's means, weighted by w_g s_g P_kg (`cluster_reference.cluster_source_deagg`, with a new out7 reader).
  - The comparisons allow for the rounding of the printed numbers: out4 has 3 significant digits, and a reference value built from an input such as 0.103 can be off by half a unit of that input's last digit.
  - With the allocation factor left out of the reference (s = 1), the out3, out4 and out7 checks of test B fail, so the tests distinguish the allocation from independent members.
  - Source type 8 and the disaggregation rule are in `docs/HAZ-45.4 Notes.docx` (Fault File Notes, Line 8), with a note on UHS_deagg in the UHS section.
- **Phase 4 (done)**, in ChristieHale/Fractiles, branch `compatibility_issues` (605e75b):
  - The repository's two tests run again: the PC flag is added to their HAZ run files, and their outputs are regenerated with HAZ 45.4. The fractiles agree with the old outputs to within 6e-4.
  - The hazard array is sized from the run. At the maximum dimensions it needed about 5.7 GB of static storage, which cannot be linked on arm64.
  - Source types 6 and 7 are read as in HAZ (A7), and source type 8 is read as type 1. HAZ writes out1 as F1, so the cluster is sampled as one source.
  - Three problems found on the way are fixed: the last period's test ground motions were used for every period; out1 blocks were read with the selected period's numbers of GMMs and levels; and a fractile curve that did not cross a hazard level gave a UHS of zero (now a negative value that says where the curve ran out).
  - New tests: `Cluster_Test` (test B, two GMMs at 0.5 / 0.5) checks the two-point distribution of the fractiles, the mean against the average of the two curves, and the 5th and 95th UHS values. `Source_Types_Test` (types 1, 6 and 7) checks that every fractile equals HAZ's total when there is a single branch. `scripts/test_reference.py` runs all four tests and, with `HAZ_BIN`, reruns their hazard cases.
- **Phase 5, HAZ (done):**
  - Reader (A3.2, A8): the single-branch and single-model rules are lifted. Every member of a cluster must have the same numbers and weights of thickness-and-dip, rate-and-magnitude and mechanism branches, and the same rates; magnitudes may differ, so a branch can hold a joint rupture set. Every segmentation model must include a member, and every member must be in a model.
  - Combination for each segmentation model and the pass 2 factor as in A8 (`S36_ClusterCombine`); out1 as F3 (`S36_ClusterWriteOut1`).
  - Pass 1 no longer clears the out1 and out5 work arrays, and pass 2 adds nothing to out1's; clearing them (at HAZ's maximum dimensions, about 200 MB) took about 90% of a small run, so a cluster now costs about the same time as its members run alone.
  - Test C (`tests/cluster/TC_Geometries`): the six NMFS traces, eight segmentation models (weights 0.6/0.4 × 0.7/0.3 × 0.7/0.3), three thicknesses (0.3/0.5/0.2), two rates × rupture sets 1 and 5, two mechanisms and two GMMs. `scripts/test_cluster_branches.py` checks every out1 block (each model, width and branch), the out3 total and each member's share against the reference tool, and that the averaged approximation (A2.3) is never below the exact result.
  - Size of the approximation in test C: up to about 15% above the exact rate at intermediate ground motions (for example 1.15 at 0.07 g, 1.0 s, Memphis), and equal at low and high ground motions. Treating the members as independent gives up to 3 times the exact rate at low ground motions.
  - Storage at the full NMFS counts (6 members, 18 levels, 16 periods, 17 GMMs, 120 rate × magnitude branches, 3 thicknesses, 8 models) is about 280 MB; with 7 periods, about 120 MB.
  - The single-model clusters of tests A and B give the same out2 to out7 as before; only their out1 changes (F3). Regression otherwise byte-identical.
- **Phase 5, Fractiles (done)**, in ChristieHale/Fractiles, branch `compatibility_issues` (a1d8052):
  - The reader records the members of clusters, and `Set_Cluster_Sources` numbers each cluster's segmentation models as extra sources after the last source (as in F3), with the first member's branches and weights, and sets the order of the sources in out1.
  - A cluster is sampled as one source: a segmentation model, then one width, rate-and-magnitude and mechanism branch, and that model's curve. out1 from HAZ before F3 stops with a message.
  - `Cluster_Test` (test B) is regenerated with F3. The new `Cluster_Geometries_Test` (test C) checks every fractile against the exact quantiles of the 384 branch curves (within 4 standard errors) and the mean against the exact mean.
- **Phase 6, step 1: in- and out-of-cluster branches in one fault system (A9), HAZ:**
  - A fault system may hold cluster members (type 8, consecutive segments) and ordinary segments. A segmentation model may include members, ordinary segments, both, or nothing (the "All Out" branch). Every member must be in a model.
  - Nothing else changes in the calculation. A model with no member has P_c = 0; HAZ writes zero curves for it in out1, and it does not enter any member's pass 2 factor. Ordinary segments are integrated as usual, with their own segmentation weight.
  - In out1 the cluster's blocks (one set per model) follow the last member, before any ordinary segment that comes after it.
  - Test D (`tests/cluster/TD_InOut`): test C's cluster plus RFT-S and RFT-L as ordinary segments with out-of-cluster rates, in eleven models as in HID Figure H-5.5-1 (8 × 0.9 × geometry weight, 0.05 × 0.7 / 0.3 for RFT alone, 0.05 for no activity). `scripts/test_cluster_inout.py` checks out1 (members, every model's cluster blocks, ordinary segments) and the out3 total and every source's share against the reference tool.
  - `MAX_ATTEN` is raised from 100 to 120, for the 102 GMMs of the CEUS rev23 run file (the rev22 and rev23 runs used a build with larger limits).
- **Phase 4/5 follow-up, Fractiles (done)**, ChristieHale/Fractiles `compatibility_issues` (a2302d7): fault systems with a cluster and ordinary segments (draw the model, add the cluster's curve for it and the ordinary segments it includes); `MAX_ATTEN` 120; `Cluster_InOut_Test` (test D) checked against its exact distribution.
- **Phase 6, step 2: the NMFS model and the Appendix I comparison:**
  - `scripts/make_nmfs_source.py` writes the NMFS RLME source (HAZ 45.4) from NMFS_RLME.xls (CEUS SSC HID database: traces, Table H-5.5-1 rupture sets, Tables H-5.5-2 to H-5.5-6 rates) and the weights of HID Figure H-5.5-1 (June 2012): one fault system, eight segments (six cluster members and RFT-S and RFT-L out of cluster), eleven segmentation models; 20 rate branches (Poisson 0.75; BPT 0.25 with alpha 0.3 / 0.5 / 0.7) × 6 rupture sets, thickness 13 / 15 / 17 km, boxcar ±0.25. Rupture scaling and steps as the rev23 RLME faults (PEER Report Eq. 13-8 and 13-9, 1 km steps). With `--run`, it also writes one run file per site from a given run file.
  - Sheet BA-BL of NMFS_RLME.xls gives 3 points but lists 4 (the 4th is the start of NMN); all 4 are used, with a warning.
  - Runs with the rev23 run file (102 NGA-East GMMs, PGA, 0.1 s, 1.0 s, seven demonstration sites): about 7 minutes and 555 MB a site. The NMFS rate at the lowest level, 2.098e-3 /yr, equals 0.9 × the mean in-cluster rate + 0.05 × the mean out-of-cluster RFT rate.
  - Adding the NMFS to the rev22 (distributed) + rev23 (other RLMEs) totals, the ratio to the Appendix I mean between 0.01 and 0.5 g goes from 0.14–0.78 to 0.96–1.16 at Jackson, 0.20–1.01 to 0.99–1.12 at Central Illinois, 0.28–0.99 to 0.84–1.00 at Chattanooga and 0.19–0.86 to 0.73–1.02 at Topeka. Savannah (0.75–0.97) and Manchester (0.57–0.94, beyond 1000 km) stay low at high ground motions, pointing to the distributed sources. Houston goes to 0.91–2.87, most likely because the Gulf Coast adjustment is not applied (on the issues list).
  - Still to do: the size of the averaged approximation and of the independent-members treatment for the NMFS (needs an NMFS members run); the comparison with the other code (A10).

