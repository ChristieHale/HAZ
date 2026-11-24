"""Tests of the boxcar magnitude pdf (magnitude model 8) with each rate type.

A single vertical fault with a boxcar of width 0.5 centred on M 7.0, with the
magnitude bins aligned with the boxcar (minimum magnitude 6.75, step 0.1), so
that every bin is inside it. From the out2 magnitude rates:

- slip rate: the moment rate of the out2 rates, sum of rate(m) 10**(1.5 m + 16.05),
  equals slip rate x fault area x rigidity (HAZ: 3e11 dyne/cm2), within the
  discretisation of the moment over the 0.1 magnitude bins;
- recurrence interval: the total rate is 1 / interval;
- activity rate: the total rate is the given rate.

Set HAZ_BIN to the executable; skipped when it cannot be found.
"""

import os
import re
import subprocess

import numpy as np
import pytest

HERE = os.path.dirname(os.path.abspath(__file__))
HAZ_BIN = os.environ.get("HAZ_BIN", os.path.join(HERE, "..", "build", "HAZ"))
pytestmark = pytest.mark.skipif(not os.path.isfile(HAZ_BIN), reason="set HAZ_BIN to the HAZ executable")

RIGIDITY = 3.0e11

RATES = {
    "slip rate": ("1.0  0.0  0.0  0.0", "0", "1\n2.0\n1.0", "0", 2.0),
    "recurrence interval": ("0.0  0.0  1.0  0.0", "0", "0", "1\n500.0\n1.0", 500.0),
    "activity rate": ("0.0  1.0  0.0  0.0", "1\n0.9  2.0e-3  1.0", "0", "0", 2.0e-3),
}


def fault_file(weights, act, sr, ri):
    return f"""1                           iCoor
1                           Number of fault systems
Boxcar test
1.0                         Prob Activity
1                           Number of seg models
1.0                           wts for seg models
1                           Number of segments
1                             flags for seg model
Boxcar_fault
1  1  1  1.0  0  0          source type, max dist type, atten type, sample step, dirflag, synflag
1.0                         aleatory seg wt
90.0  0.0                   dip, depth to top
2                           Number of long/lat pts
-90.00  35.00
-90.00  35.90
1                           Number of dip variations
0.0
1.0
1                           Number of b-values
0.9
1.0
{act}
{weights}          wts for sr, act. rate, rec int, MoRate branches
{sr}
{ri}
0                           Number of moment rates
1                           Number of mag recur models
8                             mag recur model (boxcar)
1.0
0.5  0.0  0.0                 boxcar width
1                           Number of fault widths
15.0
1.0
0  0.0  0.0  0.0            Depth pdf model and params
1                           Number of ref mags
7.0
1.0
6.75  0.1  1.0  1.0  1  1  0.0    minmag, magstep, hxStep, hzStep, nRupArea, nRupWidth, minDepth
-3.49  0.91  0.0            rupArea coeff
-1.01  0.32  0.0            rupWidth coeff
1                           Number of fault mech models
1.0
1                           Number of fault mech
0.0
1.0
"""


RUN = """Source.txt
45.4                        version of fault file
30.0  45.0  -100.0  -80.0
200.  200.
Boxcar rate test
1  1
0.0  3  0  0
3
0.01  0.1  0.5
1
102  0.0  0.0  1.0  0.0  0
0
3
6.5  7.0  7.5
3
0  50  200
3
-10.  0.  10.
4
0.0  0.10  0.40  2.0
0
1
-90.10  35.45  2000.0  0.0  0.0  0.0  0.8  0
Box.out1
Box.out2
Box.out3
Box.out4
Box.out5
Box.out6
Box.out7
"""


def run(tmp_path, case):
    weights, act, sr, ri, _ = RATES[case]
    (tmp_path / "Source.txt").write_text(fault_file(weights, act, sr, ri))
    (tmp_path / "Run.txt").write_text(RUN)
    p = subprocess.run([os.path.abspath(HAZ_BIN)], cwd=tmp_path, input=b"0\n0\nRun.txt\n",
                       capture_output=True, timeout=300)
    out = p.stdout.decode(errors="replace")
    assert p.returncode == 0 and "Normal termination" in out, out[-2000:]
    mags, rates = [], []
    for line in (tmp_path / "Box.out2").read_text().splitlines():
        t = line.split()
        if len(t) == 4 and re.match(r"^-?\d+\.\d{3}$", t[2]):
            mags.append(float(t[2]))
            rates.append(float(t[3]))
    area = float(re.search(r"fault area \(km\^2\) =\s*(\S+)", (tmp_path / "Run.txt.log").read_text()).group(1))
    return np.array(mags), np.array(rates), area


def test_bins_inside_boxcar(tmp_path):
    mags, rates, _ = run(tmp_path, "activity rate")
    np.testing.assert_allclose(mags, [6.8, 6.9, 7.0, 7.1, 7.2])
    np.testing.assert_allclose(rates, 2.0e-3 / 5, rtol=1e-3)


def test_slip_rate_moment_balance(tmp_path):
    mags, rates, area = run(tmp_path, "slip rate")
    # Each 0.1 bin is represented by its centre; the mean of 10**(1.5 m) over a
    # bin is sinh(a)/a times its value at the centre, a = 1.5 ln(10) x 0.05
    a = 1.5 * np.log(10.0) * 0.05
    moment = np.sum(rates * 10.0 ** (1.5 * mags + 16.05)) * np.sinh(a) / a
    expected = 2.0 * area * RIGIDITY * 1.0e9
    # out2 has 4 significant digits
    assert moment == pytest.approx(expected, rel=2e-3)


def test_recurrence_interval(tmp_path):
    mags, rates, _ = run(tmp_path, "recurrence interval")
    assert rates.sum() == pytest.approx(1.0 / 500.0, rel=1e-3)
