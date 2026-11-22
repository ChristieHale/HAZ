"""Tests of how HAZ reads clustered sequences (source type 8).

These run the HAZ executable on the test A cluster input and on variants of
it that break one rule each. Set HAZ_BIN to the executable; the tests are
skipped when it cannot be found. Run with pytest from scripts/.
"""

import os
import shutil
import subprocess

import pytest

HERE = os.path.dirname(os.path.abspath(__file__))
CASES = os.path.join(HERE, "..", "tests", "cluster")
TA = os.path.join(CASES, "TA_SingleRupture", "Cluster", "Input")
TB = os.path.join(CASES, "TB_NMFS", "Cluster", "Input")

HAZ_BIN = os.environ.get("HAZ_BIN", os.path.join(HERE, "..", "build", "HAZ"))
pytestmark = pytest.mark.skipif(not os.path.isfile(HAZ_BIN), reason="set HAZ_BIN to the HAZ executable")


def run_haz(folder, run_name):
    p = subprocess.run(
        [os.path.abspath(HAZ_BIN)],
        cwd=folder,
        input=f"0\n0\n{run_name}\n".encode(),
        capture_output=True,
        timeout=300,
    )
    return p.returncode, p.stdout.decode(errors="replace") + p.stderr.decode(errors="replace")


def copy_case(src, dst):
    shutil.copytree(src, dst)
    return dst


def replace_nth(text, old, new, n):
    """Replace the n-th (1-based) occurrence of old."""
    i = -1
    for _ in range(n):
        i = text.index(old, i + 1)
    return text[:i] + new + text[i + len(old):]


def edit(path, *changes):
    with open(path) as f:
        text = f.read()
    for old, new, n in changes:
        assert text.count(old) >= n, old
        text = replace_nth(text, old, new, n)
    with open(path, "w") as f:
        f.write(text)


SOURCE_8 = "8  1  1  1.0  0  0 "
NOT_YET = "for clusters is not implemented yet"


@pytest.mark.parametrize("case, run_name, names", [
    (TA, "Run_TA_Cluster.txt", ["TA_NMS_BA-BL", "TA_RFT_S", "TA_NMN_S"]),
    (TB, "Run_TB_Cluster.txt", ["TB_NMS_BA-BL", "TB_RFT_S", "TB_NMN_S"]),
])
def test_valid_cluster_is_read(tmp_path, case, run_name, names):
    folder = copy_case(case, tmp_path / "case")
    code, out = run_haz(folder, run_name)
    assert code == 99
    assert NOT_YET in out
    assert "Clustered earthquake sequences (source type 8):    1" in out
    assert "Rate of the sequence (/yr):  0.26570E-02" in out
    for name in names:
        assert name in out
    with open(os.path.join(folder, run_name + ".log"), errors="replace") as f:
        assert NOT_YET in f.read()


ERROR_CASES = {
    "mixed source types (second segment type 1)": (
        [(SOURCE_8, "1  1  1  1.0  0  0 ", 2)],
        "if one segment of a fault system is source type 8, all must be"),
    "mixed source types (first segment type 1)": (
        [(SOURCE_8, "1  1  1  1.0  0  0 ", 1)],
        "if one segment of a fault system is source type 8, all must be"),
    "two segmentation models": (
        [("1                           Number of seg models\n1.0                           wts for seg models\n"
          "3                           Number of segments\n1 1 1 ",
          "2                           Number of seg models\n0.5 0.5\n"
          "3                           Number of segments\n1 1 1\n1 1 1 ", 1)],
        "needs one segmentation"),
    "segment left out of the segmentation model": (
        [("1 1 1                             flags", "1 0 1                             flags", 1)],
        "needs one segmentation"),
    "aleatory segment weight": (
        [("1.0                         aleatory seg wt", "0.5                         aleatory seg wt", 2)],
        "aleatory segment weight of a cluster member"),
    "synchronous rupture": (
        [(SOURCE_8 + "         source type", "8  1  1  1.0  0  1          source type", 1),
         ("1.0                         aleatory seg wt",
          "1  -999\n7.0  10.0  10.0  10.0  0  8.0  0.0  90.0  0.0  15.0  0.0  0.0  1.0\n"
          "1.0                         aleatory seg wt", 1)],
        "synchronous rupture is not allowed"),
    "slip rates": (
        [("0.0  1.0  0.0  0.0          wts for sr", "0.5  0.5  0.0  0.0          wts for sr", 1),
         ("0                           Number of slip-rates", "1\n1.0\n1.0", 1),
         # HAZ makes no slip-rate branch without b-values
         ("0                           Number of b-values", "1\n1.0\n1.0", 1)],
        "takes activity rates only"),
    "activity-rate branch weight below 1": (
        [("0.0  1.0  0.0  0.0          wts for sr", "0.5  0.5  0.0  0.0          wts for sr", 1)],
        "activity-rate branch weight of a cluster member"),
    "different rates": (
        [("2.65702e-03", "3.00000e-03", 3)],
        "same\n  activity rates and weights"),
    "different atten types": (
        [(SOURCE_8, "8  1  2  1.0  0  0 ", 2)],
        "must have the same atten type"),
    "two thicknesses": (
        [("1                           Number of fault widths\n15.0                          seismogenic thickness (km)\n"
          "1.0                           wts for fault widths\n",
          "2                           Number of fault widths\n13.0  17.0\n0.5  0.5\n", 1),
         ("1                           Number of ref mags\n7.90                          ref mag\n"
          "1.0                           wts for ref mags\n",
          "1\n7.90\n1.0\n1\n7.90\n1.0\n", 1)],
        "must have a single"),
    "two reference magnitudes": (
        [("1                           Number of ref mags\n7.90                          ref mag\n"
          "1.0                           wts for ref mags\n", "2\n7.90  7.80\n0.5  0.5\n", 1)],
        "must have a single"),
    "two mechanisms": (
        [("1                           Number of fault mech\n0.0                           fault mech\n"
          "1.0                           wts for fault mech\n", "2\n0.0  1.0\n0.5  0.5\n", 1)],
        "must have a single"),
}


@pytest.mark.parametrize("label", sorted(ERROR_CASES))
def test_rule(tmp_path, label):
    changes, message = ERROR_CASES[label]
    folder = copy_case(TA, tmp_path / "case")
    edit(os.path.join(folder, "Source_TA_Cluster.txt"), *changes)
    code, out = run_haz(folder, "Run_TA_Cluster.txt")
    assert code == 99
    assert message in out
    assert NOT_YET not in out


def test_pc_method(tmp_path):
    folder = copy_case(TA, tmp_path / "case")
    edit(os.path.join(folder, "Run_TA_Cluster.txt"),
         ("0.0  8  0  0                 period", "0.0  8  0  1                 period", 1))
    code, out = run_haz(folder, "Run_TA_Cluster.txt")
    assert code == 99
    assert "the PC method is not available with clustered sequences" in out


def test_needs_45_4_format(tmp_path):
    folder = copy_case(TA, tmp_path / "case")
    edit(os.path.join(folder, "Run_TA_Cluster.txt"),
         ("45.4                        version", "45.3                        version", 1))
    # The 45.3 format has no max distance type on the source type line
    edit(os.path.join(folder, "Source_TA_Cluster.txt"),
         *[(SOURCE_8, "8  1  1.0  0  0 ", 1)] * 3)
    code, out = run_haz(folder, "Run_TA_Cluster.txt")
    assert code == 99
    assert "source type 8 (clustered sequence) needs the 45.4 fault file format" in out
