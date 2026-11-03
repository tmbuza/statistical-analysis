"""Welch comparison, B minus A. Run from any directory."""
from pathlib import Path
import csv
import numpy as np
from scipy import stats

ROOT = Path(__file__).resolve().parents[2]
with (ROOT / "data/example/independent-groups.csv").open() as handle:
    rows = list(csv.DictReader(handle))
a = np.array([float(row["score"]) for row in rows if row["group"] == "A"])
b = np.array([float(row["score"]) for row in rows if row["group"] == "B"])
if min(len(a), len(b)) < 2 or not np.isfinite(np.r_[a, b]).all():
    raise ValueError("Each group needs at least two finite scores.")
result = stats.ttest_ind(b, a, equal_var=False)
ci = result.confidence_interval(confidence_level=0.95)
record = dict(n_A=len(a), n_B=len(b), mean_A=a.mean(), mean_B=b.mean(),
              difference_B_minus_A=b.mean()-a.mean(), ci_low=ci.low,
              ci_high=ci.high, t=result.statistic, df=result.df, p_value=result.pvalue)
out = ROOT / "results/tables/05-welch-python.tsv"
out.parent.mkdir(parents=True, exist_ok=True)
with out.open("w", newline="") as handle:
    writer = csv.DictWriter(handle, fieldnames=record, delimiter="\t")
    writer.writeheader()
    writer.writerow(record)
print(out)
print(record)
