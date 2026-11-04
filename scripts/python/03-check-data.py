"""Run from the repository root: python scripts/python/03-check-data.py."""
from pathlib import Path
import csv
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from scipy import stats

root = Path(__file__).resolve().parents[2]
data_dir = root / 'data/example'
figure_dir = root / 'results/figures'
table_dir = root / 'results/tables'
for folder in (data_dir, figure_dir, table_dir):
    folder.mkdir(parents=True, exist_ok=True)

# Fixed illustrative values make every run reproduce the same synthetic data.
a = [55, 59, 61, 63, 64, 65, 66, 67, 68, 69, 70, 71,
     72, 73, 74, 75, 76, 77, 79, 82, None]
b = [48, 54, 58, 60, 63, 65, 67, 69, 71, 73, 75, 77,
     79, 81, 83, 85, 88, 91, 95, 100, None]
rows = []
for group, values in [('A', a), ('B', b)]:
    for number, value in enumerate(values, start=1):
        rows.append({'learner_id': f'{group}{number:02d}',
                     'approach': group, 'score': '' if value is None else value})
with (data_dir / 'training-scores.csv').open('w', newline='') as handle:
    writer = csv.DictWriter(handle, fieldnames=['learner_id', 'approach', 'score'])
    writer.writeheader()
    writer.writerows(rows)

# Re-read the saved input and check structure before producing summaries.
with (data_dir / 'training-scores.csv').open(newline='') as handle:
    observed = list(csv.DictReader(handle))
assert len({r['learner_id'] for r in observed}) == len(observed)
assert {r['approach'] for r in observed} == {'A', 'B'}
scores = {}
summary = []
for group in ['A', 'B']:
    subset = [r for r in observed if r['approach'] == group]
    values = np.array([float(r['score']) for r in subset if r['score'] != ''])
    assert np.isfinite(values).all() and ((values >= 0) & (values <= 100)).all()
    scores[group] = values
    summary.append({'approach': group, 'rows': len(subset),
                    'observed': len(values), 'missing': len(subset) - len(values),
                    'mean': values.mean(), 'sd': values.std(ddof=1),
                    'median': np.median(values), 'minimum': values.min(),
                    'maximum': values.max()})
with (table_dir / 'training-score-checks.csv').open('w', newline='') as handle:
    writer = csv.DictWriter(handle, fieldnames=list(summary[0]))
    writer.writeheader()
    writer.writerows(summary)

plt.rcParams.update({'font.size': 11, 'axes.spines.top': False,
                     'axes.spines.right': False, 'figure.facecolor': 'white'})
colors = ['#087f8c', '#b86e13']
fig, axes = plt.subplots(1, 3, figsize=(13, 4.3))
# Deterministic horizontal spread exposes observations without implying another variable.
rng = np.random.default_rng(303)
for position, (group, values) in enumerate(scores.items(), start=1):
    jitter = rng.uniform(-0.10, 0.10, len(values))
    axes[0].scatter(position + jitter, values, color=colors[position-1], s=27)
axes[0].set(xticks=[1, 2], xticklabels=['A', 'B'], xlabel='Teaching approach',
            ylabel='Assessment score (points)', title='Observed scores', ylim=(0, 105))
for axis, (group, values), color in zip(axes[1:], scores.items(), colors):
    (theoretical, ordered), (slope, intercept, _) = stats.probplot(values, dist='norm')
    axis.scatter(theoretical, ordered, color=color, s=27)
    axis.plot(theoretical, intercept + slope * theoretical, color='#243746', lw=1.3)
    axis.set(xlabel='Theoretical normal quantile', ylabel='Ordered score (points)',
             title=f'Normal Q-Q plot: Approach {group}')
fig.suptitle('Check distributions within each group', fontweight='bold')
fig.tight_layout(rect=(0, 0, 1, .93))
fig.savefig(figure_dir / 'training-score-diagnostics.png', dpi=180)
plt.close(fig)
for row in summary:
    print(f"{row['approach']}: observed={row['observed']}, missing={row['missing']}, "
          f"mean={row['mean']:.2f}, SD={row['sd']:.2f}")
