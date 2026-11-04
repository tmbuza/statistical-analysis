# Statistical Analysis

A free CDI Quarto reference guide. Planned URL: https://statistical.complexdatainsights.com

## Status

Development edition: Chapters 00–23 plus appendices have been drafted with synthetic examples and companion scripts. R execution, generated R figures, and complete Quarto rendering still require validation in the target environment.

## Quick start

Install Quarto separately. From this folder:

```bash
bash scripts/bash/01-check-project.sh
quarto preview
```

Chapters currently contain no executable analysis chunks, so preview/render needs Quarto but does not require R or Python packages.

## Install Python requirements

Use Python 3.11 or newer. Run these commands from the `statistical-analysis` repository root, where `requirements.txt` is located:

```bash
python3 -m venv .venv
source .venv/bin/activate
python -m pip install -r requirements.txt
```

If `.venv` already exists, skip its creation. If your prompt already shows `(.venv)`, install or update the requirements directly:

```bash
python -m pip install -r requirements.txt
```

Required third-party packages:

| Package | Used for |
|---|---|
| NumPy | Numerical arrays, summaries, and calculations |
| SciPy | Statistical functions and probability distributions |
| Matplotlib | Diagnostic plots and saved figures |

Modules such as `csv` and `pathlib` are included with Python and do not need installation.

Check that the packages import in the active environment:

```bash
python -c "import numpy, scipy, matplotlib; print('Python requirements are available.')"
```

## Run Python examples

With the environment active, run the Chapter 03 diagnostic example:

```bash
python scripts/python/03-check-data.py
```

If you retained the original scaffold's Python Welch example, run it separately:

```bash
python scripts/python/05-independent-t-test.py
```

## Run the R example

Install R, then run from the project root:

```bash
Rscript scripts/R/05-independent-t-test.R
```

The developed R examples use base R only. They export results under `results/tables/` and figures under `results/figures/` where applicable. Check the comparison direction in each script: the developed Chapter 05 R script reports A minus B; an older scaffold Python script may use a different convention.

## Run all developed examples

With R installed and the Python environment active, run from the repository root:

```bash
bash scripts/bash/run-all-examples.sh
```

Keep this wrapper in `scripts/bash/`. It runs Chapter 03's Python script and the R scripts in `scripts/R/`. It writes or overwrites teaching outputs; Chapter 03 also regenerates its fixed synthetic input. Preserve original research data separately.

Run scripts before inspecting figures or embedding generated images in the chapters. Rendering Quarto does not run these scripts.

## Render and upload

```bash
bash scripts/bash/02-render-guide.sh
```

Upload the **contents** of `docs/` to the document root assigned to `statistical.complexdatainsights.com`. The `site-url` setting does not create the subdomain or configure DNS. No site has been deployed by this scaffold.

## Development order

1. Run the examples and resolve errors or warnings.
2. Check exported tables against chapter values and inspect generated plots.
3. Add image references where figures should appear in chapters.
4. Render the assembled book and review references, navigation, and layout.
5. Publish after content and reproducibility review.

Keep code visible through “Show code” controls, place interpretation beside results, and document assumptions and limitations.

## Licensing

Add explicit content and code licenses before public release. No license is inferred by this scaffold.
