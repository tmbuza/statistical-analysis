# Statistical Analysis

A free CDI Quarto reference guide. Planned URL: https://statistical.complexdatainsights.com

## Status

Scaffold edition: 00–23 plus appendices. Chapter 05 includes a runnable Welch t-test starter; the remaining method chapters are outlines. This package is not a completed or reviewed textbook.

## Quick start

Install Quarto separately. From this folder:

```bash
bash scripts/bash/01-check-project.sh
quarto preview
```

Chapters currently contain no executable analysis chunks, so preview/render needs Quarto but does not require R or Python packages.

## Run the Python example

```bash
bash scripts/bash/00-setup-python.sh
source .venv/bin/activate
python scripts/python/05-independent-t-test.py
```

## Run the R example

Install R, then run from the project root:

```bash
Rscript scripts/R/05-independent-t-test.R
```

The R example uses base R only. Both implementations export B-minus-A results under `results/tables/`.

## Render and upload

```bash
bash scripts/bash/02-render-guide.sh
```

Upload the **contents** of `docs/` to the document root assigned to `statistical.complexdatainsights.com`. The `site-url` setting does not create the subdomain or configure DNS. No site has been deployed by this scaffold.

## Development order

1. Complete chapters 01–03 and the method chooser.
2. Expand and review chapter 05 as the chapter template.
3. Develop the remaining methods with small datasets and checked results.
4. Add authoritative references and reproducibility records.
5. Review content and navigation, then publish.

Keep code visible through “Show code” controls, place interpretation beside results, and document assumptions and limitations.

## Licensing

Add explicit content and code licenses before public release. No license is inferred by this scaffold.
