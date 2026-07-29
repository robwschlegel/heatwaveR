# Product

<!-- impeccable:product-schema 1 -->

## Platform

web

## Users

Academic marine and climate researchers (oceanographers, marine biologists, climate scientists) who
analyse in-situ or remote-sensed sea-surface or air temperature time series to detect, classify, and
report on marine heatwaves (MHWs) and cold-spells (MCSs), typically as part of peer-reviewed research.

## Product Purpose

`heatwaveR` is an R package that implements the Hobday et al. (2016, 2018) definitions for detecting,
classifying, and visualising heatwaves and cold-spells in daily or hourly temperature time series. It
exists to give the marine/climate research community a fast, well-tested, citable reference
implementation of these community-standard algorithms, succeeding the earlier `RmarineHeatWaves`
(itself a translation of the Python `marineHeatWaves` package). Success means a researcher can build a
seasonal climatology, detect events, compute standardised event metrics, and produce
publication-ready figures whose results are reproducible and consistent with the field's canonical
definitions.

## Positioning

A fast (partially C++-accelerated via Rcpp/RcppArmadillo), actively maintained R implementation of the
Hobday et al. marine heatwave/cold-spell conventions that a from-scratch script could not truthfully
match on either performance or field-standard correctness. It extends beyond the original Python/R
lineage to also support atmospheric heatwaves and provides the first implementation of a definition
for "compound heatwaves." It is the community's citable, canonical tool for this task: JOSS-published,
CRAN-hosted, and cited in well over 100 papers a year (see `CITATIONS.md`).

## Operating Context

Users bring their own daily or even-hourly temperature series (in-situ loggers, buoys, or
gridded/remote-sensed SST products such as OISST) and work through vignette-documented workflows:
`ts2clm()` to build a seasonal climatology/threshold, `detect_event()` to identify events and compute
metrics, `category()` to apply Hobday et al. (2018) severity categories, `block_average()` for annual
summaries, and `geom_flame()` / `geom_lolli()` / `event_line()` / `lolli_plot()` for publication
figures. For gridded/NetCDF workflows, users are pointed to the companion package `heatwave3`.
Documentation lives in a pkgdown site (vignettes, function reference) plus the README, and results
from this workflow feed directly into peer-reviewed research output.

## Capabilities and Constraints

R package (CRAN + GitHub), MIT licensed, requires R >= 3.0.2, with compiled Rcpp/RcppArmadillo code
for the performance-critical climatology calculation. Supports daily and even-hourly (`POSIXct`) time
series with flexible column naming. Two parallel internal implementations exist
(`ts2clm()`/`detect_event()` and the data.table-based `ts2clm3()`/`detect_event3()`) — an internal
engineering detail, not user-facing positioning. Not designed for arbitrary irregular or non-daily
frequencies (e.g. monthly, sub-hourly), and does not yet warn automatically on excessive NA gaps
beyond the user-set `maxPadLength` (both noted as open future-work in
`vignettes/internal_workflow.Rmd`).

## Brand Commitments

None binding. A visual identity already exists on the live pkgdown site (hex-sticker logo, teal/navy
"ocean" palette in `pkgdown/extra.scss`, card-grid homepage), but it is explicitly open to
reconsideration rather than fixed — treat it as incumbent evidence only, not a constraint, for future
design work.

## Evidence on Hand

`CITATIONS.md` lists 100+ peer-reviewed papers using the package (through 2025), demonstrating
established real-world academic adoption — do not fabricate additional testimonials or case studies
beyond this record. Published paper: Schlegel, R. W., & Smit, A. J. (2018). *heatwaveR: A central
algorithm for the detection of heatwaves and cold-spells*. Journal of Open Source Software, 3(27),
821, doi:10.21105/joss.00821. Bundled example datasets (`sst_WA`, `sst_Med`, `sst_NW_Atl`, `Algiers`)
are real observed SST/air-temperature records used throughout examples, vignettes, and tests — prefer
them over synthetic data in any new documentation or examples.

## Product Principles

- Correctness and reproducibility of the Hobday et al. definitions take precedence over any other
  concern — this is a scientific reference implementation, not a general-purpose analytics tool.
- Meet researchers where their data already is (daily/hourly `Date` or `POSIXct` series, flexible
  column naming) rather than requiring reformatting.
- Prefer real example datasets and citation-backed evidence over invented content in documentation.
- `heatwaveR` sits in a small, same-author ecosystem (`heatwaveR`, `heatwave3`, the Marine Heatwave
  Tracker, the Shiny demo app) — positioning and roadmap choices may consider consistency across this
  ecosystem, even though each tool keeps its own repo and scope.
- The current visual identity is incumbent evidence, not a binding constraint; future design work may
  revisit it as needed.

## Accessibility & Inclusion

No product-specific requirement established beyond standard pkgdown/CRAN documentation accessibility
norms.
