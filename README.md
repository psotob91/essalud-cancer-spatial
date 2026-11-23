# Spatio-temporal analysis of recorded cancer contact prevalence in EsSalud, Peru, 2019–2022

Analysis code for:

> Soto-Becerra P, Campos Segura AV, Osada J, Díaz-Obregón D, De La Cruz-Vargas J, Murillo Carrasco AG, Beltrán B. *Geographical variation in cancer prevalence during and after COVID-19 in Peru based on the social health insurance system.* PLOS Global Public Health (under review).

The repository contains every step from the district-level data to the tables and figures of the article: construction of the analytical dataset, the adjacency matrix, the specification and priors of the Bayesian spatio-temporal model, model assessment, sensitivity analyses, and the code that turns individual health records into the district-level counts.

## Contents

```
R/functions.R                 shared functions (data reading, adjacency, priors, model)
scripts/
  00_run_all.R                runs the public analysis end to end
  00_download_data.R          downloads the district-level data from figshare
  10_table1.R                 Table 1
  11_figure1.R                Figure 1
  12_figure2.R                Figure 2
  13_figure4.R                Figure 4 (COVID-19 and cancer by department)
  14_figure5.R                Figure 5 (cancer contact prevalence and poverty)
  20_adjacency_matrix.R       model dataset and queen-contiguity adjacency matrix
  21_main_model.R             main model, hyperparameters, fit statistics, district estimates
  22_figure3.R                Figure 3
  30_model_assessment.R       DIC, WAIC, CPO/PIT and leave-group-out cross-validation
  31_sensitivity_low_expected.R   extreme relative risks; refits excluding small expected counts
  32_sensitivity_standardised.R   refits with sex- and age-standardised expected counts
  preprocessing/              from individual records to district counts (restricted data)
    records.R                 record-level functions (import, linkage, exclusions, case pool)
    01_prepare_district_counts.R  builds the district-year files deposited in figshare
    02_missingness_exclusions.R   completeness of key variables and excluded records
    03_district_boundaries.R      district and department boundary files
data/                         input data (downloaded, not tracked)
data-raw/                     small public inputs: INEI poverty, MINSA COVID-19 aggregates
output/                       results (generated, not tracked)
```

## Data

**External public data.** Poverty incidence by department (INEI) and COVID-19 cases and deaths by department (Ministry of Health open data) are in `data-raw/`, with their sources in `data-raw/README.md`.

**District-level data (public).** Counts of cancer contacts and deaths by district and year (overall, by sex, by age group and by cancer type), the EsSalud insured population of each district, and the district and department boundaries are deposited in figshare under a CC BY 4.0 licence: <https://doi.org/10.6084/m9.figshare.32043702>. `scripts/00_download_data.R` downloads the files used here into `data/`.

**Individual records (restricted).** The district-level files were built from outpatient, emergency and inpatient electronic health records of EsSalud (2019–2022) and from the national death registry (SINADEF). These records contain personal identifiers and cannot be shared publicly. Requests for access should be addressed to the Instituto de Evaluación de Tecnologías en Salud e Investigación (IETSI), EsSalud, and are subject to its data-governance and ethics approval procedures. The code in `scripts/preprocessing/` documents every preprocessing step; with authorised access it is run by setting `ESSALUD_RAW_DIR` to the folder that contains the source files.

## Requirements

- R ≥ 4.5 (results were produced with R 4.6.0 on Windows 10).
- INLA, installed from its own repository:

  ```r
  install.packages("INLA", repos = c(getOption("repos"),
                   INLA = "https://inla.r-inla-download.org/R/stable"), dep = TRUE)
  ```

- CRAN packages: tidyverse, here, sf, spdep, SpatialEpi, patchwork, ggrepel, lme4, broom.mixed, rio, janitor, lubridate, naniar, DiagrammeR, DiagrammeRsvg, rsvg.

`renv.lock` records the exact package versions. To recreate the environment:

```r
install.packages("renv")
renv::restore()
```

## Running the analysis

Open `essalud-cancer-spatial.Rproj` (or set the working directory to the repository root) and run

```r
source("scripts/00_run_all.R")
```

or, from a terminal, `Rscript scripts/00_run_all.R`. Results are written to `output/tables`, `output/figures` and `output/models`. Every script can also be run on its own, in the numbered order.

| Article element | Script | Output |
|---|---|---|
| Table 1 | `10_table1.R` | `table1_all_cancers.csv`, `table1_by_cancer_type.csv` |
| Figure 1 | `11_figure1.R` | `figure1.png` |
| Figure 2 | `12_figure2.R` | `figure2.png`, `figure2_rates_by_type_sex.csv` |
| Figure 3 | `22_figure3.R` | `figure3.png`, `district_crude_prevalence.png` |
| Figure 4 | `13_figure4.R` | `figure4.png`, `figure4_correlations.csv` |
| Figure 5 | `14_figure5.R` | `figure5.png`, `figure5_mixed_model.csv`, `figure5_lrt_random_slope.csv`, `figure5_wilcoxon.csv` |
| Model hyperparameters and fit | `21_main_model.R` | `main_model_hyperparameters.csv`, `main_model_fit.csv` |
| District-year estimates | `21_main_model.R` | `district_year_estimates.csv` |
| Supplementary Table 4 (model assessment) | `30_model_assessment.R`, `32_sensitivity_standardised.R` | `model_assessment.csv`, `sensitivity_standardised_fit.csv` |
| Supplementary Tables 5–6 (standardisation) | `32_sensitivity_standardised.R` | `sensitivity_standardised_agreement.csv`, `sensitivity_standardised_department.csv` |
| Robustness of extreme relative risks | `31_sensitivity_low_expected.R` | `sensitivity_extreme_rr.csv`, `sensitivity_low_expected*.csv` |
| Supplementary Note 2 (missing data) | `preprocessing/02_missingness_exclusions.R` | `missing_*.csv`, `missing_*.png` |

## Methods implemented

**Unit of analysis.** District *i* (1,841 districts with insured population in all four years) and year *j* (2019–2022). The outcome is the number of insured persons with a recorded cancer contact (ICD-10 chapter C) under a cumulative definition: a person counted in one year remains counted in later years until death (`scripts/preprocessing/records.R`, `build_case_pool()`).

**Expected counts.** *E<sub>ij</sub>* is the overall crude contact prevalence of the insured population applied to the insured population of district *i* in year *j* (`expected_counts()`). Sex- and age-standardised alternatives are fitted in `32_sensitivity_standardised.R`.

**Adjacency matrix.** Binary queen contiguity between district polygons (districts sharing a border or a vertex are neighbours), built with `spdep::poly2nb()` after `sf::st_make_valid()` and written in INLA graph format (`build_adjacency()`, `20_adjacency_matrix.R`). The summary of the graph is written to `adjacency_summary.csv`.

**Model.**

$$y_{ij} \sim \text{Poisson}(E_{ij}\,\theta_{ij}), \qquad \log\theta_{ij} = \alpha + u_i + v_i + \gamma_j$$

where *u<sub>i</sub>* is an intrinsic conditional autoregressive (ICAR) effect on the adjacency graph, *v<sub>i</sub>* an unstructured district effect (Besag–York–Mollié decomposition, INLA model `"bym"`) and *γ<sub>j</sub>* an independent year effect. The posterior mean of *θ<sub>ij</sub>* is the relative risk of recorded cancer contact. The same linear predictor with a binomial likelihood and the insured population as number of trials gives the smoothed contact prevalence. Models are fitted with INLA (`fit_bym()`, `21_main_model.R`).

**Priors** (`priors` in `R/functions.R`, equal to the INLA defaults):

| Parameter | Prior |
|---|---|
| Intercept *α* | flat (Gaussian with precision 0) |
| Precision of *v<sub>i</sub>* (unstructured) | log-gamma(1, 5 × 10⁻⁴) on the log-precision |
| Precision of *u<sub>i</sub>* (ICAR) | log-gamma(1, 5 × 10⁻⁴) on the log-precision |
| Precision of *γ<sub>j</sub>* | log-gamma(1, 5 × 10⁻⁵) on the log-precision |

Hyperparameters are reported as precisions with the corresponding variances (`main_model_hyperparameters.csv`).

**Model assessment.** DIC, WAIC, conditional predictive ordinates and probability integral transform values, and leave-group-out cross-validation (`inla.group.cv`, automatic groups with 1–3 level sets), which is preferred to leave-one-out CPO in spatially structured models (`30_model_assessment.R`).

**Sensitivity analyses.** (i) Description of the district-years with the most extreme relative risks and refits excluding, from the likelihood only, districts with mean expected counts below 5, 10 or 20, keeping the adjacency graph intact (`31_sensitivity_low_expected.R`); (ii) refits with indirectly sex- and age-standardised expected counts, compared on fit, cross-validation and agreement of the relative risks (`32_sensitivity_standardised.R`).

## Checking the results

Running the public scripts on the figshare data reproduces the published values, for example:

| Quantity | Value |
|---|---|
| Crude contact prevalence per 100,000, 2019 / 2022 | 442.72 / 778.92 |
| Posterior mean relative risk, range | 0.018 – 7.162 |
| Largest upper 95% credible limit | 14.495 |
| DIC / WAIC, main model | 23,049.7 / 22,489.7 |
| Mean log LGOCV score, 1 / 2 / 3 level sets | −1.527 / −1.593 / −1.713 |
| Figure 5: likelihood-ratio test for a random poverty slope | χ² = 3.90, df = 2, p = 0.14 |

INLA results can differ in the third decimal between INLA versions and operating systems.

## Citation

If you use this code, please cite the article above and this repository (see `CITATION.cff`). The archived version of the repository has a permanent DOI on Zenodo.

## Licence

Code: MIT licence (`LICENSE`). The figshare data are distributed under CC BY 4.0.
