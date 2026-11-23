# data-raw/

Small public inputs that are not part of the figshare deposit. They are
tracked in the repository.

| File | Content | Source |
|---|---|---|
| `inei_poverty_department_groups.csv` | Poverty incidence (%) by group of departments, lower + upper limit of the 95% confidence interval, 2019-2023 | INEI, Sistema de Información Regional para la Toma de Decisiones (SIRTOD), from the Encuesta Nacional de Hogares (ENAHO); updated August 2024. <https://systems.inei.gob.pe/SIRTOD/app/consulta> |
| `covid_cases_department.csv` | COVID-19 cases by department and year, 2020-2021 | MINSA open data (*Casos positivos por COVID-19*), counted by department and year of `FECHA_RESULTADO`; aggregates used for the published Figure 4 |
| `covid_deaths_department.csv` | COVID-19 deaths by department and year, 2020-2021 | MINSA open data (*Fallecidos por COVID-19*), counted by department and year of `FECHA_FALLECIMIENTO`; aggregates used for the published Figure 4 |

The two COVID-19 files are the department-year aggregates used for the
published figure, produced from the MINSA downloads available at the time.
`prepare_covid_minsa.R` applies the same aggregation to a fresh download;
because the MINSA files are updated, a new download may give slightly
different counts.

INEI reports poverty for groups of departments whose estimates are not
statistically different; each department is assigned the midpoint of the
interval of its group. Ica, Madre de Dios and Moquegua are not included in
the series used.
