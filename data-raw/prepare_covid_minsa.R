# Department-level COVID-19 cases and deaths, 2020-2021, from the Ministry of
# Health (MINSA) open data portal:
#   Casos positivos por COVID-19:
#     https://www.datosabiertos.gob.pe/dataset/casos-positivos-por-covid-19-ministerio-de-salud-minsa
#   Fallecidos por COVID-19:
#     https://www.datosabiertos.gob.pe/dataset/fallecidos-por-covid-19-ministerio-de-salud-minsa
#
# Download positivos_covid.csv and fallecidos_covid.csv from those pages into
# data-raw/ and run this script from the repository root. The resulting
# department-year counts are the files used by scripts/13_figure4.R.

suppressPackageStartupMessages({ library(tidyverse); library(here) })

aggregate_minsa <- function(file, date_column) {
  read_delim(here("data-raw", file), delim = ";", col_select = c("DEPARTAMENTO", all_of(date_column)),
             col_types = cols(.default = "c"), show_col_types = FALSE) |>
    mutate(year = as.integer(substr(.data[[date_column]], 1, 4))) |>
    filter(year %in% c(2020, 2021)) |>
    count(DEPARTAMENTO, year, name = "n")
}

write_csv(aggregate_minsa("positivos_covid.csv", "FECHA_RESULTADO"),
          here("data-raw", "covid_cases_department.csv"))
write_csv(aggregate_minsa("fallecidos_covid.csv", "FECHA_FALLECIMIENTO"),
          here("data-raw", "covid_deaths_department.csv"))
