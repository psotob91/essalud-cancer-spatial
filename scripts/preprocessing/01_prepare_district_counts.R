# From individual EHR records to district-year counts (restricted data; see README).
#
# Builds the district-year files deposited in figshare (bd_casos, bd_muerte and
# their by-sex and by-age versions) from the EsSalud cancer-coded EHR extracts,
# the national death registry (SINADEF) and the insured-population files.
# These inputs contain personal identifiers and are available only under the
# data-access conditions described in the README; this script is published so
# that every preprocessing decision can be inspected.
#
# Case definition (cumulative recorded contact prevalence):
#   1. Pool outpatient, emergency and inpatient records with an ICD-10 code.
#   2. Keep the first visit of each person in each calendar year.
#   3. A person counted in year t who is alive on 31 December of year t is
#      carried into year t + 1 even without a new visit; people leave the pool
#      only at death (SINADEF).
#   4. Count persons by district of residence and year; the denominator is the
#      insured population of the same district and year.
#
# Input (directory set by ESSALUD_RAW_DIR, default data/restricted/):
#   {2019..2022}_onco_essi_consult_externa_v2.csv, *_emer_v2.csv, *_hosp_v2.csv
#   sinadef2017_2023.csv, total_essalud2019_2022.csv, total_essalud2019_2022.xlsx
# Output: output/preprocessing/bd_casos.rds, bd_muerte.rds, bd_casos_sexo.rds,
#         bd_muerte_sexo.rds, bd_casos_edad.rds, bd_muerte_edad.rds

suppressPackageStartupMessages({
  library(tidyverse); library(here); library(rio); library(janitor); library(lubridate)
})
source(here("R", "functions.R"))
source(here("scripts", "preprocessing", "records.R"))

out <- function(f) path_output("preprocessing", f)

rec  <- read_ehr_records()
rec  <- link_and_flag(rec)
pool <- build_case_pool(rec, read_sinadef())

total <- import(raw_file("total_essalud2019_2022.csv")) |>
  mutate(ubigeo_domic = str_pad(ubigeo, width = 6, pad = "0")) |>
  select(ubigeo_domic, everything(), -ubigeo)

age_groups <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
                "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74",
                "75-79", "80-84", "85+")
age_group <- function(age) {
  g <- cut(age, c(seq(0, 85, 5), Inf), right = FALSE, labels = age_groups)
  factor(g, age_groups)
}

total_age <- import(raw_file("total_essalud2019_2022.xlsx"), sheet = 2) |>
  filter(!str_detect(ubigeo, "To"), str_length(ubigeo) == 6) |>
  select(year, ubigeo, starts_with("edad")) |>
  pivot_longer(starts_with("edad"), names_to = "edad", values_to = "total") |>
  mutate(age = as.numeric(str_remove(edad, "edad")),
         # Single-year ages of 100 or more were not assigned an age group in
         # the deposited files; kept identical here.
         ageg = if_else(age >= 100, NA, age_group(age))) |>
  group_by(year, ubigeo_domic = ubigeo, ageg) |>
  summarise(total = sum(total), .groups = "drop") |>
  drop_na(ageg)

pool <- pool |> mutate(ageg = age_group(age_year))

# Cases and deaths; district-years absent from the counts get zero.
aggregate_to <- function(denominator, by, outcome) {
  counts <- pool |>
    { \(x) if (outcome == "nmuertos") filter(x, died_in_year == 1L) else x }() |>
    count(across(all_of(c("ubigeo_domic", "year", by))), name = outcome)
  join_by_vars <- intersect(c("ubigeo_domic", "year", by), names(denominator))
  # A few district-years appear more than once in the insured-population
  # file; rows are kept as they are (duplicates are dropped downstream by
  # select_complete_districts()).
  denominator |>
    left_join(counts, by = join_by_vars, relationship = "many-to-many") |>
    mutate(across(all_of(outcome), ~ coalesce(as.numeric(.x), 0)))
}

bd_casos       <- aggregate_to(total, character(), "ncasos")
bd_muerte      <- aggregate_to(total, character(), "nmuertos")
bd_casos_sexo  <- aggregate_to(total, "sex", "ncasos")
bd_muerte_sexo <- aggregate_to(total, "sex", "nmuertos")

# Where the age-specific insured population is smaller than the count, the
# population is raised to the count.
bd_casos_edad <- aggregate_to(total_age, "ageg", "ncasos") |>
  mutate(total = pmax(total, ncasos), dif = total >= ncasos)
bd_muerte_edad <- aggregate_to(total_age, "ageg", "nmuertos") |>
  mutate(total = pmax(total, nmuertos), dif = total >= nmuertos)

walk2(list(bd_casos, bd_muerte, bd_casos_sexo, bd_muerte_sexo, bd_casos_edad, bd_muerte_edad),
      c("bd_casos", "bd_muerte", "bd_casos_sexo", "bd_muerte_sexo", "bd_casos_edad", "bd_muerte_edad"),
      ~ saveRDS(.x, out(paste0(.y, ".rds"))))

# Agreement with the deposited files (when present in data/).
check <- function(name, outcome, keys) {
  f <- path_data(paste0(name, ".rds"))
  if (!file.exists(f)) return(NULL)
  dep <- readRDS(f) |> distinct(across(all_of(keys)), .keep_all = TRUE) |>
    select(all_of(c(keys, outcome)))
  new <- get(name) |> distinct(across(all_of(keys)), .keep_all = TRUE) |>
    select(all_of(c(keys, outcome)))
  j <- full_join(dep, new, by = keys, suffix = c("_deposited", "_rebuilt"))
  tibble(file = name, cells = nrow(j),
         differing_cells = sum(coalesce(j[[paste0(outcome, "_deposited")]], 0) !=
                                 coalesce(j[[paste0(outcome, "_rebuilt")]], 0)))
}
agreement <- bind_rows(
  check("bd_casos", "ncasos", c("ubigeo_domic", "year")),
  check("bd_muerte", "nmuertos", c("ubigeo_domic", "year")),
  check("bd_casos_sexo", "ncasos", c("ubigeo_domic", "year", "sex")),
  check("bd_muerte_sexo", "nmuertos", c("ubigeo_domic", "year", "sex")),
  check("bd_casos_edad", "ncasos", c("ubigeo_domic", "year", "ageg")),
  check("bd_muerte_edad", "nmuertos", c("ubigeo_domic", "year", "ageg")))
print(agreement)
write_csv(agreement, path_output("tables", "restricted_rebuild_agreement.csv"))
write_session_info("01_prepare_district_counts")
