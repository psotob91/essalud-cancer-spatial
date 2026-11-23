# Sensitivity of the relative risks to indirect standardisation of the
# expected counts.
#
# The main analysis uses unstandardised expected counts (descriptive estimand:
# the insured population as it is). Here the same model and graph are refitted
# with expected counts indirectly standardised by sex and by age (18 five-year
# groups), using the national rates of the insured population of each year.
#
# Input:  output/models/model_data.rds, output/models/district_graph.adj,
#         data/bd_casos.rds, data/bd_casos_sexo.rds, data/bd_casos_edad.rds
# Output: output/tables/sensitivity_standardised_fit.csv
#         output/tables/sensitivity_standardised_agreement.csv
#         output/tables/sensitivity_standardised_department.csv

suppressPackageStartupMessages({ library(tidyverse); library(here); library(INLA) })
source(here("R", "functions.R"))

d <- readRDS(path_output("models", "model_data.rds"))$model_data
g <- inla.read.graph(filename = path_output("models", "district_graph.adj"))
districts <- unique(d$ubigeo_domic)

# --------------------------------------------- expected counts by sex ---
# Reference rates use the whole insured population of each sex as
# denominator (the sex-specific case file has rows only where cases exist).
cases_sex <- readRDS(path_data("bd_casos_sexo.rds")) |>
  filter(ubigeo_domic %in% districts, sex %in% c("F", "M")) |>
  group_by(year, sex) |>
  summarise(cases = sum(ncasos, na.rm = TRUE), .groups = "drop")
ref_sex <- d |>
  group_by(year) |>
  summarise(M = sum(hombre), F = sum(mujer), .groups = "drop") |>
  pivot_longer(c(M, F), names_to = "sex", values_to = "population") |>
  inner_join(cases_sex, by = c("year", "sex")) |>
  mutate(rate = cases / population) |>
  select(year, sex, rate) |>
  pivot_wider(names_from = sex, values_from = rate, names_prefix = "rate_")
E_sex <- d |>
  left_join(ref_sex, by = "year") |>
  transmute(ubigeo_domic, year, E_sex = hombre * rate_M + mujer * rate_F)

# --------------------------------------------- expected counts by age ---
age <- readRDS(path_data("bd_casos_edad.rds")) |>
  filter(ubigeo_domic %in% districts, !is.na(ageg)) |>
  mutate(total = as.numeric(total), ncasos = as.numeric(ncasos))
ref_age <- age |> group_by(year, ageg) |>
  summarise(rate = sum(ncasos, na.rm = TRUE) / sum(total, na.rm = TRUE), .groups = "drop")
E_age <- age |>
  left_join(ref_age, by = c("year", "ageg")) |>
  group_by(ubigeo_domic, year) |>
  summarise(E_age = sum(total * rate, na.rm = TRUE), .groups = "drop")

cmp <- d |>
  left_join(E_sex, by = c("ubigeo_domic", "year")) |>
  left_join(E_age, by = c("ubigeo_domic", "year")) |>
  filter(!is.na(E_sex), !is.na(E_age), E_sex > 0, E_age > 0)

fits <- list(Unstandardised = fit_bym(cmp, g, offset = cmp$E),
             `Sex-standardised` = fit_bym(cmp, g, offset = cmp$E_sex),
             `Age-standardised` = fit_bym(cmp, g, offset = cmp$E_age))

fit_table <- imap(fits, function(f, label) {
  ok <- f$cpo$failure == 0
  lg <- inla.group.cv(f, num.level.sets = 3)
  tibble(specification = label, DIC = f$dic$dic, WAIC = f$waic$waic,
         mean_log_CPO_reliable = mean(log(f$cpo$cpo[ok]), na.rm = TRUE),
         mean_log_LGOCV_3 = mean(log(lg$cv), na.rm = TRUE))
}) |> bind_rows()

rr <- map(fits, ~ .x$summary.fitted.values$mean)
agreement <- function(x, y, label) {
  n <- ceiling(0.1 * length(x))
  tibble(comparison = label, pearson = cor(x, y), spearman = cor(x, y, method = "spearman"),
         top_decile_overlap = length(intersect(order(x, decreasing = TRUE)[1:n],
                                               order(y, decreasing = TRUE)[1:n])) / n)
}
agree <- bind_rows(agreement(rr[[1]], rr[[2]], "Unstandardised vs sex-standardised"),
                   agreement(rr[[1]], rr[[3]], "Unstandardised vs age-standardised"))

department <- cmp |>
  mutate(RR_unstd = rr[[1]], RR_sex = rr[[2]], RR_age = rr[[3]]) |>
  group_by(departamento) |>
  summarise(districts = n_distinct(ubigeo_domic), across(starts_with("RR_"), mean),
            .groups = "drop") |>
  arrange(desc(RR_unstd))

write_csv(fit_table,  path_output("tables", "sensitivity_standardised_fit.csv"))
write_csv(agree,      path_output("tables", "sensitivity_standardised_agreement.csv"))
write_csv(department, path_output("tables", "sensitivity_standardised_department.csv"))
print(as.data.frame(fit_table))
print(as.data.frame(agree))
write_session_info("32_sensitivity_standardised")
