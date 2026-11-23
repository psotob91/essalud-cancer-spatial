# Sensitivity of the relative risks to districts with small expected counts.
#
# Part A describes which district-years produce the most extreme relative
# risks. Part B refits the main model excluding, from the likelihood only,
# districts whose mean expected count over 2019-2022 is below 5, 10 or 20.
# Excluded districts keep their place in the map and in the adjacency
# structure (their counts are set to NA), so no retained district loses
# neighbours. Model, graph and priors are those of the main analysis.
#
# Input:  output/models/model_data.rds, output/models/main_models.rds,
#         output/models/district_graph.adj
# Output: output/tables/sensitivity_extreme_rr.csv
#         output/tables/sensitivity_low_expected.csv
#         output/tables/sensitivity_low_expected_department.csv

suppressPackageStartupMessages({ library(tidyverse); library(here); library(INLA) })
source(here("R", "functions.R"))

d   <- readRDS(path_output("models", "model_data.rds"))$model_data
fit <- readRDS(path_output("models", "main_models.rds"))$fit_rr
g   <- inla.read.graph(filename = path_output("models", "district_graph.adj"))

base <- d |> mutate(RR = fit$summary.fitted.values$mean,
                    RR_upper = fit$summary.fitted.values$`0.975quant`)

# ------------------------------------------------------------ Part A ---
extreme <- map(c(1.5, 2, 3, 5), function(u) {
  s <- filter(base, RR > u)
  tibble(threshold = u, district_years = nrow(s), districts = n_distinct(s$ubigeo_domic),
         population_median = median(s$total), expected_median = median(s$E),
         cases_median = median(s$ncasos),
         share_of_cases = sum(s$ncasos) / sum(base$ncasos))
}) |> bind_rows()
write_csv(extreme, path_output("tables", "sensitivity_extreme_rr.csv"))

# ------------------------------------------------------------ Part B ---
mean_E <- base |> group_by(ubigeo_domic) |> summarise(E_mean = mean(E), .groups = "drop")

top_decile_overlap <- function(x, y) {
  n <- ceiling(0.1 * length(x))
  length(intersect(order(x, decreasing = TRUE)[1:n], order(y, decreasing = TRUE)[1:n])) / n
}

results <- map(c(5, 10, 20), function(u) {
  keep <- mean_E$ubigeo_domic[mean_E$E_mean >= u]
  dat  <- base |> mutate(ncasos_fit = if_else(ubigeo_domic %in% keep, ncasos, NA_real_))
  f    <- fit_bym(dat, g, offset = dat$E, response = "ncasos_fit")
  kept <- dat |> mutate(RR_sens = f$summary.fitted.values$mean) |>
    filter(ubigeo_domic %in% keep)
  list(
    summary = tibble(
      min_mean_expected = u, districts = length(keep),
      share_cases = sum(kept$ncasos) / sum(base$ncasos),
      share_population = sum(kept$total) / sum(base$total),
      DIC = f$dic$dic, WAIC = f$waic$waic,
      pearson = cor(kept$RR, kept$RR_sens),
      spearman = cor(kept$RR, kept$RR_sens, method = "spearman"),
      top_decile_overlap = top_decile_overlap(kept$RR, kept$RR_sens)),
    department = kept |> group_by(departamento) |>
      summarise(RR_main = mean(RR), RR_sensitivity = mean(RR_sens), .groups = "drop") |>
      mutate(min_mean_expected = u)
  )
})

sens <- map(results, "summary") |> bind_rows()
write_csv(sens, path_output("tables", "sensitivity_low_expected.csv"))
write_csv(map(results, "department") |> bind_rows(),
          path_output("tables", "sensitivity_low_expected_department.csv"))
print(as.data.frame(extreme))
print(as.data.frame(sens))
write_session_info("31_sensitivity_low_expected")
