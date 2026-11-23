# Main spatio-temporal model.
#
#   y_ij ~ Poisson(E_ij * theta_ij)
#   log(theta_ij) = alpha + u_i + v_i + gamma_j
#
# u_i: intrinsic CAR (spatially structured) district effect
# v_i: unstructured district effect (BYM decomposition)
# gamma_j: iid year effect
# Priors are listed in R/functions.R (`priors`).
#
# exp(alpha + u_i + v_i + gamma_j) is the posterior relative risk of recorded
# cancer contact of district i in year j with respect to the expected count.
# The same linear predictor with a binomial likelihood (Ntrials = insured
# population) gives the smoothed contact prevalence.
#
# Input:  output/models/model_data.rds, output/models/district_graph.adj
# Output: output/models/main_models.rds
#         output/tables/main_model_hyperparameters.csv
#         output/tables/main_model_fit.csv
#         output/tables/district_year_estimates.csv

suppressPackageStartupMessages({ library(tidyverse); library(here); library(INLA) })
source(here("R", "functions.R"))

md <- readRDS(path_output("models", "model_data.rds"))
d  <- md$model_data
g  <- inla.read.graph(filename = path_output("models", "district_graph.adj"))

fit_rr   <- fit_bym(d, g, offset = d$E, family = "poisson", config = TRUE)
fit_prev <- fit_bym(d, g, ntrials = d$total, family = "binomial")
saveRDS(list(fit_rr = fit_rr, fit_prev = fit_prev), path_output("models", "main_models.rds"))

# Hyperparameters are precisions; the corresponding variances are reported
# alongside (posterior summaries of 1/precision).
inv_summary <- function(marg) {
  m <- inla.tmarginal(function(x) 1 / x, marg)
  s <- inla.zmarginal(m, silent = TRUE)
  c(mean = s$mean, lower = s$quant0.025, upper = s$quant0.975)
}
hyper <- fit_rr$summary.hyperpar |>
  rownames_to_column("parameter") |>
  transmute(parameter, precision_mean = mean, precision_lower = `0.025quant`,
            precision_upper = `0.975quant`)
variances <- map(fit_rr$marginals.hyperpar, inv_summary) |> bind_rows()
hyper <- bind_cols(hyper, rename_with(variances, ~ paste0("variance_", .x)))
fixed <- fit_rr$summary.fixed |> rownames_to_column("parameter")
write_csv(hyper, path_output("tables", "main_model_hyperparameters.csv"))
write_csv(fixed, path_output("tables", "main_model_fixed_effects.csv"))

fit_stats <- function(fit, label) {
  ok <- fit$cpo$failure == 0
  tibble(model = label, DIC = fit$dic$dic, DIC_p_eff = fit$dic$p.eff,
         WAIC = fit$waic$waic, WAIC_p_eff = fit$waic$p.eff,
         mean_log_CPO_reliable = mean(log(fit$cpo$cpo[ok]), na.rm = TRUE),
         CPO_reliable = sum(ok, na.rm = TRUE), observations = length(ok))
}
fit_table <- bind_rows(fit_stats(fit_rr, "Poisson, expected-count offset"),
                       fit_stats(fit_prev, "Binomial, insured population"))
write_csv(fit_table, path_output("tables", "main_model_fit.csv"))

fv  <- fit_rr$summary.fitted.values
fvp <- fit_prev$summary.fitted.values
estimates <- d |>
  select(ubigeo_domic, departamento, provincia, distrito, year, total, ncasos, E) |>
  mutate(RR = fv$mean, RR_lower = fv$`0.025quant`, RR_upper = fv$`0.975quant`,
         prevalence_100k = 1e5 * fvp$mean,
         prevalence_100k_lower = 1e5 * fvp$`0.025quant`,
         prevalence_100k_upper = 1e5 * fvp$`0.975quant`)
write_csv(estimates, path_output("tables", "district_year_estimates.csv"))

print(as.data.frame(hyper))
print(as.data.frame(fit_table))
cat("Posterior mean RR range:", round(range(estimates$RR), 3),
    "| maximum upper 95% limit:", round(max(estimates$RR_upper), 3), "\n")
write_session_info("21_main_model")
