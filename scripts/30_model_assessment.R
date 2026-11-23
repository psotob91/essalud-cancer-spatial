# Fit and predictive assessment of the main model.
#
# Leave-one-out CPO is unreliable in a spatially structured model (the left-out
# district-year keeps its neighbours, which share its random effect) and INLA
# flags many CPO values as unreliable. Leave-group-out cross-validation
# (LGOCV; Liu, van Niekerk & Rue 2024) removes the group of observations most
# correlated with each one and needs no refitting. Groups are built
# automatically from the posterior correlation of the linear predictor, with
# num.level.sets = 1, 2, 3 (increasingly large groups).
#
# Input:  output/models/main_models.rds
# Output: output/tables/model_assessment.csv
#         output/figures/pit_histogram.png

suppressPackageStartupMessages({ library(tidyverse); library(here); library(INLA) })
source(here("R", "functions.R"))

fit <- readRDS(path_output("models", "main_models.rds"))$fit_rr

cpo_ok <- fit$cpo$failure == 0
loo <- tibble(criterion = "LOO-CPO (reliable values only)", num_level_sets = NA,
              group_size_median = 1, mean_log_score = mean(log(fit$cpo$cpo[cpo_ok]), na.rm = TRUE),
              unreliable = sum(!cpo_ok, na.rm = TRUE))

lgocv <- map(1:3, function(k) {
  cv <- inla.group.cv(fit, num.level.sets = k)
  tibble(criterion = "LGOCV", num_level_sets = k,
         group_size_median = median(lengths(map(cv$groups, "idx"))),
         mean_log_score = mean(log(cv$cv), na.rm = TRUE),
         unreliable = sum(is.na(cv$cv) | cv$cv <= 0))
}) |> bind_rows()

assessment <- bind_rows(loo, lgocv)
write_csv(assessment, path_output("tables", "model_assessment.csv"))
print(as.data.frame(assessment))

pit <- tibble(pit = fit$cpo$pit[cpo_ok])
p <- ggplot(pit, aes(pit)) +
  geom_histogram(breaks = seq(0, 1, 0.05), fill = "grey60", colour = "white") +
  labs(x = "Probability integral transform", y = "District-years") +
  theme_bw(base_size = 10)
ggsave(path_output("figures", "pit_histogram.png"), p, width = 12, height = 8, units = "cm",
       dpi = 400, bg = "white")
write_session_info("30_model_assessment")
