# Figure 3: district maps by year.
#   A  model-smoothed cancer contact prevalence per 100,000 (binomial model)
#   B  posterior mean relative risk (Poisson model)
#   C  crude cancer mortality per 100,000
# Also writes the crude contact prevalence map.
#
# Input:  output/models/model_data.rds, output/models/main_models.rds,
#         data/shapefile_peru_dptos.shp
# Output: output/figures/figure3.png
#         output/figures/district_crude_prevalence.png

suppressPackageStartupMessages({ library(tidyverse); library(here); library(sf); library(patchwork) })
source(here("R", "functions.R"))

md   <- readRDS(path_output("models", "model_data.rds"))
fits <- readRDS(path_output("models", "main_models.rds"))
departments <- read_department_map()

map_est <- md$map_cases |>
  mutate(RR = fits$fit_rr$summary.fitted.values$mean,
         P  = 1e5 * fits$fit_prev$summary.fitted.values$mean)

district_map <- function(data, fill, title, legend) {
  ggplot(data) +
    geom_sf(aes(fill = {{ fill }}), colour = alpha("black", 1/3), linewidth = 0.05) +
    geom_sf(data = departments, colour = alpha("black", 1/2), fill = NA, linewidth = 0.1) +
    facet_wrap(~ year, ncol = 4) +
    scale_fill_gradient2(low = "#0571b0", mid = "#ffffbf", high = "#ca0020",
                         midpoint = 1, na.value = "grey") +
    labs(title = title, fill = legend) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))
}

pA <- district_map(map_est, P, "Cancer contact prevalence per 100,000 (smoothed)", "Prev.\n(x100,000)")
pB <- district_map(map_est, RR, "Spatial relative risk of recorded cancer contact", "RR")
pC <- district_map(md$map_deaths, 1e5 * mort, "Crude cancer mortality per 100,000", "Mortality\n(x100,000)")

ggsave(path_output("figures", "figure3.png"), pA / pB / pC + plot_annotation(tag_levels = "A"),
       width = 12, height = 18, units = "cm", scale = 2, dpi = 600, bg = "white")

p_crude <- district_map(md$map_cases, 1e5 * prev, "Crude cancer contact prevalence per 100,000",
                        "Prev.\n(x100,000)")
ggsave(path_output("figures", "district_crude_prevalence.png"), p_crude,
       width = 12, height = 6, units = "cm", scale = 2, dpi = 600, bg = "white")
write_session_info("22_figure3")
