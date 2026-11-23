# Figure 5: cancer contact prevalence and poverty incidence by department,
# 2019 and 2022.
#   A  prevalence vs poverty incidence, with a linear mixed model
#      (poverty and year as fixed effects, random intercept by department)
#   B  change in poverty incidence 2019-2022 by region (paired Wilcoxon)
#   C  change in cancer contact prevalence 2019-2022 by region (paired Wilcoxon)
#
# Poverty incidence: INEI, Sistema de Información Regional para la Toma de
# Decisiones (SIRTOD), "Incidencia de pobreza por grupos de departamentos según
# intervalos de confianza"; each department takes the midpoint of the interval
# of its group. Lima is split into Lima Metropolitana and Lima Provincias, as
# in the INEI series.
#
# Department prevalence: cases divided by the sum of the male and female
# insured population over the rows of the cancer type-by-sex file, as in the
# published figure.
#
# Input:  data/bd_casos_tipos_sexo.rds, data-raw/inei_poverty_department_groups.csv
# Output: output/figures/figure5.png
#         output/tables/figure5_department_data.csv, figure5_mixed_model.csv,
#         figure5_lrt_random_slope.csv, figure5_wilcoxon.csv

suppressPackageStartupMessages({
  library(tidyverse); library(here); library(lme4); library(broom.mixed)
  library(ggrepel); library(patchwork)
})
source(here("R", "functions.R"))

poverty <- read_csv(here("data-raw", "inei_poverty_department_groups.csv"),
                    show_col_types = FALSE) |>
  rename(Dep = department_group) |>
  pivot_longer(!Dep, names_to = "year", values_to = "interval") |>
  separate(interval, sep = "[+]", into = c("lower", "upper")) |>
  mutate(per = (as.numeric(lower) + as.numeric(upper)) / 2, year = as.numeric(year))

prevalence <- readRDS(path_data("bd_casos_tipos_sexo.rds")) |>
  parse_population() |>
  filter(!is.na(sex), !is.na(tipo_cancer)) |>
  mutate(departamento = case_when(
    departamento == "15 Lima" & provincia == "01 Lima" ~ "15 LIMA METROPOLITANA 1/",
    departamento == "15 Lima" ~ "15 LIMA 2/",
    TRUE ~ departamento)) |>
  group_by(year, departamento) |>
  summarise(n = sum(ncasos), population = sum(hombre, na.rm = TRUE) + sum(mujer, na.rm = TRUE),
            .groups = "drop") |>
  mutate(prev_10k = n / population * 10000,
         Dep = toupper(str_remove(departamento, "^[0-9]+ ")))

accents <- c("APURÍMAC" = "APURIMAC", "HUÁNUCO" = "HUANUCO", "ÁNCASH" = "ANCASH",
             "JUNÍN" = "JUNIN", "SAN MARTÍN" = "SAN MARTIN",
             "LIMA 2/" = "LIMA", "LIMA METROPOLITANA 1/" = "LIMA")
regions <- list(
  Coast = c("LIMA", "CALLAO", "AREQUIPA", "LA LIBERTAD", "LAMBAYEQUE", "ICA", "PIURA",
            "TUMBES", "MOQUEGUA", "TACNA", "ANCASH"),
  Highlands = c("CUSCO", "JUNIN", "PUNO", "AYACUCHO", "HUANCAVELICA", "HUANUCO",
                "APURIMAC", "CAJAMARCA", "PASCO"),
  Rainforest = c("UCAYALI", "LORETO", "SAN MARTIN", "AMAZONAS", "MADRE DE DIOS"))

fig5 <- prevalence |>
  filter(year %in% c(2019, 2022)) |>
  inner_join(poverty, by = c("Dep", "year")) |>
  mutate(department = coalesce(accents[Dep], Dep),
         Region = case_when(department %in% regions$Coast ~ "Coast",
                            department %in% regions$Highlands ~ "Highlands",
                            department %in% regions$Rainforest ~ "Rainforest"),
         prev_100k = prev_10k * 10)
write_csv(fig5, path_output("tables", "figure5_department_data.csv"))

# Mixed models (prevalence per 10,000, as fitted for the article).
model_data <- fig5 |> mutate(year = factor(year), department = factor(department))
m_intercept <- lmer(prev_10k ~ per + year + (1 | department), data = model_data, REML = FALSE)
m_slope     <- lmer(prev_10k ~ per + year + (per | department), data = model_data, REML = FALSE)
write_csv(tidy(m_intercept, effects = "fixed", conf.int = TRUE, conf.method = "profile"),
          path_output("tables", "figure5_mixed_model.csv"))
lrt <- anova(m_intercept, m_slope)
write_csv(as_tibble(lrt, rownames = "model"), path_output("tables", "figure5_lrt_random_slope.csv"))

# Paired Wilcoxon signed-rank tests, 2019 vs 2022, within each region. Exact
# p-values are used when the paired differences have no ties or zeros, and the
# normal approximation with continuity correction otherwise (the behaviour of
# wilcox.test before R 4.4, used for the article). Poverty differences are tied
# because departments in the same INEI group share the same value.
paired_test <- function(x, y) {
  d <- y - x
  exact <- !any(d == 0) && !any(duplicated(abs(d)))
  wilcox.test(x, y, paired = TRUE, exact = exact)$p.value
}
paired <- fig5 |> mutate(year = as.character(year)) |> arrange(Region, Dep, year)
wilcoxon <- fig5 |>
  select(Region, Dep, year, per, prev_100k) |>
  pivot_wider(names_from = year, values_from = c(per, prev_100k)) |>
  group_by(Region) |>
  summarise(n = n(),
            p_poverty = paired_test(per_2019, per_2022),
            p_prevalence = paired_test(prev_100k_2019, prev_100k_2022), .groups = "drop")
write_csv(wilcoxon, path_output("tables", "figure5_wilcoxon.csv"))

label_if <- function(d) ifelse(d$prev_10k > 2.5 | d$per > 35,
                               paste0(d$department, "\nPrev:", round(d$prev_100k, 1),
                                      "\nPov:", round(d$per, 1), "%"), NA)
pA <- ggplot(fig5, aes(per, prev_100k)) +
  geom_point(aes(colour = Region)) +
  facet_wrap(~ year, ncol = 1) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, alpha = 0.1, colour = "#008080") +
  geom_label_repel(aes(label = label_if(fig5), colour = Region), size = 2.2,
                   max.overlaps = Inf, force = TRUE, na.rm = TRUE) +
  ylim(c(-2, 50.5)) +
  labs(y = "Cancer Prevalence rate per 100,000 EsSalud-insured individuals",
       x = "Poverty incidence (percentage)",
       caption = "Linear mixed-effects model\n(random intercept by department)") +
  theme_bw()

paired_panel <- function(var, pcol, ylab, ylim, label_y) {
  labels <- wilcoxon |>
    transmute(Region, label = paste0("p = ", signif(.data[[pcol]], 2)))
  ggplot(paired, aes(year, .data[[var]])) +
    geom_jitter(aes(group = Dep, colour = Region), width = 0.1) +
    geom_line(aes(group = Dep, colour = Region)) +
    geom_text(data = labels, aes(x = 1.5, y = label_y, label = label), inherit.aes = FALSE) +
    ylim(ylim) + facet_wrap(~ Region) + labs(x = "Year", y = ylab) + theme_bw()
}
pB <- paired_panel("per", "p_poverty", "Poverty Inc.\n (percentage)", c(12, 45), 42)
pC <- paired_panel("prev_100k", "p_prevalence",
                   "Cancer Prev. \nper 100,000 EsSalud-insured indiv.", c(2, 47), 42)

p <- (pA / pB / pC) + plot_layout(heights = c(2.8, 1, 1)) +
  plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(size = 16, face = "bold"))
ggsave(path_output("figures", "figure5.png"), p, width = 4200, height = 6000, units = "px",
       dpi = 600, bg = "white")

print(lrt)
print(as.data.frame(wilcoxon))
write_session_info("14_figure5")
