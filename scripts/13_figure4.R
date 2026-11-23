# Figure 4: COVID-19 and cancer by department, 2020 and 2021.
#   A  cancer contacts vs COVID-19 cases (log10 scale)
#   B  cancer deaths vs COVID-19 deaths (log10 scale)
# Each point is a department; Pearson correlation of the log10 counts with a
# linear fit and 95% confidence band.
#
# Input:  data/bd_casos_tipos_sexo.rds, data/bd_muerte_sexo.rds,
#         data-raw/covid_cases_department.csv, data-raw/covid_deaths_department.csv
#         (built by data-raw/prepare_covid_minsa.R from MINSA open data)
# Output: output/figures/figure4.png, output/tables/figure4_correlations.csv

suppressPackageStartupMessages({
  library(tidyverse); library(here); library(ggrepel); library(patchwork)
})
source(here("R", "functions.R"))

regions <- list(
  Coast = c("LIMA", "CALLAO", "AREQUIPA", "LA LIBERTAD", "LAMBAYEQUE", "ICA", "PIURA",
            "TUMBES", "MOQUEGUA", "TACNA", "ANCASH"),
  Highlands = c("CUSCO", "JUNIN", "PUNO", "AYACUCHO", "HUANCAVELICA", "HUANUCO",
                "APURIMAC", "CAJAMARCA", "PASCO"),
  Rainforest = c("UCAYALI", "LORETO", "SAN MARTIN", "AMAZONAS", "MADRE DE DIOS"))

# Department names as in the MINSA files: upper case without accents.
department_counts <- function(file, outcome) {
  readRDS(path_data(file)) |>
    filter(!is.na(sex), year %in% c(2020, 2021)) |>
    group_by(year, departamento) |>
    summarise(cancer = sum(.data[[outcome]]), .groups = "drop") |>
    mutate(DEPARTAMENTO = iconv(toupper(str_remove(departamento, "^[0-9]+ ")),
                                from = "UTF-8", to = "ASCII//TRANSLIT"))
}

panel_data <- function(cancer, covid_file) {
  covid <- read_csv(here("data-raw", covid_file), show_col_types = FALSE) |>
    rename(DEPARTAMENTO = 1, year = 2, covid = 3)
  inner_join(cancer, covid, by = c("DEPARTAMENTO", "year")) |>
    mutate(Region = case_when(DEPARTAMENTO %in% regions$Coast ~ "Coast",
                              DEPARTAMENTO %in% regions$Highlands ~ "Highlands",
                              DEPARTAMENTO %in% regions$Rainforest ~ "Rainforest",
                              TRUE ~ "Unknown"))
}

cases  <- panel_data(department_counts("bd_casos_tipos_sexo.rds", "ncasos"),
                     "covid_cases_department.csv")
deaths <- panel_data(department_counts("bd_muerte_sexo.rds", "nmuertos"),
                     "covid_deaths_department.csv")

correlations <- bind_rows(cases |> mutate(outcome = "Cases"),
                          deaths |> mutate(outcome = "Deaths")) |>
  group_by(outcome, year) |>
  summarise(departments = n(),
            r = cor.test(log10(cancer), log10(covid))$estimate,
            p = cor.test(log10(cancer), log10(covid))$p.value, .groups = "drop")
write_csv(correlations, path_output("tables", "figure4_correlations.csv"))

scatter <- function(d, outcome, xlab, ylab) {
  labels <- correlations |> filter(.data$outcome == !!outcome) |>
    transmute(year, label = sprintf("R = %.2f, p = %.2g", r, p))
  ggplot(d, aes(log10(cancer), log10(covid))) +
    geom_smooth(method = "lm", formula = y ~ x, colour = "grey30", alpha = 0.1) +
    geom_point(aes(colour = Region)) +
    geom_text(data = labels, aes(x = -Inf, y = Inf, label = label), hjust = -0.1, vjust = 1.5,
              inherit.aes = FALSE) +
    geom_label_repel(aes(colour = Region, label = DEPARTAMENTO), size = 2.2, alpha = 0.9) +
    facet_wrap(~ year, ncol = 1) +
    labs(x = xlab, y = ylab) +
    theme_bw()
}

pA <- scatter(cases, "Cases", "Number of Cancer cases per department (log10)",
              "Number of COVID cases per department (log10)")
pB <- scatter(deaths, "Deaths", "Number of Cancer deaths per department (log10)",
              "Number of COVID deaths per department (log10)")

p <- (pA | pB) + plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 16, face = "bold"))
ggsave(path_output("figures", "figure4.png"), p, width = 6500, height = 4000, units = "px",
       dpi = 600, bg = "white")
print(as.data.frame(correlations))
write_session_info("13_figure4")
