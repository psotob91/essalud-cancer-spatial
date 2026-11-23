# Figure 1: EsSalud-insured population (bars, millions) and crude cancer
# contact prevalence per 100,000 (line), 2019-2022.
#
# Bars: insured population summed over all rows of the insured-population
# file for each year. Line: crude prevalence of Table 1.
#
# Input:  data/bd_casos.rds, output/tables/table1_all_cancers.csv
# Output: output/figures/figure1.png

suppressPackageStartupMessages({ library(tidyverse); library(here) })
source(here("R", "functions.R"))

insured <- readRDS(path_data("bd_casos.rds")) |>
  parse_population() |>
  group_by(year) |>
  summarise(enrolled = sum(total) / 1e6, .groups = "drop")

fig1 <- read_csv(path_output("tables", "table1_all_cancers.csv"), show_col_types = FALSE) |>
  select(year, prevalence = rate) |>
  left_join(insured, by = "year")

# The prevalence line is drawn on the bar scale and relabelled on the right axis.
conversion <- max(fig1$enrolled) / (max(fig1$prevalence) / 1.75)

p <- ggplot(fig1, aes(x = factor(year))) +
  geom_col(aes(y = enrolled), fill = "#008080", alpha = 0.7) +
  geom_line(aes(y = prevalence * conversion), colour = "red", linewidth = 1, group = 1) +
  geom_point(aes(y = prevalence * conversion), colour = "red", size = 3) +
  geom_text(aes(y = 5, label = round(enrolled, 2)), fontface = "bold") +
  geom_text(aes(y = prevalence * conversion * 1.1, label = round(prevalence, 2)),
            colour = "red", fontface = "bold") +
  scale_y_continuous(name = "Number of Enrolled (Millions)",
                     sec.axis = sec_axis(~ . / conversion, name = "Cancer Prevalence (per 100,000)")) +
  labs(title = "Number of Enrolled and Cancer Prevalence by Year", x = "Year") +
  theme_minimal()

ggsave(path_output("figures", "figure1.png"), p, width = 3500, height = 2400, units = "px",
       dpi = 600, bg = "white")
write_csv(fig1, path_output("tables", "figure1_data.csv"))
write_session_info("11_figure1")
