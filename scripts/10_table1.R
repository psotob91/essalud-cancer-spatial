# Table 1: insured population, cancer contacts and crude contact prevalence
# per 100,000 by year, for all cancers and by cancer type.
#
# Input:  data/bd_casos.rds, data/bd_casos_tipos.rds
# Output: output/tables/table1_all_cancers.csv
#         output/tables/table1_by_cancer_type.csv

suppressPackageStartupMessages({ library(tidyverse); library(here) })
source(here("R", "functions.R"))

all_cancers <- read_cases() |>
  group_by(year) |>
  summarise(population = sum(total), cases = sum(ncasos), .groups = "drop")
all_cancers <- bind_cols(all_cancers, crude_rate(all_cancers$cases, all_cancers$population))

# Sex-specific denominators: women for breast, cervix, ovary and corpus
# uteri; men for prostate.

# The type file has rows only for district-years with at least one case of
# that type; the complete-district rule is applied within each type.
by_type <- readRDS(path_data("bd_casos_tipos.rds")) |>
  select_complete_districts(by = "tipo_cancer") |>
  mutate(population = case_when(tipo_cancer %in% female_sites[1:4] ~ mujer,
                                tipo_cancer == "Próstata"       ~ hombre,
                                TRUE                           ~ total)) |>
  group_by(tipo_cancer, year) |>
  summarise(population = sum(population), cases = sum(ncasos), .groups = "drop")
by_type <- bind_cols(by_type, crude_rate(by_type$cases, by_type$population)) |>
  mutate(cancer_type = cancer_type_en[tipo_cancer], .after = tipo_cancer) |>
  arrange(tipo_cancer, year)

write_csv(all_cancers, path_output("tables", "table1_all_cancers.csv"))
write_csv(by_type,     path_output("tables", "table1_by_cancer_type.csv"))
print(as.data.frame(all_cancers))

write_session_info("10_table1")
