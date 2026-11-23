# Figure 2: cancer contact prevalence per 100,000 by cancer type, sex and
# year (women to the right, men to the left).
#
# For each type, sex and year the denominator is the insured population of
# that sex summed over the district rows of the sex-specific file, as in
# the published figure. Corpus uteri is shown together with cervix uteri.
#
# Input:  data/bd_casos_tipos_sexo.rds
# Output: output/tables/figure2_rates_by_type_sex.csv
#         output/figures/figure2.png

suppressPackageStartupMessages({ library(tidyverse); library(here) })
source(here("R", "functions.R"))

figure2_labels <- c(
  "Próstata" = "Prostate", "Mama (sólo en mujeres)" = "Breast",
  "Otro tipo de cáncer" = "Other cancer type", "Colorrectal (incluye ano)" = "Colorectal (incl. anus)",
  "Tiroides" = "Thyroid", "Linfoma no Hodgkin" = "Non-Hodgkin lymphoma",
  "Cáncer de piel no melanómico" = "Non-melanoma skin cancer", "Ovario" = "Ovary",
  "Estómago" = "Stomach", "Cuello uterino" = "Cervix uteri", "Cuerpo uterino" = "Cervix uteri",
  "Pulmón (incluyendo tráquea)" = "Lung (incl. trachea)", "Leucemia" = "Leukemia",
  "Cerebro y sistema nervioso central" = "Brain and CNS", "Riñón" = "Kidney", "Hígado" = "Liver",
  "Testículos" = "Testis", "Páncreas" = "Pancreas", "Labio, Cavidad oral" = "Lip, Oral cavity",
  "Nasofaringe" = "Nasopharynx", "Melanoma de piel" = "Skin Melanoma",
  "Vesícula biliar" = "Gallbladder", "Vulva" = "Vulva", "Pene" = "Penis", "Laringe" = "Larynx",
  "Linfoma de Hodgkin" = "Hodgkin lymphoma", "Esófago" = "Esophagus",
  "Glándulas salivares" = "Salivary glands", "Sarcoma de Kaposi" = "Kaposi's sarcoma",
  "Mesotelioma" = "Mesothelioma", "Hipofaringe" = "Hypopharynx",
  "Mieloma múltiple (incluidas las enfermedades inmunoproliferativas)" = "Multiple Myeloma"
)

rates <- readRDS(path_data("bd_casos_tipos_sexo.rds")) |>
  parse_population() |>
  mutate(cancer_type = coalesce(figure2_labels[tipo_cancer], tipo_cancer)) |>
  filter(!is.na(sex), !is.na(cancer_type)) |>
  group_by(year, sex, cancer_type) |>
  summarise(cases = sum(ncasos), men = sum(hombre, na.rm = TRUE),
            women = sum(mujer, na.rm = TRUE), .groups = "drop") |>
  mutate(rate = if_else(sex == "M", cases / men, cases / women) * 1e5)
write_csv(rates, path_output("tables", "figure2_rates_by_type_sex.csv"))

p <- rates |>
  mutate(n = if_else(sex == "M", -rate, rate)) |>
  ggplot(aes(x = reorder(cancer_type, abs(n)), y = n, fill = sex)) +
  geom_col() +
  facet_wrap(~ year) +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(values = c(M = "steelblue", F = "purple"),
                    labels = c(M = "Male", F = "Female")) +
  labs(title = "Crude recorded cancer contact prevalence per 100,000 insured",
       subtitle = "Cancer in men and women",
       x = NULL, y = "Crude contact prevalence by sex (per 100,000)", fill = "Sex") +
  theme_minimal()

ggsave(path_output("figures", "figure2.png"), p, width = 5000, height = 5400, units = "px",
       dpi = 600, bg = "white")
write_session_info("12_figure2")
