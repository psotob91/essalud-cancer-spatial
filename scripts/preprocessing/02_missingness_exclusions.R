# Completeness of key variables and records excluded during preprocessing
# (restricted data; see README). Produces Supplementary Note 2: Supplementary Tables 7-9
# and Supplementary Figures 1-5.
#
# Input:  as in 01_prepare_district_counts.R
# Output: output/tables/missing_*.csv, output/figures/missing_*.png

suppressPackageStartupMessages({
  library(tidyverse); library(here); library(rio); library(janitor); library(lubridate)
  library(naniar); library(patchwork)
  library(DiagrammeR); library(DiagrammeRsvg); library(rsvg)
})
source(here("R", "functions.R"))
source(here("scripts", "preprocessing", "records.R"))
set.seed(20190101)

fmt <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
save_fig <- function(p, f, w, h) ggsave(path_output("figures", f), p, width = w, height = h,
                                        units = "cm", dpi = 400, bg = "white")

rec  <- link_and_flag(read_ehr_records())
pool <- build_case_pool(rec, read_sinadef())

inei_codes <- sf::st_read(path_data("shapefile_peru_distritos.shp"), quiet = TRUE)$ubigeo

denominator_codes <- import(raw_file("total_essalud2019_2022.csv")) |>
  transmute(year, ubigeo_domic = str_pad(ubigeo, 6, pad = "0")) |>
  distinct()

# ------------------------------------------------ key variables ---
# Inpatient document type and district are taken after linkage. A district
# code is "recognised" when it is an INEI code present in the denominator.
key <- rec |>
  transmute(Source = setting, Year = factor(file_year), network = red, step,
            `Insured ID` = if_else(id_missing, NA, "ok"),
            `Document type` = tipdoc, `Document number` = doc,
            `District of residence` = ubigeo,
            `District code recognised` = if_else(ubigeo %in% denominator_codes$ubigeo_domic,
                                                 "ok", NA),
            Sex = sex, Age = age_year, `Visit date` = fecha_atencion, `ICD-10 code` = dx)
key_vars <- c("Insured ID", "Document type", "Document number", "District of residence",
              "District code recognised", "Sex", "Age", "Visit date", "ICD-10 code")

# Table 7: missing values by setting and year.
table7 <- bind_rows(
  key |> group_by(Source, Year) |> miss_var_summary() |> ungroup(),
  key |> select(all_of(key_vars)) |> miss_var_summary() |>
    mutate(Source = "All settings", Year = "2019-2022")) |>
  filter(variable %in% key_vars) |>
  mutate(across(c(Source, Year), as.character))
write_csv(table7, path_output("tables", "missing_by_variable_setting_year.csv"))

# Table 8a: record-level exclusions by year.
table8a <- rec |>
  count(Year = as.character(file_year), step) |>
  bind_rows(rec |> count(step) |> mutate(Year = "2019-2022")) |>
  group_by(Year) |> mutate(percent = 100 * n / sum(n)) |> ungroup()
write_csv(table8a, path_output("tables", "missing_exclusions_by_year.csv"))

# Table 8b: from person-year contacts to analysed cases.
pool <- pool |>
  left_join(denominator_codes |> mutate(in_denominator = TRUE),
            by = c("year", "ubigeo_domic")) |>
  mutate(in_denominator = coalesce(in_denominator, FALSE))
analysed <- read_cases() |> group_by(year) |>
  summarise(analysed = sum(ncasos), districts = n_distinct(ubigeo_domic), .groups = "drop")
table8b <- pool |>
  group_by(year) |>
  summarise(contacts = n(), carried_forward = sum(carried),
            missing_district = sum(is.na(ubigeo_domic)),
            unrecognised_district = sum(!is.na(ubigeo_domic) & !in_denominator),
            subdistrict_code = sum(!is.na(ubigeo_domic) & !in_denominator & !ubigeo_domic %in% inei_codes),
            inei_code_not_in_file = sum(!in_denominator & ubigeo_domic %in% inei_codes),
            assigned = sum(in_denominator), .groups = "drop") |>
  left_join(analysed, by = "year") |>
  mutate(removed_incomplete_districts = assigned - analysed)
write_csv(table8b, path_output("tables", "missing_flow_by_year.csv"))

# Table 8c: inpatient linkage.
table8c <- rec |> filter(setting == "Inpatient") |>
  group_by(year = file_year) |>
  summarise(records = n(), linked = sum(inpatient_linked), .groups = "drop") |>
  mutate(percent_linked = 100 * linked / records)
write_csv(table8c, path_output("tables", "missing_inpatient_linkage.csv"))

# Table 8d: person-level impact of the excluded records. An excluded record
# removes a case only if its person was not counted that year through another
# record. Records without any identifier are counted as different persons, so
# the total is an upper bound.
counted <- pool |> distinct(year, tipdoc, doc) |> mutate(counted = TRUE)
table8d <- rec |>
  filter(step != "Retained") |>
  left_join(counted, by = c("file_year" = "year", "tipdoc", "doc")) |>
  mutate(counted = coalesce(counted, FALSE),
         person = case_when(!is.na(tipdoc) & !is.na(doc) ~ paste(tipdoc, doc),
                            !id_missing ~ paste("ID", autogenerado))) |>
  group_by(year = file_year) |>
  summarise(records_excluded = n(),
            of_persons_counted = sum(counted),
            persons_not_counted = n_distinct(person[!counted & !is.na(person)]),
            records_without_identifier = sum(is.na(person)), .groups = "drop") |>
  left_join(table8b |> select(year, eligible = contacts), by = "year") |>
  mutate(upper_bound_lost = persons_not_counted + records_without_identifier,
         upper_bound_pct = 100 * upper_bound_lost / (eligible + upper_bound_lost))
write_csv(table8d, path_output("tables", "missing_person_level_impact.csv"))

# Table 8e: denominator retained in the analysed districts.
insured <- import(raw_file("total_essalud2019_2022.csv")) |>
  mutate(ubigeo_domic = str_pad(ubigeo, 6, pad = "0")) |>
  parse_population() |>
  distinct(ubigeo_domic, year, .keep_all = TRUE) |>
  group_by(year) |>
  summarise(population = sum(total), districts_available = n(), .groups = "drop")
table8e <- read_cases() |>
  group_by(year) |>
  summarise(population_analysed = sum(total), .groups = "drop") |>
  left_join(insured, by = "year") |>
  mutate(percent_retained = 100 * population_analysed / population)
write_csv(table8e, path_output("tables", "missing_denominator.csv"))

# Table 8f: why some districts are not present in all four years.
table8f <- import(raw_file("total_essalud2019_2022.csv")) |>
  mutate(ubigeo_domic = str_pad(ubigeo, 6, pad = "0")) |>
  parse_population() |>
  distinct(ubigeo_domic, year, .keep_all = TRUE) |>
  group_by(ubigeo_domic) |>
  summarise(years = n(), first_year = min(year), last_year = max(year),
            mean_insured = mean(total), .groups = "drop") |>
  mutate(status = case_when(
    years == 4 ~ "Present in all four years",
    !ubigeo_domic %in% inei_codes ~ "Code absent from the INEI list",
    first_year > 2019 ~ "Created during the study period",
    str_starts(ubigeo_domic, "0812") ~ "Quispicanchi province, no record under its code in 2022",
    TRUE ~ "Missing in one intermediate year")) |>
  group_by(status) |>
  summarise(districts = n(), median_insured = median(mean_insured), .groups = "drop") |>
  add_row(status = "INEI districts never present in the file",
          districts = sum(!inei_codes %in% denominator_codes$ubigeo_domic), median_insured = 0)
write_csv(table8f, path_output("tables", "missing_district_coverage.csv"))

# Table 9: geography.
table9a <- rec |>
  group_by(network = red) |>
  summarise(records = n(),
            excluded_pct = 100 * mean(step != "Retained"),
            missing_district_pct = 100 * mean(is.na(ubigeo)),
            unrecognised_district_pct = 100 * mean(!is.na(ubigeo) &
                                                     !ubigeo %in% denominator_codes$ubigeo_domic),
            missing_id_pct = 100 * mean(is.na(tipdoc) | is.na(doc)),
            missing_sex_pct = 100 * mean(is.na(sex)),
            missing_age_pct = 100 * mean(is.na(age_year)), .groups = "drop") |>
  arrange(desc(records))
write_csv(table9a, path_output("tables", "missing_by_network.csv"))

table9b <- rec |>
  filter(!is.na(ubigeo)) |>
  group_by(department = str_sub(ubigeo, 1, 2), year = file_year) |>
  summarise(records = n(), excluded = sum(step != "Retained"),
            unrecognised = sum(!ubigeo %in% denominator_codes$ubigeo_domic), .groups = "drop")
write_csv(table9b, path_output("tables", "missing_by_department_year.csv"))

table9c <- pool |> filter(in_denominator) |>
  group_by(year) |>
  summarise(cases = n(), missing_sex = sum(is.na(sex)), missing_age = sum(is.na(age_year)),
            .groups = "drop")
write_csv(table9c, path_output("tables", "missing_sex_age_cases.csv"))

# ------------------------------------------------------- figures ---
key <- key |> mutate(across(c(`Document type`, `Document number`), as.character))

p2 <- gg_miss_var(key |> select(Year, all_of(key_vars)), facet = Year, show_pct = TRUE) +
  labs(y = "Missing (%)") + theme_bw(base_size = 10)
save_fig(p2, "missing_2_by_variable_year.png", 18, 13)

network_order <- key |> count(network, sort = TRUE) |> pull(network)
p3 <- ((gg_miss_fct(key |> select(Source, all_of(key_vars)), fct = Source) +
          labs(x = "Care setting", y = NULL) + theme_bw(base_size = 10)) |
         (gg_miss_fct(key |> select(Year, all_of(key_vars)), fct = Year) +
            labs(x = "Year", y = NULL) + theme_bw(base_size = 10))) /
  (gg_miss_fct(key |> mutate(network = factor(network, network_order)) |>
                 select(network, all_of(key_vars)), fct = network) +
     labs(x = "EsSalud health-care network (ordered by number of records)", y = NULL) +
     theme_bw(base_size = 9) +
     theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7))) +
  plot_layout(heights = c(1, 1.3)) + plot_annotation(tag_levels = "A")
save_fig(p3, "missing_3_by_setting_year_network.png", 24, 22)

png(path_output("figures", "missing_4_upset.png"), width = 2400, height = 1400, res = 300)
print(gg_miss_upset(key |> select(all_of(key_vars)), nsets = length(key_vars), nintersects = 20))
dev.off()

sample_records <- key |> group_by(Source) |> slice_sample(n = 5000) |> ungroup() |>
  arrange(Source, Year)
p5 <- vis_miss(sample_records |> select(all_of(key_vars)), warn_large_data = FALSE) +
  labs(y = "Records (random sample of 5,000 per care setting)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0), plot.margin = margin(5, 45, 5, 5))
save_fig(p5, "missing_5_vis_miss_sample.png", 20, 15)

# Flow diagram (Supplementary Figure 1). Record exclusions are a percentage of
# records; losses of contacts a percentage of the eligible person-year contacts
# of each year; excluded districts a percentage of the insured population.
nl <- "\\l"
by_step <- table8a |> filter(Year == "2019-2022")
n_step <- function(s) by_step$n[by_step$step == s]
n_raw <- nrow(rec)
p2 <- function(x, base) sprintf("%.2f%%", 100 * x / base)
p1 <- function(x, base) sprintf("%.1f%%", 100 * x / base)
pct_range <- function(x, base) {
  r <- range(100 * x / base)
  d <- if (max(r) < 1) 2 else 1
  f <- function(v) formatC(v, format = "f", digits = d)
  if (f(r[1]) == f(r[2])) paste0(f(r[1]), "%") else paste0(f(r[1]), "-", f(r[2]), "%")
}
step_line <- function(label, s) paste0("  ", label, ": ", fmt(n_step(s)), " (", p2(n_step(s), n_raw), ")")
by_year <- function(n, base) paste0("  ", table8b$year, ": ", fmt(n), " (", p1(n, base), ")", collapse = nl)
sources <- rec |> count(setting)

box_records <- paste0("Cancer-coded EHR records, EsSalud 2019-2022", nl, "n = ", fmt(n_raw), nl,
                      paste0("  ", sources$setting, ": ", fmt(sources$n), collapse = nl), nl)
box_excluded <- paste0(
  "Records excluded: ", fmt(n_raw - n_step("Retained")), " (", p2(n_raw - n_step("Retained"), n_raw),
  " of records)", nl,
  step_line("No insured ID code", "No insured ID code"), nl,
  step_line("Missing ICD-10 code", "Missing ICD-10 code"), nl,
  step_line("Identity document missing or not linkable", "Identity document missing or not linkable"), nl,
  step_line("Visit date invalid or after 2022", "Visit date invalid or after 2022"), nl,
  "Persons not otherwise counted: at most ", fmt(min(table8d$upper_bound_lost)), "-",
  fmt(max(table8d$upper_bound_lost)), " per year", nl,
  "  (", sprintf("%.1f-%.1f%%", min(table8d$upper_bound_pct), max(table8d$upper_bound_pct)),
  " of eligible person-years)", nl)
box_retained <- paste0("Records retained: ", fmt(n_step("Retained")), " (", p2(n_step("Retained"), n_raw), ")", nl)
box_eligible <- paste0("ELIGIBLE POPULATION (numerator)", nl,
                       "Person-year cancer contacts, cumulative definition = 100%", nl,
                       paste0("  ", table8b$year, ": ", fmt(table8b$contacts), collapse = nl), nl)
box_unassigned <- paste0("Not assignable to a district", nl,
                         "  Missing district of residence: ", fmt(sum(table8b$missing_district)),
                         " (", pct_range(table8b$missing_district, table8b$contacts), " per year)", nl,
                         "  EsSalud sub-district code, no INEI equivalent: ",
                         fmt(sum(table8b$subdistrict_code)),
                         " (", pct_range(table8b$subdistrict_code, table8b$contacts), " per year)", nl,
                         "  INEI code absent from the insured-population file that year: ",
                         fmt(sum(table8b$inei_code_not_in_file)),
                         " (", pct_range(table8b$inei_code_not_in_file, table8b$contacts), " per year)", nl)
box_assigned <- paste0("Contacts assigned to a district (% of eligible)", nl,
                       by_year(table8b$assigned, table8b$contacts), nl)
box_incomplete <- paste0("In districts without four complete years: ",
                         fmt(sum(table8b$removed_incomplete_districts)), nl,
                         "  (", pct_range(table8b$removed_incomplete_districts, table8b$contacts),
                         " of eligible per year)", nl)
box_analysed <- paste0("ANALYSED CASES (% of eligible)", nl,
                       by_year(table8b$analysed, table8b$contacts), nl)
n_status <- function(x) sum(table8f$districts[str_detect(table8f$status, x)])
box_den <- paste0("DENOMINATOR", nl, "EsSalud-insured population by district of residence", nl,
                  "(", fmt(sum(inei_codes %in% denominator_codes$ubigeo_domic)), " of ",
                  fmt(length(inei_codes)), " INEI districts in at least one year)", nl,
                  paste0("  ", table8e$year, ": ", fmt(table8e$population), collapse = nl), nl)
box_den_out <- paste0("Districts not analysed", nl,
                      "  ", n_status("Created"), " created during 2019-2022 (not present in all years)", nl,
                      "  ", n_status("Quispicanchi"), " of Quispicanchi province (under ",
                      n_status("absent from the INEI"), " non-INEI codes in 2022)", nl,
                      "  ", n_status("intermediate"), " missing in one intermediate year", nl,
                      "  1 without an INEI boundary (211302)", nl,
                      "  ", fmt(min(table8e$population - table8e$population_analysed)), "-",
                      fmt(max(table8e$population - table8e$population_analysed)), " insured (",
                      pct_range(table8e$population - table8e$population_analysed, table8e$population), ")", nl)
box_den_in <- paste0("ANALYSED POPULATION, ", fmt(table8b$districts[1]), " districts", nl,
                     paste0("  ", table8e$year, ": ", fmt(table8e$population_analysed), " (",
                            p1(table8e$population_analysed, table8e$population), ")", collapse = nl), nl)

dot <- sprintf('
digraph flow {
  graph [rankdir = TB, fontname = "Helvetica", nodesep = 0.45, ranksep = 0.3, splines = ortho]
  node  [shape = box, style = "rounded", fontname = "Helvetica", fontsize = 10, width = 3.9]
  edge  [arrowsize = 0.6, fontname = "Helvetica", fontsize = 9]
  a [label = "%s"]
  p1 [shape = point, width = 0.01]; b [label = "%s", style = "rounded,dashed"]
  c [label = "%s"]
  d [label = "%s", style = "rounded,filled", fillcolor = "#EEF3FA"]
  p2 [shape = point, width = 0.01]; e [label = "%s", style = "rounded,dashed"]
  h [label = "%s"]
  p3 [shape = point, width = 0.01]; f [label = "%s", style = "rounded,dashed"]
  g [label = "%s", style = "rounded,bold"]
  x1 [label = "%s", style = "rounded,filled", fillcolor = "#F4F4F4"]
  px [shape = point, width = 0.01]; x2 [label = "%s", style = "rounded,dashed"]
  x3 [label = "%s", style = "rounded,bold"]
  {rank = same; p1; b} {rank = same; d; x1} {rank = same; p2; e}
  {rank = same; p3; f; px; x2} {rank = same; g; x3}
  a -> p1 [arrowhead = none]; p1 -> b; p1 -> c
  c -> d [xlabel = "one record per person-year; survivors carried forward  "]
  d -> p2 [arrowhead = none]; p2 -> e; p2 -> h
  h -> p3 [arrowhead = none]; p3 -> f; p3 -> g
  x1 -> px [arrowhead = none]; px -> x2; px -> x3
}', box_records, box_excluded, box_retained, box_eligible, box_unassigned, box_assigned,
box_incomplete, box_analysed, box_den, box_den_out, box_den_in)

grViz(dot) |> export_svg() |> charToRaw() |>
  rsvg_png(path_output("figures", "missing_1_flowchart.png"), width = 2600)
write_session_info("02_missingness_exclusions")
