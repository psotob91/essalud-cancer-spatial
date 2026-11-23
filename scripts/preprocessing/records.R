# Record-level functions shared by the restricted scripts.

raw_dir  <- function() Sys.getenv("ESSALUD_RAW_DIR", here::here("data", "restricted"))
raw_file <- function(f) file.path(raw_dir(), f)

ID_PLACEHOLDERS <- c("SINAUTOGENERADO", "**SIN AUTOGENER", "")
SETTINGS <- c("Outpatient", "Emergency", "Inpatient")

# ----------------------------------------------------------- import ---

# The three care settings use different column names; harmonise them.
read_setting <- function(setting, year) {
  spec <- switch(setting,
    Outpatient = list(file = "%d_onco_essi_consult_externa_v2.csv",
                      cols = c(tipdoc_paciente = "TIPDOC_PACIENTE", doc_paciente = "DOC_PACIENTE",
                               autogenerado = "AUTOGENERADO", ubigeo_domic = "UBIGEO_DOMIC",
                               fecha_atencion = "FECHA_ATENCION", diagnostico = "DIAGNOSTICO")),
    Emergency  = list(file = "%d_onco_essi_emer_v2.csv",
                      cols = c(tipdoc_paciente = "TIPODOC", doc_paciente = "DNI",
                               autogenerado = "AUTOGENERADO", ubigeo_domic = "UBIGEO_DOMIC",
                               fecha_atencion = "FECHA_ATEN", diagnostico = "DIAGNOSTICO")),
    Inpatient  = list(file = "%d_onco_essi_hosp_v2.csv",
                      cols = c(doc_paciente = "DOC_PAC", autogenerado = "AUTOGENERADO_PAC",
                               fecha_atencion = "FECATEN", diagnostico = "CODDX"))
  )
  cols <- c(spec$cols, red_asistencial = "RED_ASISTENCIAL", age_year = "ANNOS", sex = "SEXO")
  rio::import(raw_file(sprintf(spec$file, year))) |>
    dplyr::select(dplyr::all_of(cols)) |>
    dplyr::mutate(dplyr::across(-age_year, as.character),
                  age_year = suppressWarnings(as.integer(age_year)),
                  fecha_atencion = suppressWarnings(lubridate::dmy(fecha_atencion)),
                  setting = setting, file_year = year)
}

read_ehr_records <- function() {
  out <- tidyr::expand_grid(setting = SETTINGS, year = 2019:2022) |>
    purrr::pmap(read_setting) |>
    purrr::list_rbind()
  out |>
    dplyr::mutate(
      # Document type codes differ between settings.
      tipdoc_paciente = dplyr::case_when(
        setting == "Outpatient" & tipdoc_paciente %in% c("CEXT/PAS", "D.N.I.", "NEONATO",
                                                         "PASAPORT", "PTP.") ~ tipdoc_paciente,
        setting == "Emergency" ~ c("1" = "D.N.I.", "2" = "CEXT/PAS", "3" = "PASAPORT",
                                   "4" = "D.I.EXT.", "5" = "PTP.", "8" = "NEONATO")[tipdoc_paciente],
        TRUE ~ NA_character_),
      doc_paciente = dplyr::if_else(setting == "Emergency",
                                    stringr::str_replace(doc_paciente, "^[0-9]+-", ""), doc_paciente),
      rid = dplyr::row_number(),
      setting = factor(setting, SETTINGS),
      red = stringr::str_squish(stringr::str_remove(red_asistencial, "^RED (ASISTENCIAL|PRESTACIONAL) ")),
      red = dplyr::if_else(is.na(red) | stringr::str_detect(red, "[0-9]") |
                             !stringr::str_detect(red, "[A-Z]{3}"), "Malformed row", red)
    )
}

# --------------------------------------------- linkage and exclusions ---

valid_ubigeo <- function(x) {
  x <- stringr::str_pad(x, 6, pad = "0")
  dplyr::if_else(stringr::str_detect(x, "^[0-9]+$") & x != "000000", x, NA_character_)
}

# Inpatient extracts lack document type and district of residence; both are
# taken from the earliest outpatient/emergency record with the same insured
# ID code. Each record is then assigned the first exclusion step it fails.
link_and_flag <- function(rec) {
  no_id <- rec$setting != "Inpatient" & rec$autogenerado %in% ID_PLACEHOLDERS

  lookup <- rec[!no_id & rec$setting != "Inpatient", ] |>
    dplyr::arrange(autogenerado, fecha_atencion) |>
    dplyr::distinct(autogenerado, .keep_all = TRUE) |>
    dplyr::select(autogenerado, tipdoc_link = tipdoc_paciente, doc_link = doc_paciente,
                  ubigeo_link = ubigeo_domic)

  linked <- rec |>
    dplyr::filter(setting == "Inpatient") |>
    dplyr::select(rid, autogenerado) |>
    dplyr::left_join(lookup, by = "autogenerado", relationship = "many-to-one") |>
    dplyr::select(-autogenerado)

  rec |>
    dplyr::left_join(linked, by = "rid") |>
    dplyr::mutate(
      inpatient = setting == "Inpatient",
      id_missing = is.na(autogenerado) | autogenerado %in% ID_PLACEHOLDERS,
      dx = dplyr::if_else(is.na(diagnostico) | stringr::str_trim(diagnostico) == "" |
                            diagnostico %in% c("N", "R"), NA_character_, diagnostico),
      sex = dplyr::if_else(sex %in% c("F", "M"), sex, NA_character_),
      tipdoc = dplyr::if_else(inpatient, tipdoc_link, tipdoc_paciente),
      doc = suppressWarnings(as.integer(dplyr::if_else(inpatient, doc_link, doc_paciente))),
      ubigeo = valid_ubigeo(dplyr::if_else(inpatient, ubigeo_link, ubigeo_domic)),
      inpatient_linked = inpatient & !is.na(tipdoc_link),
      step = dplyr::case_when(
        no_id                                   ~ "No insured ID code",
        is.na(dx)                               ~ "Missing ICD-10 code",
        is.na(tipdoc) | is.na(doc)              ~ "Identity document missing or not linkable",
        is.na(fecha_atencion) |
          fecha_atencion > as.Date("2022-12-31") ~ "Visit date invalid or after 2022",
        TRUE                                    ~ "Retained"),
      step = factor(step, c("No insured ID code", "Missing ICD-10 code",
                            "Identity document missing or not linkable",
                            "Visit date invalid or after 2022", "Retained"))
    ) |>
    dplyr::select(-tipdoc_link, -doc_link, -ubigeo_link)
}

# ------------------------------------------------------------ deaths ---

read_sinadef <- function() {
  rio::import(raw_file("sinadef2017_2023.csv")) |>
    janitor::clean_names() |>
    dplyr::transmute(
      tipdoc = dplyr::case_when(
        tipo_doc == "DNI/LE" ~ "D.N.I.",
        tipo_doc %in% c("CARNET DE EXTRANJERIA", "PASAPORTE") ~ "CEXT/PAS",
        tipo_doc == "DI DEL EXTRANJERO" ~ "D.I.EXT.",
        tipo_doc == "ACTA DE NACIMIENTO" ~ "NEONATO",
        tipo_doc == "LM/BOL" ~ "LM/BOL",
        TRUE ~ NA_character_),
      doc = suppressWarnings(as.integer(documento)),
      sex_sinadef = c(FEMENINO = "F", MASCULINO = "M")[sexo_sinadef],
      age_sinadef = suppressWarnings(as.integer(edad_sinadef)),
      fallecido, death_date = as.Date(fech_fallecimiento)) |>
    tidyr::drop_na(tipdoc)
}

# ------------------------------------------------------- case pool ---

build_case_pool <- function(rec, sinadef) {
  visits <- rec |>
    dplyr::filter(step == "Retained") |>
    dplyr::select(rid, tipdoc, doc, fecha_atencion, ubigeo_domic = ubigeo, age_year, sex) |>
    dplyr::left_join(sinadef, by = c("tipdoc", "doc"), relationship = "many-to-many") |>
    dplyr::mutate(
      # A death recorded before the visit is treated as an erroneous link.
      wrong_link = !is.na(death_date) & death_date < fecha_atencion,
      dplyr::across(c(sex_sinadef, age_sinadef, fallecido, death_date),
                    ~ dplyr::if_else(wrong_link, NA, .x)),
      age_year = dplyr::coalesce(age_year, age_sinadef),
      sex = dplyr::coalesce(sex, sex_sinadef),
      death_date = dplyr::if_else(dplyr::coalesce(fallecido, "no") == "si" &
                                    death_date <= as.Date("2022-12-31"), death_date, as.Date(NA)),
      year = lubridate::year(fecha_atencion)) |>
    dplyr::arrange(tipdoc, doc, fecha_atencion) |>
    dplyr::group_by(year, tipdoc, doc) |>
    dplyr::slice(1) |>
    dplyr::ungroup()

  died_by <- function(d, y) dplyr::if_else(!is.na(d$death_date) &
                                             d$death_date <= as.Date(sprintf("%d-12-31", y)), 1L, 0L)

  # Vital status is evaluated for people with a visit in the year; people
  # carried forward keep the status assigned when they were last seen.
  pools <- list()
  for (y in 2019:2022) {
    new <- visits |> dplyr::filter(fecha_atencion <= as.Date(sprintf("%d-12-31", y)))
    if (y > 2019) new <- new |> dplyr::filter(fecha_atencion >= as.Date(sprintf("%d-01-01", y)))
    new <- new |> dplyr::mutate(died_in_year = died_by(new, y), carried = FALSE)
    if (y > 2019) {
      carried <- pools[[as.character(y - 1)]] |>
        dplyr::filter(died_in_year == 0L) |>
        dplyr::mutate(age_year = age_year + 1, year = year + 1, carried = TRUE)
      new <- dplyr::bind_rows(carried, new) |>
        dplyr::arrange(tipdoc, doc, dplyr::desc(fecha_atencion)) |>
        dplyr::group_by(tipdoc, doc) |>
        dplyr::slice(1) |>
        dplyr::ungroup()
    }
    pools[[as.character(y)]] <- dplyr::mutate(new, year = y)
  }
  dplyr::bind_rows(pools)
}
