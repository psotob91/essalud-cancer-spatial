# Shared functions used by the analysis scripts.
# Sourced at the top of every script in scripts/.

# ---------------------------------------------------------------- paths ---

path_data   <- function(...) here::here("data", ...)
path_output <- function(...) {
  p <- here::here("output", ...)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  p
}

# --------------------------------------------------------- district data ---

# The insured-population columns are stored as text with a thousands
# separator ("17,470"); convert them before any arithmetic.
parse_population <- function(df) {
  dplyr::mutate(df, dplyr::across(
    dplyr::any_of(c("total", "hombre", "mujer")),
    ~ as.integer(stringr::str_replace_all(.x, ",", ""))
  ))
}

# District-year panel used in all analyses: districts present in the
# insured-population file in all four years (2019-2022). District 211302 is
# excluded because it has no counterpart in the 2023 INEI boundary file.
# For files with one row per cancer type, pass `by = "tipo_cancer"` so that
# the rule is applied within each type.
select_complete_districts <- function(df, by = character()) {
  df |>
    parse_population() |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(by, "ubigeo_domic", "year")))) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(by, "ubigeo_domic")))) |>
    dplyr::mutate(nyears = dplyr::row_number(), maxyears = max(nyears)) |>
    dplyr::ungroup() |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(by, "ubigeo_domic", "year"))), .keep_all = TRUE) |>
    dplyr::filter(nyears != 5, maxyears >= 4, ubigeo_domic != "211302") |>
    dplyr::select(-nyears, -maxyears)
}

read_cases  <- function() select_complete_districts(readRDS(path_data("bd_casos.rds")))
read_deaths <- function() select_complete_districts(readRDS(path_data("bd_muerte.rds")))

# District boundaries (INEI 2023); district names come from the case files.
read_district_map <- function() {
  sf::st_read(path_data("shapefile_peru_distritos.shp"), quiet = TRUE) |>
    dplyr::select(ubigeo_domic = ubigeo)
}

read_department_map <- function() {
  sf::st_read(path_data("shapefile_peru_dptos.shp"), quiet = TRUE)
}

# -------------------------------------------------------- crude measures ---

# Crude rate per 100,000 with exact (Garwood) Poisson 95% confidence limits.
crude_rate <- function(cases, population, per = 1e5) {
  tibble::tibble(
    rate  = per * cases / population,
    lower = per * 0.5 * stats::qchisq(0.025, 2 * cases) / population,
    upper = per * 0.5 * stats::qchisq(0.975, 2 * cases + 2) / population
  )
}

# ------------------------------------------------------- spatial model ---

# Expected counts: overall crude contact prevalence of the insured
# population applied to each district-year population (no strata).
expected_counts <- function(population, cases) {
  SpatialEpi::expected(population = population, cases = cases, n.strata = 1)
}

# Queen contiguity (shared border or vertex) between district polygons,
# written in INLA graph format. Node i corresponds to row i of `map`.
build_adjacency <- function(map, file) {
  map$geometry <- sf::st_make_valid(map$geometry)
  nb <- spdep::poly2nb(map, queen = TRUE)
  spdep::nb2INLA(file, nb)
  list(nb = nb, graph = INLA::inla.read.graph(filename = file))
}

# Hyperpriors, stated explicitly. They equal the INLA defaults, so the
# fitted model is identical to one specified without `hyper`.
#   BYM unstructured and spatial precisions: log-gamma(shape = 1, rate = 5e-4)
#   year iid precision:                      log-gamma(shape = 1, rate = 5e-5)
#   intercept:                               flat (precision 0)
priors <- list(
  bym = list(prec.unstruct = list(prior = "loggamma", param = c(1, 5e-4)),
             prec.spatial  = list(prior = "loggamma", param = c(1, 5e-4))),
  iid = list(prec = list(prior = "loggamma", param = c(1, 5e-5)))
)

# log(theta_ij) = alpha + u_i + v_i + gamma_j
#   u_i: spatially structured (intrinsic CAR) effect of district i
#   v_i: unstructured (iid) effect of district i
#   gamma_j: iid effect of year j
bym_formula <- function(response = "ncasos") {
  stats::as.formula(paste(
    response,
    "~ f(idarea, model = 'bym', graph = g, hyper = priors$bym)",
    "+ f(idtime, model = 'iid', hyper = priors$iid)"
  ))
}

fit_bym <- function(data, graph, offset = NULL, family = "poisson", ntrials = NULL,
                    response = "ncasos", config = FALSE) {
  g <- graph
  args <- list(
    formula = bym_formula(response), family = family, data = data,
    control.fixed = list(mean.intercept = 0, prec.intercept = 0),
    control.predictor = list(compute = TRUE),
    control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = config)
  )
  if (!is.null(offset))  args$E <- offset
  if (!is.null(ntrials)) args$Ntrials <- ntrials
  environment(args$formula) <- environment()
  do.call(INLA::inla, args)
}

# ----------------------------------------------------------- utilities ---

write_session_info <- function(script) {
  writeLines(utils::capture.output(utils::sessionInfo()),
             path_output("logs", paste0(script, "_sessionInfo.txt")))
}

# --------------------------------------------------------- cancer types ---

# English labels for the GLOBOCAN-based cancer groups used in the data files.
cancer_type_en <- c(
  "Labio, Cavidad oral" = "Lip, oral cavity", "Glándulas salivares" = "Salivary glands",
  "Orofaringe" = "Oropharynx", "Nasofaringe" = "Nasopharynx", "Hipofaringe" = "Hypopharynx",
  "Esófago" = "Oesophagus", "Estómago" = "Stomach", "Colorrectal (incluye ano)" = "Colorectum (incl. anus)",
  "Hígado" = "Liver", "Vesícula biliar" = "Gallbladder", "Páncreas" = "Pancreas",
  "Laringe" = "Larynx", "Pulmón (incluyendo tráquea)" = "Lung (incl. trachea)",
  "Melanoma de piel" = "Melanoma of skin", "Cáncer de piel no melanómico" = "Non-melanoma skin cancer",
  "Mesotelioma" = "Mesothelioma", "Sarcoma de Kaposi" = "Kaposi sarcoma",
  "Mama (sólo en mujeres)" = "Breast (women)", "Vulva" = "Vulva", "Vagina" = "Vagina",
  "Cuello uterino" = "Cervix uteri", "Cuerpo uterino" = "Corpus uteri", "Ovario" = "Ovary",
  "Pene" = "Penis", "Próstata" = "Prostate", "Testículos" = "Testis",
  "Riñón" = "Kidney", "Vejiga" = "Bladder", "Cerebro y sistema nervioso central" = "Brain, central nervous system",
  "Tiroides" = "Thyroid", "Linfoma de Hodgkin" = "Hodgkin lymphoma",
  "Linfoma no Hodgkin" = "Non-Hodgkin lymphoma",
  "Mieloma múltiple (incluidas las enfermedades inmunoproliferativas)" = "Multiple myeloma (incl. immunoproliferative diseases)",
  "Leucemia" = "Leukaemia", "Otro tipo de cáncer" = "Other cancers"
)

female_sites <- c("Mama (sólo en mujeres)", "Cuello uterino", "Ovario", "Cuerpo uterino",
                  "Vulva", "Vagina")
male_sites   <- c("Próstata", "Pene", "Testículos")
