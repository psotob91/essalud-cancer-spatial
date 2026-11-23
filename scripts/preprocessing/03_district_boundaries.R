# District and department boundaries.
#
# Builds the boundary files deposited in figshare from the INEI political
# boundaries (2023 edition, distributed by GeoGPS Peru,
# https://www.geogpsperu.com/p/descargas.html). District codes are INEI ubigeo
# codes; area-level indicators are added from the `ubigeo` R package
# (https://github.com/jmcastagnetto/ubigeo).
#
# Input (directory set by BOUNDARIES_DIR, default data/boundaries/):
#   DEPARTAMENTOS_inei_geogpsperu_suyopomalia/*.shp
#   PROVINCIAS_inei_geogpsperu_suyopomalia/*.shp
#   Distrital INEI 2023 geogpsperu SuyoPomalia/*.shp
# Output: output/preprocessing/shapefile_peru_distritos.shp,
#         shapefile_peru_dptos.shp, shapefile_peru_dptos_limaprov.shp

suppressPackageStartupMessages({ library(tidyverse); library(here); library(sf); library(janitor) })
source(here("R", "functions.R"))

src <- function(...) file.path(Sys.getenv("BOUNDARIES_DIR", here("data", "boundaries")), ...)
out <- function(f) path_output("preprocessing", f)

departments <- st_read(src("DEPARTAMENTOS_inei_geogpsperu_suyopomalia",
                           "DEPARTAMENTOS_inei_geogpsperu_suyopomalia.shp"), quiet = TRUE)
provinces   <- st_read(src("PROVINCIAS_inei_geogpsperu_suyopomalia",
                           "PROVINCIAS_inei_geogpsperu_suyopomalia.shp"), quiet = TRUE)

# Lima is split into Lima Metropolitana (province of Lima) and Lima Provincias
# (the remaining provinces of the department), as in EsSalud reporting.
lima_metro <- provinces |>
  filter(NOMBPROV == "LIMA") |>
  transmute(NOMBDEP = "LIMA METROPOLITANA")
lima_provinces <- tibble(
  NOMBDEP = "LIMA PROVINCIAS",
  geometry = provinces |> filter(NOMBDEP == "LIMA", NOMBPROV != "LIMA") |>
    st_union() |> st_combine()
) |> st_as_sf()

departments_lima_split <- departments |>
  select(NOMBDEP) |>
  filter(NOMBDEP != "LIMA") |>
  bind_rows(lima_metro, lima_provinces)

data(ubigeo_district, package = "ubigeo")
districts <- st_read(src("Distrital INEI 2023 geogpsperu SuyoPomalia",
                         "Distrital INEI 2023 geogpsperu SuyoPomalia.shp"), quiet = TRUE) |>
  select(DEPARTAMEN, PROVINCIA, DISTRITO, UBIGEO) |>
  clean_names() |>
  left_join(ubigeo_district |>
              select(inei, reniec, surface, pop_density_2020, food_vulnerability_index,
                     hdi_2019, total_poverty_pct, extreme_poverty_pct),
            by = c("ubigeo" = "inei"))

st_write(departments_lima_split, out("shapefile_peru_dptos_limaprov.shp"), delete_dsn = TRUE, quiet = TRUE)
st_write(departments,            out("shapefile_peru_dptos.shp"),          delete_dsn = TRUE, quiet = TRUE)
st_write(districts,              out("shapefile_peru_distritos.shp"),      delete_dsn = TRUE, quiet = TRUE)
write_session_info("03_district_boundaries")
