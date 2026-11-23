# Model dataset and adjacency structure.
#
# Builds the district-year dataset for the spatial model (observed cases,
# insured population, expected counts) and the binary queen-contiguity
# adjacency matrix between the 1,841 districts, in INLA graph format.
#
# Input:  data/bd_casos.rds, data/bd_muerte.rds, data/shapefile_peru_distritos.shp
# Output: output/models/district_graph.adj      adjacency (INLA format)
#         output/models/model_data.rds          model dataset, map and neighbours
#         output/tables/adjacency_summary.csv

suppressPackageStartupMessages({
  library(tidyverse); library(here); library(sf); library(spdep); library(INLA)
})
source(here("R", "functions.R"))

cases  <- read_cases()  |> arrange(ubigeo_domic, year)
deaths <- read_deaths() |> arrange(ubigeo_domic, year)

cases  <- cases  |> mutate(E  = expected_counts(total, ncasos),
                           prev = ncasos / total, smr = ncasos / E)
deaths <- deaths |> mutate(Em = expected_counts(total, nmuertos),
                           mort = nmuertos / total)

district_map <- read_district_map()

# District-year rows with geometry, in map order then year.
map_cases  <- district_map |> inner_join(cases,  by = "ubigeo_domic") |> drop_na(total)
map_deaths <- district_map |> inner_join(deaths, by = "ubigeo_domic") |> drop_na(total)

# One polygon per district for the adjacency structure.
map_districts <- district_map |>
  filter(ubigeo_domic %in% map_cases$ubigeo_domic) |>
  arrange(ubigeo_domic)

adj <- build_adjacency(map_districts, path_output("models", "district_graph.adj"))

# idarea indexes districts in alphabetical ubigeo order, which is the node
# order of the graph built above.
model_data <- st_drop_geometry(map_cases) |>
  mutate(idarea = as.integer(factor(ubigeo_domic)),
         idtime = as.integer(year - min(year) + 1))
stopifnot(identical(levels(factor(model_data$ubigeo_domic)), map_districts$ubigeo_domic),
          adj$graph$n == n_distinct(model_data$idarea))

n_neigh <- card(adj$nb)
adjacency_summary <- tibble(
  districts = length(adj$nb),
  links = sum(n_neigh),
  districts_without_neighbours = sum(n_neigh == 0),
  neighbours_median = median(n_neigh),
  neighbours_min = min(n_neigh),
  neighbours_max = max(n_neigh),
  connected_components = n.comp.nb(adj$nb)$nc
)
write_csv(adjacency_summary, path_output("tables", "adjacency_summary.csv"))
print(as.data.frame(adjacency_summary))

saveRDS(list(model_data = model_data, map_cases = map_cases, map_deaths = map_deaths,
             map_districts = map_districts, nb = adj$nb),
        path_output("models", "model_data.rds"))
write_session_info("20_adjacency_matrix")
