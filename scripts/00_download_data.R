# Download the district-level data deposited in figshare
# (doi:10.6084/m9.figshare.32043702, CC BY 4.0) into data/.
#
# Input:  none (internet connection)
# Output: data/*.rds, data/*.shp (+ .dbf, .shx, .prj)

source(here::here("R", "functions.R"))

files <- c(
  "bd_casos.rds"                   = 63820668,
  "bd_casos_edad.rds"              = 63820671,
  "bd_casos_sexo.rds"              = 63820674,
  "bd_casos_tipos.rds"             = 63820677,
  "bd_casos_tipos_sexo.rds"        = 63820680,
  "bd_muerte.rds"                  = 63820686,
  "bd_muerte_edad.rds"             = 63820689,
  "bd_muerte_sexo.rds"             = 63820692,
  "shapefile_peru_distritos.dbf"   = 63820725,
  "shapefile_peru_distritos.prj"   = 63820728,
  "shapefile_peru_distritos.shp"   = 63820743,
  "shapefile_peru_distritos.shx"   = 63820734,
  "shapefile_peru_dptos.dbf"       = 63820737,
  "shapefile_peru_dptos.prj"       = 63820740,
  "shapefile_peru_dptos.shp"       = 63820749,
  "shapefile_peru_dptos.shx"       = 63820746
)

dir.create(path_data(), showWarnings = FALSE)
options(timeout = max(600, getOption("timeout")))

for (f in names(files)) {
  dest <- path_data(f)
  if (file.exists(dest)) next
  url <- paste0("https://ndownloader.figshare.com/files/", files[[f]])
  message("Downloading ", f)
  utils::download.file(url, dest, mode = "wb", quiet = TRUE)
}

missing <- names(files)[!file.exists(path_data(names(files)))]
if (length(missing) > 0) stop("Not downloaded: ", paste(missing, collapse = ", "))
message("All ", length(files), " files are in ", path_data())
