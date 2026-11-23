# data/

This folder is filled by `scripts/00_download_data.R` with the district-level
files deposited in figshare (<https://doi.org/10.6084/m9.figshare.32043702>,
CC BY 4.0). Its contents are not tracked by git.

| File | Content |
|---|---|
| `bd_casos.rds` | cancer contacts (`ncasos`) and insured population (`total`, `hombre`, `mujer`) by district (`ubigeo_domic`) and year |
| `bd_muerte.rds` | cancer deaths (`nmuertos`) among people with a recorded contact, by district and year |
| `bd_casos_sexo.rds`, `bd_muerte_sexo.rds` | as above, by sex |
| `bd_casos_edad.rds`, `bd_muerte_edad.rds` | as above, by five-year age group (`ageg`) |
| `bd_casos_tipos.rds`, `bd_casos_tipos_sexo.rds` | contacts by cancer type (`tipo_cancer`, GLOBOCAN groups), overall and by sex |
| `shapefile_peru_distritos.*` | district boundaries, INEI 2023 |
| `shapefile_peru_dptos.*` | department boundaries, INEI |

Population columns are stored as text with a thousands separator ("17,470");
`parse_population()` in `R/functions.R` converts them.

The individual-level source records used in `scripts/preprocessing/` are not
distributed (see the main README). When access has been granted, place them in
`data/restricted/` or point `ESSALUD_RAW_DIR` to their folder.
