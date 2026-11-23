# Run the complete public analysis in order.
#   Rscript scripts/00_run_all.R
# Scripts in scripts/preprocessing/ need the individual-level EsSalud records
# and are not run here (see README).

scripts <- c(
  "00_download_data.R",
  "10_table1.R",
  "11_figure1.R",
  "12_figure2.R",
  "13_figure4.R",
  "14_figure5.R",
  "20_adjacency_matrix.R",
  "21_main_model.R",
  "22_figure3.R",
  "30_model_assessment.R",
  "31_sensitivity_low_expected.R",
  "32_sensitivity_standardised.R"
)

for (s in scripts) {
  message("\n==== ", s, " ====")
  t0 <- Sys.time()
  source(here::here("scripts", s), local = new.env())
  message("finished in ", format(round(Sys.time() - t0, 1)))
}
