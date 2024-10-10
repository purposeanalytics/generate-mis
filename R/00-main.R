#' Main function to generate MIS tables
#'
#' @param data_folder
#' @param debug
#'
#' @return
#' @export
#'
#' @examples
generate_tables <- function(data_folder = here::here("data"),
                            fc_450_version = "verA",
                            fc_455_version = "verA",
                            fc_506_version = "verA",
                            debug = FALSE) {

  if(debug == TRUE) {
    rlog::log_warn("Running in debug mode")
  }

  tictoc::tic()

  suppressWarnings({
    processed_data <- process_data(data_folder, debug)
    generate_census_counts(processed_data, fc_455_version)
    generate_visit_and_service_counts(processed_data, fc_506_version)
  })

  rlog::log_info("MIS table generation complete!")

  tictoc::toc()
}
