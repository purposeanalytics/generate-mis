#' Main function to generate MIS tables
#'
#' @param data_folder
#' @param report_date The date the run is "as at". Open statuses run to the day after this
#'   date and only discharges on or before it are counted. Defaults to today; pass an
#'   earlier date to reproduce a past run.
#' @param debug
#'
#' @return Invisibly, a list of the four tables written to `data/processed`.
#' @export
#'
#' @examples
generate_tables <- function(data_folder = here::here("data"),
                            fc_450_version = "verA",
                            fc_455_version = "verA",
                            fc_506_version = "verA",
                            ir_version = "verCMHA",
                            report_date = lubridate::today(),
                            debug = FALSE) {

  report_date <- as.Date(report_date)

  if(debug == TRUE) {
    rlog::log_warn("Running in debug mode")
  }

  tictoc::tic()

  processed_data <- process_data(data_folder, debug, report_date)
  census_counts <- generate_census_counts(processed_data, fc_455_version, fc_450_version)
  visit_and_service_counts <- generate_visit_and_service_counts(processed_data, fc_450_version, fc_506_version, ir_version)

  rlog::log_info("MIS table generation complete!")

  tictoc::toc()

  invisible(c(census_counts, visit_and_service_counts))
}
