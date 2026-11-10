#' Main function to generate MIS tables
#'
#' @param data_folder
#' @param report_date The date the run is "as at". Open statuses run to the day after this
#'   date and only discharges on or before it are counted. Defaults to today; pass an
#'   earlier date to reproduce a past run.
#' @param hold_day_version How the day a client goes on hold is counted in the daily census
#'   (S403 resident days, and other daily statistics):
#'   - `"verA"` (default): an Active status ends the day before the next status starts,
#'     whatever it is, so the day a client goes on hold is not counted as Active.
#'   - `"verB"`: the same for a discharge, but when an Active status is followed by On Hold,
#'     the day the client goes on hold is still counted as Active (a hold is a temporary
#'     absence, not a separation).
#' @param debug
#'
#' @return Invisibly, a list of the four tables written to `data/processed`:
#'   - `mis_detail_by_fc` and `mis_detail_by_service`: one row per counted unit for every
#'     statistic (a client day for census statistics; an episode, visit or session for the
#'     others), with the client. They name clients, so restrict who can see them.
#'   - `mis_visit_and_service_counts_by_fc` and `mis_visit_and_service_counts_by_service`:
#'     the visit and service statistics summed by date, reporting unit and account.
#' @export
#'
#' @examples
generate_tables <- function(data_folder = here::here("data"),
                            fc_450_version = "verA",
                            fc_455_version = "verA",
                            fc_506_version = "verA",
                            ir_version = "verCMHA",
                            report_date = lubridate::today(),
                            hold_day_version = "verA",
                            debug = FALSE) {

  report_date <- as.Date(report_date)

  if(debug == TRUE) {
    rlog::log_warn("Running in debug mode")
  }

  tictoc::tic()

  processed_data <- process_data(data_folder, debug, report_date, hold_day_version)
  census_counts <- generate_census_counts(processed_data, fc_455_version, fc_450_version)
  visit_and_service_counts <- generate_visit_and_service_counts(processed_data, fc_450_version, fc_506_version, ir_version)

  # One detail table per grain: the census rows, then the visit and service rows
  details <- list(
    mis_detail_by_fc = dplyr::bind_rows(census_counts$fc, visit_and_service_counts$detail_by_fc),
    mis_detail_by_service = dplyr::bind_rows(census_counts$service, visit_and_service_counts$detail_by_service)
  )
  for (name in names(details)) {
    rlog::log_info(glue::glue("Writing {name}.csv (contains client IDs)"))
    readr::write_csv(details[[name]], file.path(data_folder, "processed", paste0(name, ".csv")), na = "")
  }
  summaries <- visit_and_service_counts[c("mis_visit_and_service_counts_by_fc", "mis_visit_and_service_counts_by_service")]

  rlog::log_info("MIS table generation complete!")

  tictoc::toc()

  invisible(c(details, summaries))
}
