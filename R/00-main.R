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
#' @param detail If TRUE, also write the detail tables (`mis_detail_by_fc.csv` and
#'   `mis_detail_by_service.csv`) to `data/processed`. They name the clients behind each
#'   statistic, so they are off by default; share them only with people who check the data.
#'
#' @return Invisibly, a list of the four tables written to `data/processed`, plus
#'   `mis_detail_by_fc` and `mis_detail_by_service`: the rows behind the visit and service
#'   counts, one per counted unit (episode, visit or session). These are written to files only
#'   when `detail = TRUE`. The census tables are already one row per client per day.
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
                            debug = FALSE,
                            detail = FALSE) {

  report_date <- as.Date(report_date)

  if(debug == TRUE) {
    rlog::log_warn("Running in debug mode")
  }

  tictoc::tic()

  processed_data <- process_data(data_folder, debug, report_date, hold_day_version)
  census_counts <- generate_census_counts(processed_data, fc_455_version, fc_450_version)
  visit_and_service_counts <- generate_visit_and_service_counts(processed_data, fc_450_version, fc_506_version, ir_version)

  # visit and service detail; the census tables are already one row per client per day
  details <- visit_and_service_counts[c("mis_detail_by_fc", "mis_detail_by_service")]
  summaries <- c(census_counts, visit_and_service_counts[setdiff(names(visit_and_service_counts), names(details))])

  if (isTRUE(detail)) {
    for (name in names(details)) {
      rlog::log_info(glue::glue("Writing {name}.csv (contains client IDs)"))
      readr::write_csv(details[[name]], file.path(data_folder, "processed", paste0(name, ".csv")), na = "")
    }
  }

  rlog::log_info("MIS table generation complete!")

  tictoc::toc()

  invisible(c(summaries, details))
}
