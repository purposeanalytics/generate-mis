#' Main function to generate MIS tables
#'
#' @param data_folder
#' @param report_date The date the run is "as at". Open statuses run to the day after this
#'   date and only discharges on or before it are counted. Defaults to today; pass an
#'   earlier date to reproduce a past run.
#' @param debug
#' @param detail If TRUE, also write the detail tables (`mis_detail_by_fc.csv` and
#'   `mis_detail_by_service.csv`) to `data/processed`. They name the clients behind each
#'   statistic, so they are off by default; share them only with people who check the data.
#'
#' @return Invisibly, a list of the four tables written to `data/processed`, plus
#'   `mis_detail_by_fc` and `mis_detail_by_service`: the rows behind every statistic. Visit
#'   and service statistics have one row per counted unit (episode, visit or session);
#'   census statistics have one row per run of consecutive days for a client, within a
#'   month (`date` to `end_date`). These are written to files only when `detail = TRUE`.
#' @export
#'
#' @examples
generate_tables <- function(data_folder = here::here("data"),
                            fc_450_version = "verA",
                            fc_455_version = "verA",
                            fc_506_version = "verA",
                            ir_version = "verCMHA",
                            report_date = lubridate::today(),
                            debug = FALSE,
                            detail = FALSE) {

  report_date <- as.Date(report_date)

  if(debug == TRUE) {
    rlog::log_warn("Running in debug mode")
  }

  tictoc::tic()

  processed_data <- process_data(data_folder, debug, report_date)
  census_counts <- generate_census_counts(processed_data, fc_455_version, fc_450_version)
  visit_and_service_counts <- generate_visit_and_service_counts(processed_data, fc_450_version, fc_506_version, ir_version)

  # one detail table per grain, census and visit statistics together
  combine_detail <- function(grain) {
    name <- paste0("detail_by_", grain)
    label_detail(dplyr::bind_rows(census_counts[[name]], visit_and_service_counts[[name]]))
  }
  summaries <- c(census_counts, visit_and_service_counts)
  summaries <- summaries[!startsWith(names(summaries), "detail_by_")]

  details <- list(mis_detail_by_fc = combine_detail("fc"), mis_detail_by_service = combine_detail("service"))

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
