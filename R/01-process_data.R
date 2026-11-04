#' Process data
#'
#' @param data_folder
#'
#' @return
#' @export
#'
#' @examples
process_data <- function(data_folder, debug = FALSE, report_date = lubridate::today()){

  # Read data ----

  rlog::log_info("Reading interim data files")

  participants <- readr::read_rds(paste0(data_folder, "/interim/rds/participants.rds"))
  services <- readr::read_rds(paste0(data_folder, "/interim/rds/services.rds"))
  activities <- readr::read_rds(paste0(data_folder, "/interim/rds/activities.rds"))
  functional_centre_mapping <- readr::read_rds(paste0(data_folder, "/interim/rds/functional_centre_mapping.rds"))

  # Stop on missing required columns; warn and fill blanks for missing expected ones
  # (see input_columns in R/input_columns.R)
  participants <- check_input_columns(participants, "participants")
  services <- check_input_columns(services, "services")
  activities <- check_input_columns(activities, "activities")
  functional_centre_mapping <- check_input_columns(functional_centre_mapping, "functional_centre_mapping")

  # Nothing should be dated after report_date; drop anything that is, so a run reflects
  # the data as at report_date. Statuses that start later are removed, and decision and
  # first-service dates that fall later are treated as not yet known.
  services <- services |>
    dplyr::filter(service_status_start_date <= report_date) |>
    dplyr::mutate(service_decision_date = dplyr::if_else(service_decision_date > report_date, lubridate::NA_Date_, service_decision_date),
                  service_first_direct_activity = dplyr::if_else(service_first_direct_activity > report_date, lubridate::NA_Date_, service_first_direct_activity))

  activities <- activities |>
    dplyr::filter(activity_date <= report_date)


  # Build service list ----

  service_list <- all_functional_centres_and_statistical_codes |>
    dplyr::right_join(functional_centre_mapping, by = c("funder_service_code")) |>
    dplyr::mutate(funder_target_category = NA,
           funder_target_category = dplyr::if_else(stringr::str_detect(funder_statistical_account_category, "248"), "Meals Delivered-Combined", funder_target_category ),
           funder_target_category = dplyr::if_else(stringr::str_detect(funder_statistical_account_category, "266 00"), "Service Provider Group Interactions", funder_target_category ),
           funder_target_category = dplyr::if_else(stringr::str_detect(funder_statistical_account_category, "265"), "Service Provider Interactions", funder_target_category ),
           funder_target_category = dplyr::if_else(stringr::str_detect(funder_statistical_account_category, "403"), "Inpatient/Resident Days", funder_target_category ),
           funder_target_category = dplyr::if_else(stringr::str_detect(funder_statistical_account_category, "(450|451)"), "Visits", funder_target_category ),
           funder_target_category = dplyr::if_else(stringr::str_detect(funder_statistical_account_category, "452"), "Not Uniquely Identified Service Recipient Interactions", funder_target_category ),
           funder_target_category = dplyr::if_else(stringr::str_detect(funder_statistical_account_category, "(453|454)"), "Hours of Care", funder_target_category ),
           funder_target_category = dplyr::if_else(stringr::str_detect(funder_statistical_account_category, "455"), "Individuals Served by Functional Centre", funder_target_category ),
           funder_target_category = dplyr::if_else(stringr::str_detect(funder_statistical_account_category, "492 00"), "Group Sessions", funder_target_category ),
           funder_target_category = dplyr::if_else(stringr::str_detect(funder_statistical_account_category, "483"), "Attendance Days", funder_target_category ),
           funder_target_category = dplyr::if_else(stringr::str_detect(funder_statistical_account_category, "491"), "Group Participant Attendances", funder_target_category ),
           funder_target_key = dplyr::if_else(!is.na(funder_target_category), glue::glue("{funder_service_code}_{funder_target_category}"), NA_character_)) |>
    dplyr::filter(!is.na(funder_service_name))

  readr::write_csv(service_list, paste0(data_folder, "/processed/service_list.csv"), na = "")


  # Build MIS tables ----


  rlog::log_info("Building MIS service table")

  # get list of non-admissions
  non_admissions <- services |>
    dplyr::filter(service_status == "Not Admitted") |>
    dplyr::distinct(service_enrollment_id) |>
    dplyr::pull()

  mis_service_history <- services |>
    # filter on Alayacare or TELUS eligibility criteria
    dplyr::filter(!stringr::str_detect(service_id, "--99") & (stringr::str_detect(service_id, "--80") | stringr::str_sub(funder_service_code, 2, 2) == "2" | stringr::str_sub(funder_service_code, 2, 2) == "3")) |>
    dplyr::filter(!service_enrollment_id %in% non_admissions) |>

    # remove "Waiting for Service Initiation" as this is only counted for time spent on waitlist; waiting for assessment then becomes the time from referral to decision date
    dplyr::filter(service_status != "Waiting for Service Initiation") |>
    dplyr::mutate(service_status = factor(service_status, levels = c("Waiting for Assessment",
                                                              "On Waitlist",
                                                              "On Hold",
                                                              "Not Admitted",
                                                              "Active",
                                                              "Inactive"))) |>

    # drop Active statuses corrected the same day (uses the recorded end dates)
    drop_same_day_corrections() |>

    # reset service_start_end_date
    dplyr::group_by(service_enrollment_id) |>
    dplyr::arrange(service_enrollment_id, service_status_start_date, service_status) |>
    dplyr::mutate(service_status_end_date = dplyr::lead(service_status_start_date)) |>
    dplyr::ungroup() |>
    dplyr::mutate(service_status_end_date = dplyr::coalesce(service_status_end_date, report_date + lubridate::days(1))) |>
    number_episodes()

  warn_missing_decision_dates(mis_service_history)

  if(!("participant_preferred_language" %in% colnames(mis_service_history))){
    mis_service_history <- mis_service_history |>
      dplyr::mutate(participant_preferred_language = NA_character_)
  }

  # to speed up debugging
  if(debug == TRUE) {
    mis_service_history <- mis_service_history |>
      dplyr::slice_tail(n = 10000)
  }

  rlog::log_info("Building MIS visits table")

  mis_visits <- activities |>
    dplyr::filter(!stringr::str_detect(service_id, "--99") & (stringr::str_detect(service_id, "--80") | stringr::str_sub(funder_service_code, 2, 2) == "2" | stringr::str_sub(funder_service_code, 2, 2) == "3")) |>
    dplyr::filter(activity_status == "Completed" & participant_id != "Unregistered") |>
    dplyr::filter(activity_type %in% c("Face-to-face", "Face-to-face Virtual", "Non-face-to-face", "Assessment", "Meal delivery") | activity_individual_group == "Group" | funder_service_code == "72 5 82 10") |>

    # include all activity_durations for Transportation
    dplyr::mutate(activity_duration = dplyr::if_else(funder_service_code == "72 5 82 14", dplyr::if_else(activity_duration < 5, 6, activity_duration), activity_duration))


  rlog::log_info("Building MIS unregistered interactions table")

  mis_unregistered_interactions <- activities |>
    dplyr::filter(stringr::str_detect(service_id, "--80") | stringr::str_sub(funder_service_code, 2, 2) == "2" | stringr::str_sub(funder_service_code, 2, 2) == "3") |>
    dplyr::filter(activity_status == "Completed" & participant_id == "Unregistered")


  rlog::log_info("Building MIS daily census table")

  mis_daily_census <- mis_service_history |>
    dplyr::filter(service_status != "Inactive") |>
    dplyr::filter(is.na(service_status_end_date) | service_status_end_date >= dashboard_start_date) |>
    dplyr::mutate(start_date = dplyr::if_else(service_status_start_date < dashboard_start_date, dashboard_start_date, service_status_start_date),
           end_date = dplyr::if_else(service_status_end_date > report_date | is.na(service_status_end_date), report_date + lubridate::days(1), service_status_end_date),

           # for active period, make end_date day before discharge date except when start and end are same day
           end_date = dplyr::if_else(service_status == "Active", end_date - lubridate::days(1), end_date),
           end_date = dplyr::if_else(end_date <= start_date, start_date, end_date)) |>
    dplyr::select(dplyr::any_of(c("participant_id", "service_enrollment_id", "service_id", "service_name", "funder_service_code", "service_status", "service_status_start_date", "start_date", "end_date", "service_worker_id"))) |>

    # list all dates between start and end and then split into rows
    dplyr::mutate(date = purrr::pmap(list(x = start_date, y = end_date), function(x, y) seq.Date(x, y, "1 day"))) |>
    tidyr::unnest(date) |>

    dplyr::select(-start_date, -end_date) |>
    dplyr::mutate(service_status_duration = lubridate::interval(service_status_start_date, date)/lubridate::days(1)) |>

    # calculate age on date
    dplyr::inner_join(participants |>
                        dplyr::select(participant_id, participant_date_of_birth, participant_ohrs_special_needs, participant_preferred_language), by = "participant_id") |>
    fast_age_calculation(participant_date_of_birth, date)

  processed_data <- list(data_folder = data_folder,
                         report_date = report_date,
                         mis_service_history = mis_service_history,
                         mis_visits = mis_visits,
                         mis_unregistered_interactions = mis_unregistered_interactions,
                         mis_daily_census = mis_daily_census,
                         service_list = service_list)

  return(processed_data)

}
