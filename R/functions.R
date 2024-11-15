#' Calculate age from birth date
#'
#' @param df
#' @param date_of_birth
#' @param reference_date
#'
#' @return
#' @export
#'
#' @examples
calculate_age <- function(df, date_of_birth, reference_date){
  df |>
    dplyr::mutate(participant_age = lubridate::interval({{date_of_birth}}, {{reference_date}}) / lubridate::years(1),
           participant_funder_age_group_code = cut(participant_age, breaks = c(0, 17, 65, 999), labels = c("60", "40", "20")),
           participant_funder_age_group_code = dplyr::coalesce(participant_funder_age_group_code, "90"),
           participant_age = floor(participant_age),
           .after = participant_date_of_birth) |>
    dplyr::select(-participant_date_of_birth)
}


#' Determine the service recipient code (statistical account digits 4 and 5) based on functional centre code
#'
#' @param fc_code
#'
#' @return
#' @export
#'
#' @examples
get_sr_code <- function(fc_code) {

  code <- dplyr::case_when(
    (stringr::str_sub({{fc_code}}, 9, 10) == "76" | stringr::str_sub({{fc_code}},  9, 10) == "78") & stringr::str_sub({{fc_code}}, 6, 7) == "40" ~ "45",
    (stringr::str_sub({{fc_code}}, 9, 10) == "76" | stringr::str_sub({{fc_code}},  9, 10) == "78") & stringr::str_sub({{fc_code}}, 6, 7) != "40" ~ "25",
    .default = "80")  # if not "76" (mental health) or "78" (addictions), default to "80" (community support services)
}


#' Determine functional centres that report in each statistical code
#'
#' @param df
#' @param statistical_code
#'
#' @return
#' @export
#'
#' @examples
filter_eligible <- function(df, statistical_code){

  eligible_fcs <- all_functional_centres_and_statistical_codes |>
    dplyr::filter(stringr::str_starts(funder_statistical_account_code, statistical_code)) |>
    dplyr::distinct(funder_service_code) |>
    dplyr::pull()

  df |>
    dplyr::filter(funder_service_code %in% eligible_fcs)

}


#' Get time intervals
#'
#' @param activity_duration
#'
#' @return
#' @export
#'
#' @examples
get_time_intervals <- function(activity_duration){
  dplyr::case_when(
    is.na(activity_duration) ~ "00",
    activity_duration == 0 ~ "00",
    activity_duration > 0 & activity_duration <=5 ~ "99",  # 0-5 min are not reported in MIS, hence "99" code
    activity_duration > 5 & activity_duration <=30 ~ "01",  # 5-30 min
    activity_duration > 30 & activity_duration <=60 ~ "02",  # 30-60 min
    activity_duration > 60 & activity_duration <=120 ~ "03",  # 1-2 hours
    activity_duration > 120 & activity_duration <=300 ~ "04",  # 2-5 hours
    activity_duration > 300  ~ "05",  # over 5 hours
    .default = "99"
  )
}


#' Combine parts to create statistical account code
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
assemble_statistical_account <- function(df){

  df |>
    dplyr::mutate(funder_statistical_account_code = paste(digits_1_3, digits_4_5, digits_6_7))

}


#' Union overlapping program enrollments
#'
#' @param df
#' @param dimension_start
#' @param dimension_end
#' @param report_end
#' @param merge_level
#'
#' @return
#' @export
#'
#' @examples
merge_overlaps <- function(df, dimension_start, dimension_end, report_end, merge_level){

  if(merge_level == "service_name") {
    grouping_variables <-  c("funder_service_code", "service_name")
  }

  if(merge_level == "funder_service_code") {
    grouping_variables <-  c("funder_service_code")
  }

  if(merge_level == "organization") {
    grouping_variables <-  c("sr_code")
  }

  df |>
    dplyr::mutate(end_date = dplyr::if_else(is.na({{dimension_end}}), {{report_end}} + lubridate::days(1), {{dimension_end}})) |>
    dplyr::arrange(participant_id, {{dimension_start}}) |>
    dplyr::mutate(sr_code = get_sr_code(funder_service_code)) |>
    dplyr::group_by(participant_id, dplyr::pick(dplyr::all_of(grouping_variables))) |>
    dplyr::mutate(index = c(0, cumsum(as.numeric(dplyr::lead({{dimension_start}})) >
                                 cummax(as.numeric(end_date)))[-dplyr::n()])) |>
    dplyr::group_by(participant_id, participant_funder_age_group_code, index, dplyr::pick(dplyr::all_of(grouping_variables))) |>
    dplyr::summarize(merged_start_date = dplyr::first({{dimension_start}}),
              merged_end_date = dplyr::last(end_date),
              all_starts = paste({{dimension_start}}, collapse = ", "),
              all_ends = paste({{dimension_end}}, collapse = ", "),
              all_funder_service_codes = paste(funder_service_code, collapse = ", "),
              all_service_names = paste(service_name, collapse = ", "),
              all_service_status_reasons = paste(service_status_reason, collapse = ", "),
              service_first_direct_activity = min(service_first_direct_activity)) |>
    dplyr::ungroup()

}


#' Intersect overlapping program enrollments (useful for finding last discharge date)
#'
#' @param df
#' @param dimension_start
#' @param dimension_end
#' @param report_end
#' @param merge_level
#'
#' @return
#' @export
#'
#' @examples
intersect_overlaps <- function(df, dimension_start, dimension_end, report_end, merge_level){

  if(merge_level == "service_name") {
    grouping_variables <-  c("funder_service_code", "service_name")
  }

  if(merge_level == "funder_service_code") {
    grouping_variables <-  c("funder_service_code")
  }

  if(merge_level == "organization") {
    grouping_variables <-  c("sr_code")
  }

  df |>
    dplyr::mutate(end_date = dplyr::if_else(is.na({{dimension_end}}), {{report_end}} + lubridate::days(1), {{dimension_end}})) |>
    dplyr::arrange(participant_id, {{dimension_start}}) |>
    dplyr::mutate(sr_code = get_sr_code(funder_service_code)) |>
    dplyr::group_by(participant_id, dplyr::pick(dplyr::all_of(grouping_variables))) |>
    dplyr::mutate(index = c(0, cumsum(as.numeric(dplyr::lead({{dimension_start}})) >
                                 cummax(as.numeric(end_date)))[-dplyr::n()])) |>
    dplyr::group_by(participant_id, participant_funder_age_group_code, index, dplyr::pick(dplyr::all_of(grouping_variables))) |>
    dplyr::summarize(merged_start_date = dplyr::last({{dimension_start}}),
              merged_end_date = dplyr::first(end_date),
              all_starts = paste({{dimension_start}}, collapse = ", "),
              all_ends = paste({{dimension_end}}, collapse = ", "),
              all_funder_service_codes = paste(funder_service_code, collapse = ", "),
              all_service_names = paste(service_name, collapse = ", "),
              all_service_status_reasons = paste(service_status_reason, collapse = ", ")) |>
    dplyr::ungroup()

}
