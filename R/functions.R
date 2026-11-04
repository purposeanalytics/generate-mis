#' Apply fc_450_version to visits
#'
#' Used for every statistic built from visits, so visit counts and "served" counts
#' (S455, S855, S950, S955) use the same visits.
#'
#' @param visits mis_visits
#' @param fc_450_version "verA" counts only activities over 5 minutes (plus entries with no
#'   reported time and all CSS Transportation); "verB" counts all activities.
#'
#' @return visits, filtered
apply_fc_450_version <- function(visits, fc_450_version) {
  fc_450_version <- match.arg(fc_450_version, c("verA", "verB"))

  if (fc_450_version == "verA") {
    visits <- visits |>
      dplyr::filter(activity_duration > 5 | activity_duration == 0 | is.na(activity_duration) | funder_service_code == "72 5 82 14")
  }

  visits
}


# CSS case management FCs. Assessments here are client activity (visit, individual served);
# in other CSS service delivery FCs they are not (OHRS Ch. 10, 10.9.4.4, S512).
css_case_management_fcs <- c("72 5 82 09", "73 5 82 09")


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


#' A faster method for calculating age from birth date
#'
#' @param df
#' @param date_of_birth
#' @param reference_date
#' @param feb29_as
#'
#' @return
#' @export
#'
#' @examples
fast_age_calculation <- function(df, date_of_birth, reference_date, feb29_as = "feb28", age_breakpoints = NULL) {

  feb29_as <- match.arg(feb29_as, c("feb28", "mar01"))

  # Default matches original behaviour exactly
  default_breaks <- c(0, 5, 12, 17, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, 94, 99, 999)
  default_labels <- as.character(1:20)

  bp_breaks <- if (is.null(age_breakpoints)) default_breaks else age_breakpoints$breaks
  bp_labels <- if (is.null(age_breakpoints)) default_labels else age_breakpoints$labels

  df <- df %>% dplyr::mutate(
    reference_date = as.Date({{ reference_date }}),
    date_of_birth  = as.Date({{ date_of_birth }})
  )

  participants <- df %>%
    dplyr::distinct(participant_id, date_of_birth) %>%
    dplyr::arrange(participant_id)

  dob_lt   <- as.POSIXlt(participants$date_of_birth)
  dob_year <- dob_lt$year + 1900L
  dob_mon  <- dob_lt$mon  + 1L
  dob_day  <- dob_lt$mday
  dob_mmdd <- dob_mon * 100L + dob_day
  is_feb29 <- (dob_mon == 2L & dob_day == 29L)

  if (feb29_as == "feb28") {
    dob_mmdd[is_feb29] <- 228L
  } else {
    dob_mmdd[is_feb29] <- 301L
  }

  idx   <- match(df$participant_id, participants$participant_id)
  dt_lt <- as.POSIXlt(df$reference_date)
  year  <- dt_lt$year + 1900L
  mmdd  <- (dt_lt$mon + 1L) * 100L + dt_lt$mday

  participant_age <- (year - dob_year[idx]) - as.integer(mmdd < dob_mmdd[idx])

  dplyr::select(
    dplyr::mutate(df,
      participant_age = participant_age,

      participant_funder_age_group_code = cut(
        participant_age,
        breaks = c(0, 17, 65, 999),
        labels = c("60", "40", "20")
      ),
      participant_funder_age_group_code = dplyr::coalesce(
        participant_funder_age_group_code, "90"
      ),

      participant_age_group_sort = cut(
        participant_age,
        breaks = bp_breaks,
        labels = bp_labels
      ),
      participant_age_group_sort = as.numeric(participant_age_group_sort),
      participant_age_group_sort = dplyr::coalesce(
        participant_age_group_sort, 99
      ),

      participant_age = floor(participant_age),
      .after = participant_date_of_birth
    ),
    -date_of_birth,
    -reference_date
  )
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
    # Centralized/Coordinated Access is a CMH&A FC (OHRS Ch 7) with no 76/78 in its code
    stringr::str_detect({{fc_code}}, "^7[23] 5 08$") ~ "25",
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
              # latest end in the merged period (the last-starting row may end earlier)
              merged_end_date = max(end_date),
              all_starts = paste({{dimension_start}}, collapse = ", "),
              all_ends = paste({{dimension_end}}, collapse = ", "),
              all_funder_service_codes = paste(funder_service_code, collapse = ", "),
              all_service_names = paste(service_name, collapse = ", "),
              all_service_status_reasons = paste(service_status_reason, collapse = ", "),
              # reason on the row that ended last (e.g. the final discharge in the period)
              last_service_status_reason = service_status_reason[which.max(end_date)],
              service_first_direct_activity = min(service_first_direct_activity),
              service_enrollment_id = min(service_enrollment_id),
              # whether service started at any point in the merged period
              ever_active = any(service_status == "Active")) |>
    dplyr::ungroup()

}

#' Intersect overlapping program enrollments (useful for finding last discharge date on inactive status)
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

#' Earliest overlapping program enrollments (takes first start and first end in overlap; useful for finding wait time)
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
earliest_overlaps <- function(df, dimension_start, dimension_end, report_end, merge_level, report_on) {

  # optional date to report the merged period on (e.g. service start date for S407);
  # returned as merged_report_date (latest in the period), NA if not supplied
  if (missing(report_on)) {
    df <- dplyr::mutate(df, .report_date = lubridate::NA_Date_)
  } else {
    df <- dplyr::mutate(df, .report_date = {{report_on}})
  }

  if (merge_level == "service_name") {
    grouping_variables <- c("funder_service_code", "service_name")
  }

  if (merge_level == "funder_service_code") {
    grouping_variables <- c("funder_service_code")
  }

  if (merge_level == "organization") {
    grouping_variables <- c("sr_code")
  }

  df |>
    dplyr::mutate(end_date = dplyr::if_else(is.na({{dimension_end}}), {{report_end}} + lubridate::days(1), {{dimension_end}})) |>
    dplyr::arrange(participant_id, {{dimension_start}}) |>
    dplyr::mutate(sr_code = get_sr_code(funder_service_code)) |>
    dplyr::group_by(participant_id, dplyr::pick(dplyr::all_of(grouping_variables))) |>
    dplyr::mutate(index = c(0, cumsum(as.numeric(dplyr::lead({{dimension_start}})) >
                                        cummax(as.numeric(end_date)))[-dplyr::n()])) |>
    dplyr::group_by(participant_id, participant_funder_age_group_code, index, dplyr::pick(dplyr::all_of(grouping_variables))) |>
    dplyr::summarize(
      merged_start_date = dplyr::first({{dimension_start}}),
      merged_end_date = dplyr::last(end_date),
      merged_report_date = max(.report_date),
      all_starts = paste({{dimension_start}}, collapse = ", "),
      all_ends = paste({{dimension_end}}, collapse = ", "),
      all_funder_service_codes = paste(funder_service_code, collapse = ", "),
      all_service_names = paste(service_name, collapse = ", "),
      all_service_status_reasons = paste(service_status_reason, collapse = ", ")
    ) |>
    dplyr::ungroup()
}

