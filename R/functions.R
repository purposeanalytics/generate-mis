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
      dplyr::filter(activity_duration > 5 | activity_duration == 0 | is.na(activity_duration) | funder_service_code %in% css_transportation_fcs)
  }

  visits
}


# CSS case management FCs. Assessments here are client activity (visit, individual served);
# in other CSS service delivery FCs they are not (OHRS Ch. 10, 10.9.4.4, S512).
css_case_management_fcs <- c("72 5 82 09", "73 5 82 09")

# FCs with their own activity rules. Each is written 7* in the OHRS, so both the 72 and 73
# fund types are listed.
# CSS Meals Delivery: every activity type counts (meals are counted, not visits; S248)
css_meals_delivery_fcs <- c("72 5 82 10", "73 5 82 10")
# CSS Transportation-Client: every trip counts, however short (no 5-minute rule)
css_transportation_fcs <- c("72 5 82 14", "73 5 82 14")
# Information and Referral Services - General: SR 65 under ir_version = "verCMHA"
info_referral_fcs <- c("72 5 70 10", "73 5 70 10")


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


# Default service recipient (SR) category for an FC: digits 4 and 5 of most of its
# statistical accounts. Some statistics use a different category (e.g. S452 60/65, S407 99,
# S950 10, S483 25 in residential FCs), set where the account is built.
# - 45: CMH&A residential (7* 5 40 ** with 76 mental health or 78 addictions)
# - 25: other CMH&A (76 or 78), and Centralized/Coordinated Access (7* 5 08, OHRS Ch. 7)
# - 80: everything else (community support services)
default_sr_category <- function(funder_service_code) {
  mh_addictions <- stringr::str_sub(funder_service_code, 9, 10) %in% c("76", "78")
  residential <- stringr::str_sub(funder_service_code, 6, 7) == "40"
  dplyr::case_when(
    mh_addictions & residential ~ "45",
    mh_addictions ~ "25",
    stringr::str_detect(funder_service_code, "^7[23] 5 08$") ~ "25",
    .default = "80")
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


# Add funder_statistical_account_code ("ddd dd dd") from its three parts, each a column
# expression evaluated in `df` (as in dplyr::mutate()):
# - statistic: the 3-digit statistic, e.g. "506"
# - sr_category: the 2-digit service recipient category, usually default_sr_category()
# - detail: the last 2 digits, e.g. age group, reason or time interval
# A part with the wrong number of digits is a bug in the statistic, so the run stops. A blank
# part (e.g. an unmapped meal type) comes from the data: the row is kept and a data warning
# names the account, which can't be reported as it stands.
add_statistical_account <- function(df, statistic, sr_category, detail) {
  df <- dplyr::mutate(df,
                      .statistic = as.character({{ statistic }}),
                      .sr_category = as.character({{ sr_category }}),
                      .detail = as.character({{ detail }}))

  widths <- c(statistic = 3, sr_category = 2, detail = 2)
  blank <- rep(FALSE, nrow(df))
  for (part_name in names(widths)) {
    part <- df[[paste0(".", part_name)]]
    # a blank pasted into a part (paste0(NA, "1")) reads as "NA..."
    part_blank <- is.na(part) | grepl("NA", part, fixed = TRUE)
    wrong <- !part_blank & !grepl(paste0("^[0-9]{", widths[[part_name]], "}$"), part)
    if (any(wrong)) {
      stop(glue::glue("{part_name} must be {widths[[part_name]]} digits; got {paste(unique(part[wrong]), collapse = ', ')}"),
           call. = FALSE)
    }
    blank <- blank | part_blank
  }

  df <- df |>
    dplyr::mutate(funder_statistical_account_code = paste(.statistic, .sr_category, .detail)) |>
    dplyr::select(-.statistic, -.sr_category, -.detail)

  if (any(blank)) {
    accounts <- unique(df$funder_statistical_account_code[blank])
    data_warning(glue::glue("{sum(blank)} row(s) have a statistical account with a blank part ",
                            "({paste(utils::head(accounts, 10), collapse = ', ')}), so they can't be reported ",
                            "as they stand. Check the input columns the statistic uses."))
  }

  df
}

# First digit of an age group code (20, 40, 60, 90), for accounts that pair it with another
# digit (e.g. S450 ** 41 = adult, virtual)
age_digit <- function(age_group_code) {
  stringr::str_sub(age_group_code, 1, 1)
}

# Warn about FC/account pairs in an output table that aren't in the OHRS code table
# (all_functional_centres_and_statistical_codes), naming up to 15. Each is either a rule
# that builds an account the FC doesn't report, or a gap in the code table. The code table
# has no fund type 3 copy of the organization FC, so 83 9 90 is checked against 82 9 90.
check_accounts <- function(output, table_name) {
  valid <- all_functional_centres_and_statistical_codes |>
    dplyr::distinct(funder_service_code, funder_statistical_account_code)

  unknown <- output |>
    dplyr::distinct(funder_service_code, funder_statistical_account_code) |>
    dplyr::mutate(check_code = dplyr::if_else(funder_service_code == "83 9 90", "82 9 90", funder_service_code)) |>
    dplyr::anti_join(valid, by = c("check_code" = "funder_service_code", "funder_statistical_account_code")) |>
    dplyr::arrange(funder_statistical_account_code, funder_service_code)

  if (nrow(unknown) > 0) {
    pairs <- paste0(unknown$funder_service_code, ": ", unknown$funder_statistical_account_code)
    shown <- paste(utils::head(pairs, 15), collapse = "; ")
    if (length(pairs) > 15) shown <- paste0(shown, "; ...")
    data_warning(glue::glue("{nrow(unknown)} FC/account pair(s) in {table_name} aren't in the OHRS code table: ",
                            "{shown}. Check the rule that builds them, or update the code table."))
  }

  invisible(output)
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
# `breaks` (optional, logical column): a row flagged TRUE starts a new merged period when it
# starts on or after the latest end so far, instead of only when it starts after it.
merge_overlaps <- function(df, dimension_start, dimension_end, report_end, merge_level, breaks = FALSE){

  grouping_variables <- overlap_grouping(merge_level)

  df |>
    dplyr::mutate(end_date = dplyr::if_else(is.na({{dimension_end}}), {{report_end}} + lubridate::days(1), {{dimension_end}})) |>
    dplyr::arrange(participant_id, {{dimension_start}}) |>
    dplyr::mutate(sr_code = default_sr_category(funder_service_code)) |>
    dplyr::group_by(participant_id, dplyr::pick(dplyr::all_of(grouping_variables))) |>
    dplyr::mutate(.break = {{breaks}},
                  .next_start = as.numeric(dplyr::lead({{dimension_start}})),
                  .latest_end = cummax(as.numeric(end_date)),
                  index = c(0, cumsum(.next_start > .latest_end |
                                        (dplyr::lead(.break) & .next_start >= .latest_end))[-dplyr::n()])) |>
    dplyr::group_by(participant_id, participant_funder_age_group_code, index, dplyr::pick(dplyr::all_of(grouping_variables))) |>
    dplyr::summarize(merged_start_date = dplyr::first({{dimension_start}}),
              # latest end in the merged period (the last-starting row may end earlier)
              merged_end_date = max_or_blank(end_date),
              all_starts = paste({{dimension_start}}, collapse = ", "),
              all_ends = paste({{dimension_end}}, collapse = ", "),
              all_funder_service_codes = paste(funder_service_code, collapse = ", "),
              all_service_names = paste(service_name, collapse = ", "),
              all_service_status_reasons = paste(service_status_reason, collapse = ", "),
              # reason on the row that ended last (e.g. the final discharge in the period)
              last_service_status_reason = service_status_reason[which.max(end_date)],
              service_first_direct_activity = min_or_blank(service_first_direct_activity),
              service_enrollment_id = min_or_blank(service_enrollment_id),
              # whether service started at any point in the merged period, and when it first did
              ever_active = any(service_status == "Active"),
              # earliest eligibility decision in the merged period (NA if none)
              episode_decision_date = first_date(episode_decision_date),
              first_active_date = first_date(service_status_start_date[service_status == "Active"])) |>
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

  grouping_variables <- overlap_grouping(merge_level)

  df |>
    dplyr::mutate(end_date = dplyr::if_else(is.na({{dimension_end}}), {{report_end}} + lubridate::days(1), {{dimension_end}})) |>
    dplyr::arrange(participant_id, {{dimension_start}}) |>
    dplyr::mutate(sr_code = default_sr_category(funder_service_code)) |>
    dplyr::group_by(participant_id, dplyr::pick(dplyr::all_of(grouping_variables))) |>
    dplyr::mutate(index = c(0, cumsum(as.numeric(dplyr::lead({{dimension_start}})) >
                                        cummax(as.numeric(end_date)))[-dplyr::n()])) |>
    dplyr::group_by(participant_id, participant_funder_age_group_code, index, dplyr::pick(dplyr::all_of(grouping_variables))) |>
    dplyr::summarize(
      merged_start_date = dplyr::first({{dimension_start}}),
      merged_end_date = dplyr::last(end_date),
      merged_report_date = max_or_blank(.report_date),
      all_starts = paste({{dimension_start}}, collapse = ", "),
      all_ends = paste({{dimension_end}}, collapse = ", "),
      all_funder_service_codes = paste(funder_service_code, collapse = ", "),
      all_service_names = paste(service_name, collapse = ", "),
      all_service_status_reasons = paste(service_status_reason, collapse = ", ")
    ) |>
    dplyr::ungroup()
}



# Grain ----
# Most statistics are reported at two grains: by functional centre ("fc"), and by service
# within a functional centre ("service"). grain_cols() gives the columns that identify the
# reporting unit, so each statistic is written once and run at both grains.
grain_cols <- function(grain) {
  switch(match.arg(grain, c("fc", "service")),
         fc = "funder_service_code",
         service = c("funder_service_code", "service_name"))
}

# Columns the overlap helpers group by. Accepts a grain ("fc", "service") or the older
# names ("funder_service_code", "service_name"), plus "organization" (by SR code).
overlap_grouping <- function(merge_level) {
  switch(merge_level,
         fc = , funder_service_code = grain_cols("fc"),
         service = , service_name = grain_cols("service"),
         organization = "sr_code",
         stop("unknown merge_level: ", merge_level, call. = FALSE))
}

# Count rows per date, reporting unit and statistical account
count_by_grain <- function(df, grain) {
  df |>
    dplyr::count(date, dplyr::across(dplyr::all_of(grain_cols(grain))), funder_statistical_account_code, name = "value")
}

# Sum an expression per date, reporting unit and statistical account
sum_by_grain <- function(df, grain, value) {
  df |>
    dplyr::group_by(date, dplyr::across(dplyr::all_of(grain_cols(grain))), funder_statistical_account_code) |>
    dplyr::summarize(value = {{ value }}, .groups = "drop")
}

# Fiscal year (April to March) as the 4-digit year it ends in (2025-04-01 is in "2026")
fiscal_year <- function(date) {
  lubridate::quarter(date, type = "year.quarter", fiscal_start = 4) |> stringr::str_sub(1, 4)
}

# Use each person's lowest age group code in the fiscal year (the age group at their latest
# contact, as codes run 20 elderly < 40 adult < 60 paediatric), within the `by` columns.
min_age_group_by_fiscal_year <- function(df, by = character(0)) {
  df |>
    dplyr::mutate(fiscal_year = fiscal_year(date)) |>
    dplyr::group_by(dplyr::pick(dplyr::all_of(c("participant_id", by, "fiscal_year")))) |>
    dplyr::mutate(participant_funder_age_group_code = min_or_blank(participant_funder_age_group_code)) |>
    dplyr::ungroup()
}

# Keep one row per person per day in each reporting unit, preferring face-to-face in person,
# then virtual, then non-face-to-face (activity_type_priority 1, 2, 3)
one_per_person_per_day <- function(df, grain) {
  keys <- c(grain_cols(grain), "participant_id", "date")
  df |>
    dplyr::group_by(dplyr::pick(dplyr::all_of(keys))) |>
    dplyr::arrange(dplyr::pick(dplyr::all_of(c(keys, "activity_type_priority")))) |>
    dplyr::slice(1) |>
    dplyr::ungroup()
}

# min() and max(), but a blank of the same type for empty input instead of a warning. dplyr
# evaluates summaries once on empty input when there are no groups (e.g. no clients in any
# FC that reports a statistic).
min_or_blank <- function(x) if (length(x) == 0) x[NA_integer_] else min(x)
max_or_blank <- function(x) if (length(x) == 0) x[NA_integer_] else max(x)

# Earliest date, or NA if there are none (min() would warn and return Inf)
first_date <- function(dates) {
  if (length(dates) == 0 || all(is.na(dates))) lubridate::NA_Date_ else min(dates, na.rm = TRUE)
}


# Episodes ----
# An episode is one stretch of time a person is on an FC's books for an enrollment, from
# acceptance to discharge (OHRS Ch. 10, 10.9.4.4 examples 1 and 2). number_episodes() marks
# them on the service history (sorted, with end dates set); fc_episodes() turns them into
# one row per episode for the statistics that count referrals, first services, waits and
# discharges.
#
# Rules (decided with the user, 2026-11-04):
# - An Inactive status ends an episode. Any later status in the same enrollment starts a new
#   one (the file was reopened), with its own referral, first service, wait and discharge.
# - Once service has started in an episode, a return to On Waitlist also ends it: the client
#   is discharged from the service (S513) and the waitlist starts a new episode with a new
#   referral (S489), wait and first service (OHRS Ch. 10, 10.9.4.4 example 2). The client
#   stays the organization's responsibility, so there is no S511/S501.
# - Holds don't end an episode, so service that pauses and resumes has one first service.
# - An Active status that starts and ends on the same day, when a waitlist status starts
#   that day and carries on past it, was a correction (the client went straight back to
#   waiting) and is dropped before episodes are numbered: see drop_same_day_corrections().
#   This uses the status end dates as recorded in services.rds.
# - episode_decision_date is the eligibility decision date for the episode. A decision date
#   from before a reopened episode began belongs to an earlier episode. S489 needs one: an
#   episode with no decision date has no referral counted, and process_data() warns about it:
#   data prep should supply a decision date whenever a client is moved to Waitlist, Hold or
#   Active, so a missing one is an error in the source data.
# - accepted_date is when the FC took responsibility: the decision date, or if there is none,
#   the first Waitlist, Hold or Active date. It is used to merge episodes and for S501/S511.
# - Overlapping episodes in the same FC (or service) are merged into one, so a person has one
#   referral and one first service while any of their enrollments there is open. Episodes of
#   the same enrollment are never merged with each other, even when one ends the day the next
#   begins.
episode_numbers <- function(enrollment, status) {
  episode <- integer(length(status))
  current <- 0L
  active_seen <- FALSE
  for (i in seq_along(status)) {
    if (i > 1 && enrollment[i] != enrollment[i - 1]) {
      current <- 0L
      active_seen <- FALSE
    } else if (i > 1 && (status[i - 1] == "Inactive" || (status[i] == "On Waitlist" && active_seen))) {
      current <- current + 1L
      active_seen <- FALSE
    }
    if (status[i] == "Active") active_seen <- TRUE
    episode[i] <- current
  }
  episode
}

number_episodes <- function(service_history) {
  # expects rows sorted by enrollment, then status start date (as process_data() leaves them)
  service_history |>
    dplyr::mutate(episode = episode_numbers(service_enrollment_id, as.character(service_status))) |>
    dplyr::group_by(service_enrollment_id, episode) |>
    dplyr::mutate(
      episode_start_date = min_or_blank(service_status_start_date),
      episode_decision_date = first_date(service_decision_date[episode == 0 | service_decision_date >= episode_start_date]),
      accepted_date = dplyr::coalesce(
        episode_decision_date,
        first_date(service_status_start_date[service_status %in% c("On Waitlist", "On Hold", "Active")]),
        episode_start_date),
      # discharge reason, from the Inactive status that ended the episode (none if the client
      # went back to the waitlist)
      episode_discharge_reason = dplyr::last(service_status_reason[service_status == "Inactive"])) |>
    dplyr::ungroup() |>
    dplyr::select(-episode_start_date)
}

# Drop Active statuses that were corrected the same day: the Active starts and ends on the
# same day, and an On Waitlist status in the same enrollment starts that day and doesn't end
# on it. Uses service_status_end_date as recorded in services.rds (blank = still current),
# so it must run before end dates are recalculated.
drop_same_day_corrections <- function(services) {
  continuing_waitlists <- services |>
    dplyr::filter(service_status == "On Waitlist",
                  is.na(service_status_end_date) | service_status_end_date > service_status_start_date) |>
    dplyr::distinct(service_enrollment_id, service_status_start_date)

  corrected <- services |>
    dplyr::filter(service_status == "Active", service_status_end_date == service_status_start_date) |>
    dplyr::semi_join(continuing_waitlists, by = c("service_enrollment_id", "service_status_start_date")) |>
    dplyr::distinct(service_enrollment_id, service_status, service_status_start_date)

  dplyr::anti_join(services, corrected, by = c("service_enrollment_id", "service_status", "service_status_start_date"))
}

# A warning about the input data, with its own class (generateMIS_data_warning) so callers
# can pick these out from other warnings.
data_warning <- function(message) {
  warning(structure(class = c("generateMIS_data_warning", "warning", "condition"),
                    list(message = message, call = NULL)))
}

service_status_levels <- c("Waiting for Assessment", "On Waitlist", "On Hold",
                           "Not Admitted", "Active", "Inactive")

# Drop service history rows that can't be placed in an episode: a status that isn't one of
# service_status_levels, or no enrollment id. Warns with the labels found so the source
# data (or the status mapping in data prep) can be corrected.
drop_unusable_statuses <- function(services) {
  unknown <- !is.na(services$service_status) & !services$service_status %in% service_status_levels
  blank_status <- is.na(services$service_status)
  no_enrollment <- is.na(services$service_enrollment_id)

  if (any(unknown)) {
    counts <- table(services$service_status[unknown])
    labels <- paste0('"', names(counts), '" (', as.integer(counts), ")", collapse = ", ")
    data_warning(glue::glue("{sum(unknown)} service status row(s) have a status generateMIS doesn't ",
                            "recognise and were dropped: {labels}. Expected one of: ",
                            "{paste(service_status_levels, collapse = ', ')}."))
  }
  if (any(blank_status)) {
    data_warning(glue::glue("{sum(blank_status)} service status row(s) have no service_status and were dropped."))
  }
  if (any(no_enrollment)) {
    data_warning(glue::glue("{sum(no_enrollment)} service status row(s) have no service_enrollment_id and were dropped."))
  }

  services[!(unknown | blank_status | no_enrollment), ]
}

# Warn about episodes that reached Waitlist, Hold or Active with no eligibility decision date.
# These have no referral (S489) but can still have a first service (S506/S401), so the
# counts won't pair up until the source data is corrected.
warn_missing_decision_dates <- function(service_history) {
  missing <- service_history |>
    dplyr::filter(service_status %in% c("On Waitlist", "On Hold", "Active"), is.na(episode_decision_date)) |>
    dplyr::distinct(service_enrollment_id, episode)

  if (nrow(missing) > 0) {
    enrollments <- unique(missing$service_enrollment_id)
    shown <- paste(utils::head(enrollments, 10), collapse = ", ")
    if (length(enrollments) > 10) shown <- paste0(shown, ", ...")
    data_warning(glue::glue("{nrow(missing)} episode(s) were moved to Waitlist, Hold or Active with no ",
                       "service_decision_date, so they have no referral (S489) but can still have a ",
                       "first service (S506/S401). Correct the source data. Enrollment(s): {shown}"))
  }

  invisible(service_history)
}

# One row per FC (or service) episode: overlapping episodes in the same FC/service are merged,
# from the earliest acceptance to the latest end. Open episodes run to the day after
# report_date, so merged_end_date <= report_date means every episode in it has ended.
# Columns used by the statistics: merged_start_date (accepted), episode_decision_date (NA if
# none), first_active_date (first service, NA if none), merged_end_date,
# last_service_status_reason (from the episode that ended last) and
# participant_funder_age_group_code.
fc_episodes <- function(service_history, grain, report_date) {
  service_history |>
    dplyr::filter(service_status %in% c("On Waitlist", "On Hold", "Active")) |>
    dplyr::group_by(service_enrollment_id, episode) |>
    # first row of a later episode: not merged with the episode before it
    dplyr::mutate(new_episode = episode > 0 & dplyr::row_number() == 1) |>
    dplyr::ungroup() |>
    dplyr::mutate(service_status_reason = episode_discharge_reason,
                  # at the organization level the client is still on the books
                  new_episode = new_episode & grain != "organization") |>
    merge_overlaps(accepted_date, service_status_end_date, report_date, merge_level = grain,
                   breaks = new_episode)
}
