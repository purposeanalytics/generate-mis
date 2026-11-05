#' Generate census count tables
#'
#' @param processed_data
#'
#' @return
#' @export
#'
#' @examples
generate_census_counts <- function(processed_data,
                                   fc_455_version = "verA",
                                   fc_450_version = "verA"){

  fc_455_version <- match.arg(fc_455_version, c("verA", "verB", "verC"))

  rlog::log_info(glue::glue("Applying fc_455_version = '{fc_455_version}' and fc_450_version = '{fc_450_version}' to census counts"))

  data_folder <- processed_data$data_folder
  mis_service_history <- processed_data$mis_service_history
  # Visits used as evidence that someone was served (S455, S855, S950, S955).
  # An assessment is client activity only in the CSS case management FC (OHRS Ch. 10,
  # 10.9.4.4, S512). CMH&A has no assessment activity type (assessments are recorded as
  # visits), so any other "Assessment" activity is not evidence of service.
  # Only visits that count under fc_450_version are evidence of service, as in the visit counts.
  mis_visits <- processed_data$mis_visits |>
    apply_fc_450_version(fc_450_version) |>
    dplyr::filter(!(activity_type %in% "Assessment" & !funder_service_code %in% css_case_management_fcs))

  # Fund type = 2nd digit of the FC (e.g. 72 -> 2, 73 -> 3); CMH&A S855 is reported by fund type
  # in AC 8* 9 90 (OHRS Ch. 7, 7.1.4 and 7.7.6)
  mis_visits <- mis_visits |>
    dplyr::mutate(fund_type = stringr::str_sub(funder_service_code, 2, 2))
  mis_daily_census <- processed_data$mis_daily_census |>
    dplyr::mutate(fund_type = stringr::str_sub(funder_service_code, 2, 2))

  # Each FC-level statistic below is a function of `grain` ("fc" or "service"; see
  # grain_cols()), run once per grain when the output tables are put together at the end.
  # Organization-level statistics (reported in AC 8* 9 90) have no grain.


  # S403 4* *0 Resident Days ----
  # The number of calendar days that a community mental health and addictions residential care client (service recipient code 45 and 44) is served in a 7*5 40* residential service functional centre.
  # The day of admission is counted, and the day of discharge is not counted.
  # When the client is admitted and discharged on the same day, one resident day is counted.
  # Age category breakdown and service recipient category are required.  The service recipient (SR) category 45 and 44 (as noted in the 4th and 5th digits of the account number above).
  # Note:  S403 4* *0 Resident Days, must be reported for all residential services functional centres 7* 5 40 ** **, except the Supportive Housing functional centres (refer to section 7.6.8 for the list of Supportive Housing functional centres).  There is a mandatory edit rule for trial balance submission process to ensure this statistic is reported.

  calc_403 <- function(grain) {
    rlog::log_info(glue::glue("Generating S403 statistic by {grain}"))
    mis_daily_census |>
      filter_eligible("403") |>
      dplyr::filter(service_status == "Active") |>
      dplyr::distinct(dplyr::pick(dplyr::all_of(c("participant_id", "participant_funder_age_group_code", grain_cols(grain), "date")))) |>
      add_statistical_account(statistic = "403",
                              sr_category = default_sr_category(funder_service_code),
                              detail = participant_funder_age_group_code) |>
      count_by_grain(grain)
  }


  #S406 ** 20 Individuals Currently Waiting for Service Initiation ----
  #The number of individuals waiting for service from a specific functional centre.  These individuals are eligible for service but have not yet had their first service and thus are still waiting for service.  This statistic is recorded in the client/service functional centre.  This reflects the number on the waiting list on the last day of reporting period.  Service Recipient Category required.  This is a snapshot at a specific point in time as at September 30 (Q2), December 31 (Q3), and March 31 (Year End).  It is not a cumulative number at Year End.
  #The S406**20 does not include individuals who are waiting, when the service is available to the service recipient but the individual requests to delay initiation of service.

  calc_406 <- function(grain) {
    rlog::log_info(glue::glue("Generating S406 statistic by {grain}"))
    mis_daily_census |>
      filter_eligible("406") |>
      # participants who were on the waitlist at the end of the reporting period
      dplyr::filter(service_status == "On Waitlist") |>
      # exclude first day of wait where duration is zero -- only count on waitlist if they wait one day or more
      dplyr::filter(service_status_start_date != date) |>
      dplyr::distinct(dplyr::pick(dplyr::all_of(c("date", "participant_id", grain_cols(grain), "service_status")))) |>
      add_statistical_account(statistic = "406",
                              sr_category = default_sr_category(funder_service_code),
                              detail = "20") |>
      count_by_grain(grain)
  }


  # S406 99 10 Individuals Currently Waiting for Assessment ----
  # The number of individuals referred to the organization and currently waiting for screening to determine service eligibility.
  # Request for the service has been received but no service decision has been made.
  # This reflects the number on the waiting list on the last day of reporting period.
  # This is a snapshot at a specific point in time as at September 30 (Q2), December 31 (Q3), and March 31 (Year End).
  # It is not a cumulative number at Year End.

  calc_406_99 <- function(grain) {
    rlog::log_info(glue::glue("Generating S406 99 statistic by {grain}"))
    mis_daily_census |>
      filter_eligible("406 99") |>
      # filter participants who were pending assessment at the end of the reporting period
      dplyr::filter(service_status == "Waiting for Assessment") |>
      # exclude first day of wait where duration is zero -- only count as waiting for assessment if they wait one day or more
      dplyr::filter(service_status_start_date != date) |>
      dplyr::distinct(dplyr::pick(dplyr::all_of(c("date", "participant_id", grain_cols(grain), "service_status")))) |>
      add_statistical_account(statistic = "406",
                              sr_category = "99",
                              detail = "10") |>
      count_by_grain(grain)
  }


  # Individuals served (S455, S855, S950, S955) ----
  # One row per person per day they count as served, by fc_455_version:
  #   verA: Active enrollment, and a visit that day (matched on the `match_on` columns)
  #   verB: a visit, whatever the enrollment status
  #   verC: Active enrollment, whether or not there was a visit
  # `keep` applies the statistic's own row filters (FC eligibility, language, ...) to whichever
  # source is used; `cols` are the columns to keep besides date and participant_id.
  # Under verB, language and special needs come from the visit records; otherwise from the
  # participant record (via the daily census).
  served_days <- function(cols, keep = identity, match_on = character(0)) {
    id_cols <- c("date", "participant_id", cols)

    visit_days <- function() {
      mis_visits |>
        keep() |>
        dplyr::rename(date = activity_date) |>
        dplyr::distinct(dplyr::pick(dplyr::all_of(id_cols)))
    }

    active_days <- function() {
      mis_daily_census |>
        dplyr::filter(service_status == "Active") |>
        keep() |>
        dplyr::distinct(dplyr::pick(dplyr::all_of(id_cols)))
    }

    switch(fc_455_version,
           verA = active_days() |>
             dplyr::semi_join(mis_visits, by = c("date" = "activity_date", "participant_id", match_on)),
           verB = visit_days(),
           verC = active_days())
  }


  # S455 ** *0 Individuals Served by Functional Centre ----
  # This statistical account is a year-to-date count of the number of individuals served by the functional centre in a reporting period and identified by a unique identifier.  Individuals are counted only once within the functional centre within a fiscal year, regardless of how many different services they have received or the number of times they were admitted and discharged within the reporting period.  This account is reported in the functional centre where the service was received.  An individual may receive services from several functional centres during the same reporting period.  This count cannot be summed for a “total” for the whole organization to report S855** Total Individuals Served by the Organization.
  # Note: Only individuals who have received service in a reporting period can be counted.
  # 	Service recipient category is required. Reporting with age category –Not Known, S455** 90 is not recommended
  # Note: All the S455* counts in the various functional centres cannot be added together to calculate a total for the whole organization.  See S855**76 for a total unique count of the Total Individuals Served by the Organization.
  # Note: the number of individuals reported using account S455****, Individuals Served by Functional Centre, within a functional centre cannot be greater than the number individuals served by the organization in account S855**76.

  calc_455 <- function(grain) {
    rlog::log_info(glue::glue("Generating S455 statistic by {grain}"))
    keys <- grain_cols(grain)
    served_days(c(keys, "participant_funder_age_group_code"),
                keep = function(df) filter_eligible(df, "455"),
                match_on = keys) |>
      min_age_group_by_fiscal_year(by = keys) |>
      add_statistical_account(statistic = "455",
                              sr_category = default_sr_category(funder_service_code),
                              detail = participant_funder_age_group_code) |>
      dplyr::select(date, dplyr::all_of(keys), funder_statistical_account_code, participant_id)
  }


  # S855 ** 5* Individuals Served by Organization ----
  # This statistical account is a year-to-date count of the number of individuals served by the organization, within a provincial sector code, and by fund type in a reporting period and identified by a unique identifier.  Individuals are counted only once within the organization within a fiscal year, regardless of how many different services they have received or the number of times they were admitted and discharged within the reporting period.  Age category reporting is required.  This account is reported in accounting centre 8*990.
  # - S 855 ** 52 Individuals Served by Org. – Elderly
  # - S 855 ** 54 Individuals Served by Org. – Adult
  # - S 855 ** 56 Individuals Served by Org. – Pediatric
  # - S 855 ** 59 Individuals Served by Org. – Age not Known
  # Where ** the 4th and 5th digit represent service recipient type
  # Where * the 7th digit represents age category. The reporting of Age not Known, is not recommended

  rlog::log_info("Generating S855 statistic")

  # CSS: one count for the organization (AC 82 9 90); under verA the visit can be in any FC
  calc_org_855_css <- served_days("participant_funder_age_group_code",
                                  keep = function(df) dplyr::filter(df, default_sr_category(funder_service_code) == "80")) |>
    min_age_group_by_fiscal_year() |>
    dplyr::mutate(funder_service_code = "82 9 90") |>
    add_statistical_account(statistic = "855",
                            sr_category = "80",
                            detail = paste0("5", age_digit(participant_funder_age_group_code))) |>
    dplyr::select(date, funder_service_code, funder_statistical_account_code, participant_id)

  # CMH&A: one count per fund type, in the organizational AC for that fund type (AC 82 9 90, 83 9 90)
  calc_org_855_cmha <- served_days(c("fund_type", "participant_funder_age_group_code"),
                                   keep = function(df) dplyr::filter(df, default_sr_category(funder_service_code) != "80"),
                                   match_on = "fund_type") |>
    min_age_group_by_fiscal_year(by = "fund_type") |>
    dplyr::mutate(funder_service_code = paste0("8", fund_type, " 9 90")) |>
    add_statistical_account(statistic = "855",
                            sr_category = "25",
                            detail = paste0("5", age_digit(participant_funder_age_group_code))) |>
    dplyr::select(date, funder_service_code, funder_statistical_account_code, participant_id)


  # S950 ** 10 Francophone Individuals Requested Service/Served in French (4th, 5th = SR code) ----
  # The following S950* accounts are available for reporting services provided to Francophone clients.  For CSS HSPs, it is mandatory to report S9508010 and S9501080 for individuals requested service in French by functional centre and organization level respectively.  S9508020 and S9502080 for individuals served in French are optional reporting.
  # •	S950 ** 10 Francophone Individuals Requested Service in French by Functional Centre
  # •	S950 ** 20 Francophone Individuals Served in French by Functional Centre
  # •	S950 10 ** Francophone Individuals Requested Service in French by the Organization
  # •	S950 20 ** Francophone Individuals Served in French by the Organization

  rlog::log_info("Generating S950 statistic")

  keep_950 <- function(df) {
    df |>
      filter_eligible("950") |>
      dplyr::filter(participant_preferred_language == "French")
  }

  # Organization: S950 10 80 in AC 82 9 90; under verA the visit can be in any FC
  calc_org_950_10 <- served_days(character(0), keep = keep_950) |>
    dplyr::mutate(funder_service_code = "82 9 90") |>
    add_statistical_account(statistic = "950",
                            sr_category = "10",
                            detail = "80") |>
    dplyr::select(date, funder_service_code, funder_statistical_account_code, participant_id)

  # Functional centre: S950 80 10
  calc_950 <- function(grain) {
    keys <- grain_cols(grain)
    served_days(keys, keep = keep_950, match_on = keys) |>
      add_statistical_account(statistic = "950",
                              sr_category = "80",
                              detail = "10") |>
      dplyr::select(date, dplyr::all_of(keys), funder_statistical_account_code, participant_id)
  }


  # S955 80 ** Individuals Served ----
  # The following S955* statistics provide a breakdown of the number of CSS clients served as reported in S455 80 ** specific FCs.
  # •	S955 80 10	Individuals Served – Physical Disability
  # •	S955 80 15	Individuals Served – Cognitive Impairment
  # •	S955 80 20	Individuals Served – Frail and/or Elderly
  # •	S955 80 22	Individuals Served – High Risk Seniors
  # •	S955 80 25	Individuals Served – Living with affects of HIV/AIDS
  # The sum of S955 80 10 to S955 80 25 should equal to the total reported in S455 80 ** under the same FC.
  # S955 80 10/15/20/25 accounts are mandatory reporting for:
  #   o	FC 72 5 82 45 (Assisted Living Service)
  #   o	FC 72 5 82 20 (Adult Day Services)
  #   o	FC 72 5 82 31 (Homemaking)
  #   o	FC 72 5 82 33 (Personal Support/Independence Training)
  #   o	FC 72 5 82 34 (Respite)
  #   o	FC 72 5 82 35 (Combined PS/HM/Respite Services)
  # S955 80 22 High Risk Senior is mandatory reporting for FC 725 82 45 Assisted Living Services.

  # S955 is a breakdown of S455 80 ** and is reported in the same FC (OHRS Ch. 10, 10.9.4.8), not AC 82 9 90
  calc_955 <- function(grain) {
    rlog::log_info(glue::glue("Generating S955 statistic by {grain}"))
    keys <- grain_cols(grain)
    served_days(c(keys, "participant_ohrs_special_needs"),
                keep = function(df) df |> filter_eligible("955") |> dplyr::filter(!is.na(participant_ohrs_special_needs)),
                match_on = keys) |>
      add_statistical_account(statistic = "955",
                              sr_category = default_sr_category(funder_service_code),
                              detail = dplyr::case_when(participant_ohrs_special_needs == "Physical Disability" ~ "10",
                                                        participant_ohrs_special_needs == "Cognitive Impairment" ~ "15",
                                                        participant_ohrs_special_needs == "Frail And-or Elderly" ~ "20",
                                                        participant_ohrs_special_needs == "High Risk Senior" ~ "22",
                                                        participant_ohrs_special_needs == "Living With Effects Of HIV-AIDS" ~ "25")) |>
      dplyr::select(date, dplyr::all_of(keys), funder_statistical_account_code, participant_id)
  }


  # Write mis_census_counts_by_fc to file ----

  rlog::log_info("Writing census-type statistics files")

  mis_census_counts_by_fc <- dplyr::bind_rows(calc_403("fc"), calc_406("fc"), calc_406_99("fc"), calc_455("fc"),
                                              calc_org_855_css, calc_org_855_cmha, calc_org_950_10,
                                              calc_950("fc"), calc_955("fc")) |>
    dplyr::mutate(funder_key = paste0(funder_service_code, "_", funder_statistical_account_code))

  check_accounts(mis_census_counts_by_fc, "mis_census_counts_by_fc")
  readr::write_csv(mis_census_counts_by_fc, paste0(data_folder, "/processed/mis_census_counts_by_fc.csv"), na = "")


  mis_census_counts_by_service <- dplyr::bind_rows(calc_403("service"), calc_406("service"), calc_406_99("service"),
                                                   calc_455("service"), calc_950("service"), calc_955("service")) |>
    dplyr::mutate(funder_service_key = paste0(funder_service_code, "_", service_name, "_", funder_statistical_account_code))

  readr::write_csv(mis_census_counts_by_service, paste0(data_folder, "/processed/mis_census_counts_by_service.csv"), na = "")

  invisible(list(mis_census_counts_by_fc = mis_census_counts_by_fc,
                 mis_census_counts_by_service = mis_census_counts_by_service))
}
