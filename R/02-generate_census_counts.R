#' Generate census count tables
#'
#' @param processed_data
#'
#' @return
#' @export
#'
#' @examples
generate_census_counts <- function(processed_data,
                                   fc_455_version = "verA"){

  data_folder <- processed_data$data_folder
  mis_service_history <- processed_data$mis_service_history
  mis_visits <- processed_data$mis_visits
  mis_daily_census <- processed_data$mis_daily_census

  # S403 4* *0 Resident Days ----
  # The number of calendar days that a community mental health and addictions residential care client (service recipient code 45 and 44) is served in a 7*5 40* residential service functional centre.
  # The day of admission is counted, and the day of discharge is not counted.
  # When the client is admitted and discharged on the same day, one resident day is counted.
  # Age category breakdown and service recipient category are required.  The service recipient (SR) category 45 and 44 (as noted in the 4th and 5th digits of the account number above).
  # Note:  S403 4* *0 Resident Days, must be reported for all residential services functional centres 7* 5 40 ** **, except the Supportive Housing functional centres (refer to section 7.6.8 for the list of Supportive Housing functional centres).  There is a mandatory edit rule for trial balance submission process to ensure this statistic is reported.

  rlog::log_info("Generating S403 statistic")

  calc_fc_403 <- mis_daily_census |>
    filter_eligible("403") |>
    dplyr::filter(service_status == "Active") |>
    dplyr::distinct(participant_id, participant_funder_age_group_code, funder_service_code, date) |>
    dplyr::mutate(digits_1_3 = "403",
                  digits_4_5 = get_sr_code(funder_service_code),
                  digits_6_7 = participant_funder_age_group_code) |>
    assemble_statistical_account() |>
    dplyr::count(date, funder_service_code, funder_statistical_account_code, name = "value")

  calc_service_403 <- mis_daily_census |>
    filter_eligible("403") |>
    dplyr::filter(service_status == "Active") |>
    dplyr::distinct(participant_id, participant_funder_age_group_code, funder_service_code, service_name, date) |>
    dplyr::mutate(digits_1_3 = "403",
                  digits_4_5 = get_sr_code(funder_service_code),
                  digits_6_7 = participant_funder_age_group_code) |>
    assemble_statistical_account() |>
    dplyr::count(date, funder_service_code, service_name, funder_statistical_account_code, name = "value")


  #S406 ** 20 Individuals Currently Waiting for Service Initiation ----
  #The number of individuals waiting for service from a specific functional centre.  These individuals are eligible for service but have not yet had their first service and thus are still waiting for service.  This statistic is recorded in the client/service functional centre.  This reflects the number on the waiting list on the last day of reporting period.  Service Recipient Category required.  This is a snapshot at a specific point in time as at September 30 (Q2), December 31 (Q3), and March 31 (Year End).  It is not a cumulative number at Year End.
  #The S406**20 does not include individuals who are waiting, when the service is available to the service recipient but the individual requests to delay initiation of service.

  rlog::log_info("Generating S406 statistic")

  calc_fc_406 <- mis_daily_census |>
    filter_eligible("406") |>
    dplyr::filter(service_status == "On Waitlist") |>
    # participants who were on the waitlist at the end of the reporting period
    # exclude those whose waitlist start and end dates are the same -- implied they were not actually on a waitlist
    dplyr::distinct(date, participant_id, funder_service_code, service_status) |>
    dplyr::mutate(digits_1_3 = "406",
                  digits_4_5 = get_sr_code(funder_service_code),
                  digits_6_7 = "20") |>
    assemble_statistical_account() |>
    dplyr::count(date, funder_service_code, funder_statistical_account_code, name = "value")

  calc_service_406 <- mis_daily_census |>
    filter_eligible("406") |>
    # participants who were on the waitlist at the end of the reporting period
    dplyr::filter(service_status == "On Waitlist") |>
    # exclude first day of wait where duration is zero -- only count on waitlist if they wait one day or more
    dplyr::filter(service_status_start_date != date) |>
    dplyr::distinct(date, participant_id, funder_service_code, service_name, service_status) |>
    dplyr::mutate(digits_1_3 = "406",
                  digits_4_5 = get_sr_code(funder_service_code),
                  digits_6_7 = "20") |>
    assemble_statistical_account() |>
    dplyr::count(date, funder_service_code, service_name, funder_statistical_account_code, name = "value")


  # S406 99 10 Individuals Currently Waiting for Assessment ----
  # The number of individuals referred to the organization and currently waiting for screening to determine service eligibility.
  # Request for the service has been received but no service decision has been made.
  # This reflects the number on the waiting list on the last day of reporting period.
  # This is a snapshot at a specific point in time as at September 30 (Q2), December 31 (Q3), and March 31 (Year End).
  # It is not a cumulative number at Year End.


  rlog::log_info("Generating S406 99 statistic")

  calc_fc_406_99 <- mis_daily_census |>
    filter_eligible("406 99") |>
    # filter participants who were pending assessment at the end of the reporting period
    dplyr::filter(service_status == "Waiting for Assessment") |>
    # exclude first day of wait where duration is zero -- only count as waiting for assessment if they wait one day or more  dplyr::filter(service_status_start_date != date) |>
    dplyr::distinct(date, participant_id, funder_service_code, service_status) |>
    dplyr::mutate(digits_1_3 = "406",
                  digits_4_5 = "99",
                  digits_6_7 = "10") |>
    assemble_statistical_account() |>
    dplyr::count(date, funder_service_code, funder_statistical_account_code, name = "value")

  calc_service_406_99 <- mis_daily_census |>
    filter_eligible("406 99") |>
    # filter participants who were pending assessment at the end of the reporting period
    dplyr::filter(service_status == "Waiting for Assessment") |>
    # exclude those whose start and end dates are the same -- implied they went straight to waitlist/service/non-admission
    dplyr::distinct(date, participant_id, funder_service_code, service_name, service_status) |>
    dplyr::mutate(digits_1_3 = "406",
                  digits_4_5 = "99",
                  digits_6_7 = "10") |>
    assemble_statistical_account() |>
    dplyr::count(date, funder_service_code, service_name, funder_statistical_account_code, name = "value")


  # S455 ** *0 Individuals Served by Functional Centre ----
  # This statistical account is a year-to-date count of the number of individuals served by the functional centre in a reporting period and identified by a unique identifier.  Individuals are counted only once within the functional centre within a fiscal year, regardless of how many different services they have received or the number of times they were admitted and discharged within the reporting period.  This account is reported in the functional centre where the service was received.  An individual may receive services from several functional centres during the same reporting period.  This count cannot be summed for a “total” for the whole organization to report S855** Total Individuals Served by the Organization.
  # Note: Only individuals who have received service in a reporting period can be counted.
  # 	Service recipient category is required. Reporting with age category –Not Known, S455** 90 is not recommended
  # Note: All the S455* counts in the various functional centres cannot be added together to calculate a total for the whole organization.  See S855**76 for a total unique count of the Total Individuals Served by the Organization.
  # Note: the number of individuals reported using account S455****, Individuals Served by Functional Centre, within a functional centre cannot be greater than the number individuals served by the organization in account S855**76.


  rlog::log_info("Generating S455 statistic")

  if(fc_455_version == "verA"){
    ## Service enrollment with at least one visit calculation
    calc_fc_455 <- mis_daily_census |>
      filter_eligible("455") |>
      dplyr::filter(service_status == "Active") |>
      dplyr::distinct(date, participant_id, funder_service_code, participant_funder_age_group_code) |>

      # join against visits in period (i.e. must be enrolled and have at least one visit)
      dplyr::semi_join(mis_visits, by = c("date" = "activity_date", "participant_id", "funder_service_code")) |>

      # get min. age group code for fiscal year (i.e. age group on latest visit/end of period)
      dplyr::mutate(fiscal_year = lubridate::quarter(date, fiscal_start = 4) |> stringr::str_sub(1, 4)) |>
      dplyr::group_by(participant_id, funder_service_code, fiscal_year) |>
      dplyr::mutate(participant_funder_age_group_code = min(participant_funder_age_group_code)) |>
      dplyr::ungroup() |>

      # assemble funder_statistical_account_code
      dplyr::mutate(digits_1_3 = "455",
                    digits_4_5 = get_sr_code(funder_service_code),
                    digits_6_7 = participant_funder_age_group_code) |>
      assemble_statistical_account() |>
      dplyr::select(date, funder_service_code, funder_statistical_account_code, participant_id)

    calc_service_455 <- mis_daily_census |>
      filter_eligible("455") |>
      dplyr::filter(service_status == "Active") |>
      dplyr::distinct(date, participant_id, funder_service_code, service_name, participant_funder_age_group_code) |>

      # join against visits in period (i.e. must be enrolled and have at least one visit)
      dplyr::semi_join(mis_visits, by = c("date" = "activity_date", "participant_id", "funder_service_code", "service_name")) |>

      # get min. age group code for fiscal year (i.e. age group on latest visit/end of period)
      dplyr::mutate(fiscal_year = lubridate::quarter(date, fiscal_start = 4) |> stringr::str_sub(1, 4)) |>
      dplyr::group_by(participant_id, funder_service_code, service_name, fiscal_year) |>
      dplyr::mutate(participant_funder_age_group_code = min(participant_funder_age_group_code)) |>
      dplyr::ungroup() |>

      # assemble funder_statistical_account_code
      dplyr::mutate(digits_1_3 = "455",
                    digits_4_5 = get_sr_code(funder_service_code),
                    digits_6_7 = participant_funder_age_group_code) |>
      assemble_statistical_account() |>
      dplyr::select(date, funder_service_code, funder_statistical_account_code, service_name, participant_id)
  }

  if(fc_455_version == "verB"){
    ## Visit-based calculation
    calc_fc_455 <- mis_visits |>
      filter_eligible("455") |>
      # remove indirect and clinical entries
      dplyr::filter(stringr::str_detect(activity_type, "ace-to-face")) |>
      dplyr::distinct(activity_date, participant_id, funder_service_code, participant_funder_age_group_code) |>

      # get min. age group code for fiscal year (i.e. age group on latest visit/end of period)
      dplyr::mutate(fiscal_year = lubridate::quarter(activity_date, fiscal_start = 4) |> stringr::str_sub(1, 4)) |>
      dplyr::group_by(participant_id, funder_service_code, fiscal_year) |>
      dplyr::mutate(participant_funder_age_group_code = min(participant_funder_age_group_code)) |>
      dplyr::ungroup() |>

      # assemble funder_statistical_account_code
      dplyr::mutate(digits_1_3 = "455",
                    digits_4_5 = get_sr_code(funder_service_code),
                    digits_6_7 = participant_funder_age_group_code) |>
      assemble_statistical_account() |>
      dplyr::rename(date = activity_date) |>
      dplyr::select(date, funder_service_code, funder_statistical_account_code, participant_id)

    calc_service_455 <- mis_visits |>
      filter_eligible("455") |>
      # remove indirect and clinical entries
      dplyr::filter(stringr::str_detect(activity_type, "ace-to-face")) |>
      dplyr::distinct(activity_date, participant_id, funder_service_code, service_name, participant_funder_age_group_code) |>

      # get min. age group code for fiscal year (i.e. age group on latest visit/end of period)
      dplyr::mutate(fiscal_year = lubridate::quarter(activity_date, fiscal_start = 4) |> stringr::str_sub(1, 4)) |>
      dplyr::group_by(participant_id, funder_service_code, service_name, fiscal_year) |>
      dplyr::mutate(participant_funder_age_group_code = min(participant_funder_age_group_code)) |>
      dplyr::ungroup() |>

      # assemble funder_statistical_account_code
      dplyr::mutate(digits_1_3 = "455",
                    digits_4_5 = get_sr_code(funder_service_code),
                    digits_6_7 = participant_funder_age_group_code) |>
      assemble_statistical_account() |>
      dplyr::rename(date = activity_date) |>
      dplyr::select(date, funder_service_code, service_name, funder_statistical_account_code, participant_id)
  }

  if(fc_455_version == "verC"){
    ## Service enrollment-based calculation with
    calc_fc_455 <- mis_daily_census |>
      filter_eligible("455") |>
      dplyr::filter(service_status == "Active") |>
      dplyr::distinct(date, participant_id, funder_service_code, participant_funder_age_group_code) |>

      # get min. age group code for fiscal year (i.e. age group on latest visit/end of period)
      dplyr::mutate(fiscal_year = lubridate::quarter(date, fiscal_start = 4) |> stringr::str_sub(1, 4)) |>
      dplyr::group_by(participant_id, funder_service_code, fiscal_year) |>
      dplyr::mutate(participant_funder_age_group_code = min(participant_funder_age_group_code)) |>
      dplyr::ungroup() |>

      # assemble funder_statistical_account_code
      dplyr::mutate(digits_1_3 = "455",
                    digits_4_5 = get_sr_code(funder_service_code),
                    digits_6_7 = participant_funder_age_group_code) |>
      assemble_statistical_account() |>
      dplyr::select(date, funder_service_code, funder_statistical_account_code, participant_id)

    calc_service_455 <- mis_daily_census |>
      filter_eligible("455") |>
      dplyr::filter(service_status == "Active") |>
      dplyr::distinct(date, participant_id, funder_service_code, service_name, participant_funder_age_group_code) |>

      # get min. age group code for fiscal year (i.e. age group on latest visit/end of period)
      dplyr::mutate(fiscal_year = lubridate::quarter(date, fiscal_start = 4) |> stringr::str_sub(1, 4)) |>
      dplyr::group_by(participant_id, funder_service_code, service_name, fiscal_year) |>
      dplyr::mutate(participant_funder_age_group_code = min(participant_funder_age_group_code)) |>
      dplyr::ungroup() |>

      # assemble funder_statistical_account_code
      dplyr::mutate(digits_1_3 = "455",
                    digits_4_5 = get_sr_code(funder_service_code),
                    digits_6_7 = participant_funder_age_group_code) |>
      assemble_statistical_account() |>
      dplyr::select(date, funder_service_code, funder_statistical_account_code, service_name, participant_id)
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


  if(fc_455_version == "verA"){
    ## Service enrollment with at least one visit calculation
    calc_org_855_css <- mis_daily_census |>
      dplyr::filter(service_status == "Active", get_sr_code(funder_service_code) == "80") |>
      dplyr::distinct(date, participant_id,  participant_funder_age_group_code) |>

      # join against visits in period (i.e. must be enrolled and have at least one visit)
      dplyr::semi_join(mis_visits, by = c("date" = "activity_date", "participant_id")) |>

      # get min. age group code for fiscal year (i.e. age group on latest visit/end of period)
      dplyr::mutate(fiscal_year = lubridate::quarter(date, fiscal_start = 4) |> stringr::str_sub(1, 4)) |>
      dplyr::group_by(participant_id, fiscal_year) |>
      dplyr::mutate(participant_funder_age_group_code = min(participant_funder_age_group_code)) |>
      dplyr::ungroup() |>

      # add organizational functional centre
      dplyr::mutate(funder_service_code = "82 9 90") |>

      # assemble funder_statistical_account_code
      dplyr::mutate(digits_1_3 = "855",
                    digits_4_5 = "80",
                    digits_6_7 = paste0("5", str_sub(participant_funder_age_group_code, 1, 1))) |>
      assemble_statistical_account() |>
      dplyr::select(date, funder_service_code, funder_statistical_account_code, participant_id)

    calc_org_855_cmha <- mis_daily_census |>
      dplyr::filter(service_status == "Active", get_sr_code(funder_service_code) != "80") |>
      dplyr::distinct(date, participant_id,  participant_funder_age_group_code) |>

      # join against visits in period (i.e. must be enrolled and have at least one visit)
      dplyr::semi_join(mis_visits, by = c("date" = "activity_date", "participant_id")) |>

      # get min. age group code for fiscal year (i.e. age group on latest visit/end of period)
      dplyr::mutate(fiscal_year = lubridate::quarter(date, fiscal_start = 4) |> stringr::str_sub(1, 4)) |>
      dplyr::group_by(participant_id, fiscal_year) |>
      dplyr::mutate(participant_funder_age_group_code = min(participant_funder_age_group_code)) |>
      dplyr::ungroup() |>

      # add organizational functional centre
      dplyr::mutate(funder_service_code = "82 9 90") |>

      # assemble funder_statistical_account_code
      dplyr::mutate(digits_1_3 = "855",
                    digits_4_5 = "25",
                    digits_6_7 = paste0("5", str_sub(participant_funder_age_group_code, 1, 1))) |>
      assemble_statistical_account() |>
      dplyr::select(date, funder_service_code, funder_statistical_account_code, participant_id)
  }

  if(fc_455_version == "verB"){
    ## Visit-based calculation
    calc_org_855_css <- mis_visits |>
      # remove indirect and clinical entries
      dplyr::filter(stringr::str_detect(activity_type, "ace-to-face"), get_sr_code(funder_service_code) == "80") |>
      dplyr::distinct(activity_date, participant_id, participant_funder_age_group_code) |>

      # get min. age group code for fiscal year (i.e. age group on latest visit/end of period)
      dplyr::mutate(fiscal_year = lubridate::quarter(activity_date, fiscal_start = 4) |> stringr::str_sub(1, 4)) |>
      dplyr::group_by(participant_id,  fiscal_year) |>
      dplyr::mutate(participant_funder_age_group_code = min(participant_funder_age_group_code)) |>
      dplyr::ungroup() |>

      # add organizational functional centre
      dplyr::mutate(funder_service_code = "82 9 90") |>

      # assemble funder_statistical_account_code
      dplyr::mutate(digits_1_3 = "855",
                    digits_4_5 = "80",
                    digits_6_7 = paste0("5", str_sub(participant_funder_age_group_code, 1, 1))) |>
      assemble_statistical_account() |>
      dplyr::rename(date = activity_date) |>
      dplyr::select(date, funder_service_code, funder_statistical_account_code, participant_id)

    calc_org_855_cmha <- mis_visits |>
      # remove indirect and clinical entries
      dplyr::filter(stringr::str_detect(activity_type, "ace-to-face"), get_sr_code(funder_service_code) != "80") |>
      dplyr::distinct(activity_date, participant_id, participant_funder_age_group_code) |>

      # get min. age group code for fiscal year (i.e. age group on latest visit/end of period)
      dplyr::mutate(fiscal_year = lubridate::quarter(activity_date, fiscal_start = 4) |> stringr::str_sub(1, 4)) |>
      dplyr::group_by(participant_id,  fiscal_year) |>
      dplyr::mutate(participant_funder_age_group_code = min(participant_funder_age_group_code)) |>
      dplyr::ungroup() |>

      # add organizational functional centre
      dplyr::mutate(funder_service_code = "82 9 90") |>

      # assemble funder_statistical_account_code
      dplyr::mutate(digits_1_3 = "855",
                    digits_4_5 = "25",
                    digits_6_7 = paste0("5", str_sub(participant_funder_age_group_code, 1, 1))) |>
      assemble_statistical_account() |>
      dplyr::rename(date = activity_date) |>
      dplyr::select(date, funder_service_code, funder_statistical_account_code, participant_id)
  }

  if(fc_455_version == "verC"){
    ## Service enrollment-based calculation
    calc_org_855_css <- mis_daily_census |>
      dplyr::filter(service_status == "Active", get_sr_code(funder_service_code) == "80") |>
      dplyr::distinct(date, participant_id,  participant_funder_age_group_code) |>

      # get min. age group code for fiscal year (i.e. age group on latest visit/end of period)
      dplyr::mutate(fiscal_year = lubridate::quarter(date, fiscal_start = 4) |> stringr::str_sub(1, 4)) |>
      dplyr::group_by(participant_id, fiscal_year) |>
      dplyr::mutate(participant_funder_age_group_code = min(participant_funder_age_group_code)) |>
      dplyr::ungroup() |>

      # add organizational functional centre
      dplyr::mutate(funder_service_code = "82 9 90") |>

      # assemble funder_statistical_account_code
      dplyr::mutate(digits_1_3 = "855",
                    digits_4_5 = "80",
                    digits_6_7 = paste0("5", str_sub(participant_funder_age_group_code, 1, 1))) |>
      assemble_statistical_account() |>
      dplyr::select(date, funder_service_code, funder_statistical_account_code, participant_id)

    calc_org_855_cmha <- mis_daily_census |>
      dplyr::filter(service_status == "Active", get_sr_code(funder_service_code) != "80") |>
      dplyr::distinct(date, participant_id,  participant_funder_age_group_code) |>

      # get min. age group code for fiscal year (i.e. age group on latest visit/end of period)
      dplyr::mutate(fiscal_year = lubridate::quarter(date, fiscal_start = 4) |> stringr::str_sub(1, 4)) |>
      dplyr::group_by(participant_id, fiscal_year) |>
      dplyr::mutate(participant_funder_age_group_code = min(participant_funder_age_group_code)) |>
      dplyr::ungroup() |>

      # add organizational functional centre
      dplyr::mutate(funder_service_code = "82 9 90") |>

      # assemble funder_statistical_account_code
      dplyr::mutate(digits_1_3 = "855",
                    digits_4_5 = "25",
                    digits_6_7 = paste0("5", str_sub(participant_funder_age_group_code, 1, 1))) |>
      assemble_statistical_account() |>
      dplyr::select(date, funder_service_code, funder_statistical_account_code, participant_id)
  }


  # S950 ** 10 Francophone Individuals Requested Service/Served in French (4th, 5th = SR code) ----
  # The following S950* accounts are available for reporting services provided to Francophone clients.  For CSS HSPs, it is mandatory to report S9508010 and S9501080 for individuals requested service in French by functional centre and organization level respectively.  S9508020 and S9502080 for individuals served in French are optional reporting.
  # •	S950 ** 10 Francophone Individuals Requested Service in French by Functional Centre
  # •	S950 ** 20 Francophone Individuals Served in French by Functional Centre
  # •	S950 10 ** Francophone Individuals Requested Service in French by the Organization
  # •	S950 20 ** Francophone Individuals Served in French by the Organization


  rlog::log_info("Generating S950 statistic")

  if(fc_455_version == "verA"){
    ## Service enrollment with at least one visit calculation
    calc_org_950_10 <- mis_daily_census |>
      filter_eligible("950") |>
      dplyr::filter(service_status == "Active" & participant_preferred_language == "French") |>
      dplyr::distinct(date, participant_id) |>

      # join against visits in period (i.e. must be enrolled and have at least one visit)
      dplyr::semi_join(mis_visits, by = c("date" = "activity_date", "participant_id")) |>

      # add organizational functional centre
      dplyr::mutate(funder_service_code = "82 9 90") |>

      # assemble funder_statistical_account_code
      dplyr::mutate(digits_1_3 = "950",
                    digits_4_5 = "10",
                    digits_6_7 = "80") |>
      assemble_statistical_account() |>
      dplyr::select(date, funder_service_code, funder_statistical_account_code, participant_id)

    calc_fc_950 <- mis_daily_census |>
      filter_eligible("950") |>
      dplyr::filter(service_status == "Active" & participant_preferred_language == "French") |>
      dplyr::distinct(date, participant_id, funder_service_code) |>

      # join against visits in period (i.e. must be enrolled and have at least one visit)
      dplyr::semi_join(mis_visits, by = c("date" = "activity_date", "participant_id", "funder_service_code")) |>

      # assemble funder_statistical_account_code
      dplyr::mutate(digits_1_3 = "950",
                    digits_4_5 = "80",
                    digits_6_7 = "10") |>
      assemble_statistical_account() |>
      dplyr::select(date, funder_service_code, funder_statistical_account_code, participant_id)

    calc_service_950 <- mis_daily_census |>
      filter_eligible("950") |>
      dplyr::filter(service_status == "Active" & participant_preferred_language == "French") |>
      dplyr::distinct(date, participant_id, funder_service_code, service_name) |>

      # join against visits in period (i.e. must be enrolled and have at least one visit)
      dplyr::semi_join(mis_visits, by = c("date" = "activity_date", "participant_id", "funder_service_code", "service_name")) |>

      # assemble funder_statistical_account_code
      dplyr::mutate(digits_1_3 = "950",
                    digits_4_5 = "80",
                    digits_6_7 = "10") |>
      assemble_statistical_account() |>
      dplyr::select(date, funder_service_code, service_name, funder_statistical_account_code, participant_id)
  }

  if(fc_455_version == "verB"){
    ## Visit-based calculation
    calc_org_950_10 <- mis_visits |>
      filter_eligible("950") |>
      # remove indirect and clinical entries
      dplyr::filter(stringr::str_detect(activity_type, "ace-to-face")) |>
      dplyr::filter(participant_preferred_language == "French") |>

      # add organizational functional centre
      dplyr::mutate(funder_service_code = "82 9 90") |>

      # assemble funder_statistical_account_code
      dplyr::mutate(digits_1_3 = "950",
                    digits_4_5 = "10",
                    digits_6_7 = "80") |>
      assemble_statistical_account() |>
      dplyr::rename(date = activity_date) |>
      dplyr::distinct(date, funder_service_code, funder_statistical_account_code, participant_id)

    calc_fc_950 <- mis_visits |>
      filter_eligible("950") |>
      # remove indirect and clinical entries
      dplyr::filter(stringr::str_detect(activity_type, "ace-to-face")) |>
      dplyr::filter(participant_preferred_language == "French") |>

      # assemble funder_statistical_account_code
      dplyr::mutate(digits_1_3 = "950",
                    digits_4_5 = "80",
                    digits_6_7 = "10") |>
      assemble_statistical_account() |>
      dplyr::rename(date = activity_date) |>
      dplyr::distinct(date, funder_service_code, funder_statistical_account_code, participant_id)

    calc_service_950 <- mis_visits |>
      filter_eligible("950") |>
      # remove indirect and clinical entries
      dplyr::filter(stringr::str_detect(activity_type, "ace-to-face")) |>
      dplyr::filter(participant_preferred_language == "French") |>

      # assemble funder_statistical_account_code
      dplyr::mutate(digits_1_3 = "950",
                    digits_4_5 = "80",
                    digits_6_7 = "10") |>
      assemble_statistical_account() |>
      dplyr::rename(date = activity_date) |>
      dplyr::distinct(date, funder_service_code, service_name, funder_statistical_account_code, participant_id)
  }

  if(fc_455_version == "verC"){
    ## Service enrollment-based calculation
    calc_org_950_10 <- mis_daily_census |>
      filter_eligible("950") |>
      dplyr::filter(service_status == "Active" & participant_preferred_language == "French") |>
      dplyr::distinct(date, participant_id) |>

      # add organizational functional centre
      dplyr::mutate(funder_service_code = "82 9 90") |>

      # assemble funder_statistical_account_code
      dplyr::mutate(digits_1_3 = "950",
                    digits_4_5 = "10",
                    digits_6_7 = "80") |>
      assemble_statistical_account() |>
      dplyr::select(date, funder_service_code, funder_statistical_account_code, participant_id)

    calc_fc_950 <- mis_daily_census |>
      filter_eligible("950") |>
      dplyr::filter(service_status == "Active" & participant_preferred_language == "French") |>
      dplyr::distinct(date, participant_id, funder_service_code) |>

      # assemble funder_statistical_account_code
      dplyr::mutate(digits_1_3 = "950",
                    digits_4_5 = "80",
                    digits_6_7 = "10") |>
      assemble_statistical_account() |>
      dplyr::select(date, funder_service_code, funder_statistical_account_code, participant_id)

    calc_service_950 <- mis_daily_census |>
      filter_eligible("950") |>
      dplyr::filter(service_status == "Active" & participant_preferred_language == "French") |>
      dplyr::distinct(date, participant_id, funder_service_code, service_name) |>

      # assemble funder_statistical_account_code
      dplyr::mutate(digits_1_3 = "950",
                    digits_4_5 = "80",
                    digits_6_7 = "10") |>
      assemble_statistical_account() |>
      dplyr::select(date, funder_service_code, service_name, funder_statistical_account_code, participant_id)
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

  rlog::log_info("Generating S955 statistic")

  if(fc_455_version == "verA"){
    ## Service enrollment with at least one visit calculation
    calc_org_955 <- mis_daily_census |>
      filter_eligible("955") |>
      dplyr::filter(service_status == "Active") |>
      dplyr::distinct(date, participant_id, participant_ohrs_special_needs) |>

      # join against visits in period (i.e. must be enrolled and have at least one visit)
      dplyr::semi_join(mis_visits, by = c("date" = "activity_date", "participant_id")) |>

      # add organizational functional centre
      dplyr::mutate(funder_service_code = "82 9 90") |>

      # assemble funder_statistical_account_code
      dplyr::mutate(digits_1_3 = "955",
                    digits_4_5 = get_sr_code(funder_service_code),
                    digits_6_7 = dplyr::case_when(participant_ohrs_special_needs == "Physical Disability" ~ "10",
                                                  participant_ohrs_special_needs == "Cognitive Impairment" ~ "15",
                                                  participant_ohrs_special_needs == "Frail And-or Elderly" ~ "20",
                                                  participant_ohrs_special_needs == "High Risk Senior" ~ "22",
                                                  participant_ohrs_special_needs == "Living With Effects Of HIV-AIDS" ~ "25")) |>
      assemble_statistical_account() |>
      dplyr::select(date, funder_service_code, funder_statistical_account_code, participant_id)

    calc_service_955 <- mis_daily_census |>
      filter_eligible("955") |>
      dplyr::filter(service_status == "Active") |>
      dplyr::distinct(date, participant_id, funder_service_code, service_name, participant_ohrs_special_needs) |>

      # join against visits in period (i.e. must be enrolled and have at least one visit)
      dplyr::semi_join(mis_visits, by = c("date" = "activity_date", "participant_id", "funder_service_code", "service_name")) |>

      # add organizational functional centre
      dplyr::mutate(funder_service_code = "82 9 90") |>

      # assemble funder_statistical_account_code
      dplyr::mutate(digits_1_3 = "955",
                    digits_4_5 = get_sr_code(funder_service_code),
                    digits_6_7 = dplyr::case_when(participant_ohrs_special_needs == "Physical Disability" ~ "10",
                                                  participant_ohrs_special_needs == "Cognitive Impairment" ~ "15",
                                                  participant_ohrs_special_needs == "Frail And-or Elderly" ~ "20",
                                                  participant_ohrs_special_needs == "High Risk Senior" ~ "22",
                                                  participant_ohrs_special_needs == "Living With Effects Of HIV-AIDS" ~ "25")) |>
      assemble_statistical_account() |>
      dplyr::select(date, funder_service_code, service_name, funder_statistical_account_code, participant_id)
  }

  if(fc_455_version == "verB"){
  ## Visit-based calculation
    calc_org_955 <- mis_visits |>
      filter_eligible("955") |>
      # remove indirect and clinical entries
      dplyr::filter(stringr::str_detect(activity_type, "ace-to-face")) |>
      dplyr::filter(!is.na(participant_ohrs_special_needs)) |>
      dplyr::distinct(activity_date, participant_id, funder_service_code, participant_ohrs_special_needs) |>

      # assemble funder_statistical_account_code
      dplyr::mutate(digits_1_3 = "955",
                    digits_4_5 = get_sr_code(funder_service_code),
                    digits_6_7 = dplyr::case_when(participant_ohrs_special_needs == "Physical Disability" ~ "10",
                                     participant_ohrs_special_needs == "Cognitive Impairment" ~ "15",
                                     participant_ohrs_special_needs == "Frail And-or Elderly" ~ "20",
                                     participant_ohrs_special_needs == "High Risk Senior" ~ "22",
                                     participant_ohrs_special_needs == "Living With Effects Of HIV-AIDS" ~ "25")) |>
      assemble_statistical_account() |>
      dplyr::rename(date = activity_date) |>
      dplyr::select(date, funder_service_code, funder_statistical_account_code, participant_id)

    calc_service_955 <- mis_visits |>
      filter_eligible("955") |>
      # remove indirect and clinical entries
      dplyr::filter(stringr::str_detect(activity_type, "ace-to-face")) |>
      dplyr::filter(!is.na(participant_ohrs_special_needs)) |>
      dplyr::distinct(activity_date, participant_id, funder_service_code, service_name, participant_ohrs_special_needs) |>

      # assemble funder_statistical_account_code
      dplyr::mutate(digits_1_3 = "955",
                    digits_4_5 = get_sr_code(funder_service_code),
                    digits_6_7 =  dplyr::case_when(participant_ohrs_special_needs == "Physical Disability" ~ "10",
                                     participant_ohrs_special_needs == "Cognitive Impairment" ~ "15",
                                     participant_ohrs_special_needs == "Frail And-or Elderly" ~ "20",
                                     participant_ohrs_special_needs == "High Risk Senior" ~ "22",
                                     participant_ohrs_special_needs == "Living With Effects Of HIV-AIDS" ~ "25")) |>
      assemble_statistical_account() |>
      dplyr::rename(date = activity_date) |>
      dplyr::select(date, funder_service_code, service_name, funder_statistical_account_code, participant_id)
  }

  if(fc_455_version == "verC"){
    ## Service enrollment-based calculation
    calc_org_955 <- mis_daily_census |>
      filter_eligible("955") |>
      dplyr::filter(service_status == "Active") |>
      dplyr::distinct(date, participant_id, participant_ohrs_special_needs) |>

      # add organizational functional centre
      dplyr::mutate(funder_service_code = "82 9 90") |>

      # assemble funder_statistical_account_code
      dplyr::mutate(digits_1_3 = "955",
                    digits_4_5 = get_sr_code(funder_service_code),
                    digits_6_7 = dplyr::case_when(participant_ohrs_special_needs == "Physical Disability" ~ "10",
                                                  participant_ohrs_special_needs == "Cognitive Impairment" ~ "15",
                                                  participant_ohrs_special_needs == "Frail And-or Elderly" ~ "20",
                                                  participant_ohrs_special_needs == "High Risk Senior" ~ "22",
                                                  participant_ohrs_special_needs == "Living With Effects Of HIV-AIDS" ~ "25")) |>
      assemble_statistical_account() |>
      dplyr::select(date, funder_service_code, funder_statistical_account_code, participant_id)

    calc_service_955 <- mis_daily_census |>
      filter_eligible("955") |>
      dplyr::filter(service_status == "Active") |>
      dplyr::distinct(date, participant_id, funder_service_code, service_name, participant_ohrs_special_needs) |>

      # add organizational functional centre
      dplyr::mutate(funder_service_code = "82 9 90") |>

      # assemble funder_statistical_account_code
      dplyr::mutate(digits_1_3 = "955",
                    digits_4_5 = get_sr_code(funder_service_code),
                    digits_6_7 = dplyr::case_when(participant_ohrs_special_needs == "Physical Disability" ~ "10",
                                                  participant_ohrs_special_needs == "Cognitive Impairment" ~ "15",
                                                  participant_ohrs_special_needs == "Frail And-or Elderly" ~ "20",
                                                  participant_ohrs_special_needs == "High Risk Senior" ~ "22",
                                                  participant_ohrs_special_needs == "Living With Effects Of HIV-AIDS" ~ "25")) |>
      assemble_statistical_account() |>
      dplyr::select(date, funder_service_code, service_name, funder_statistical_account_code, participant_id)
  }


  # Write mis_census_counts_by_fc to file ----

  rlog::log_info("Writing census-type statistics files")

  mis_census_counts_by_fc <- dplyr::bind_rows(calc_fc_403, calc_fc_406, calc_fc_406_99, calc_fc_455, calc_org_855_css, calc_org_855_cmha, calc_org_950_10, calc_fc_950, calc_org_955) |>
    dplyr::mutate(funder_key = paste0(funder_service_code, "_", funder_statistical_account_code))

  readr::write_csv(mis_census_counts_by_fc, paste0(data_folder, "/processed/mis_census_counts_by_fc.csv"), na = "")


  mis_census_counts_by_service <- dplyr::bind_rows(calc_service_403, calc_service_406, calc_service_406_99, calc_service_455, calc_service_950, calc_service_955) |>
    dplyr::mutate(funder_service_key = paste0(funder_service_code, "_", service_name, "_", funder_statistical_account_code))

  readr::write_csv(mis_census_counts_by_service, paste0(data_folder, "/processed/mis_census_counts_by_service.csv"), na = "")

}
