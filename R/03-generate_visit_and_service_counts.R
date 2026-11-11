#' Generate vist and service counts
#'
#' @param processed_data
#'
#' @return
#' @export
#'
#' @examples
generate_visit_and_service_counts <- function(processed_data,
                                              fc_450_version = "verA",
                                              fc_506_version = "verA",
                                              ir_version = "verCMHA"){

  rlog::log_info(glue::glue("Applying fc_450_version = '{fc_450_version}', fc_506_version = '{fc_506_version}', and ir_version = '{ir_version}' to visit and service counts"))

  data_folder <- processed_data$data_folder
  report_date <- processed_data$report_date
  mis_service_history <- processed_data$mis_service_history
  mis_visits <- processed_data$mis_visits
  mis_unregistered_interactions <- processed_data$mis_unregistered_interactions
  mis_daily_census <- processed_data$mis_daily_census

  # Apply fc_450_version (same rule as in generate_census_counts)
  mis_visits <- apply_fc_450_version(mis_visits, fc_450_version)

  # Each statistic builds its rows once (calc_*), then report_*(grain) returns one detail row
  # per counted unit by FC ("fc") or by service within an FC ("service"); see grain_cols()
  # and detail_by_grain(). The output tables at the end are the sums of the detail rows.
  # Organization-level statistics (S501, S511, reported in AC 82 9 90) have no grain.

  # S248 ** ** Meals Delivered (Number of)
  # Report for FC 72 5 82 10 (Meals Delivery Services) at combined or detailed (hot meal, frozen meal and side dish) level but not both.  Beverages are not counted.  Refer to Section 10.10.3 FC 72 5 82 10 for examples.
  #
  # Report the number of meals delivered at combined or detailed level but not both.
  # •	S248**10 Meal (combined) = For 1 hot meal, count as 1; for 1 frozen meal, count as 1; for side dish (additional order) – count every 2 side dishes as 1 meal.
  # •	S248**12 Hot meal = up to 3 course meal.
  # •	S248**14 Frozen meal = 1 meal package which usually comes with a side dish.
  # •	S248**16 Side Dish = Additional order, excluding beverage, such as a salad, soup or dessert.  Count only when purchased separately, i.e. when not included under the 3-course hot meal or frozen meal.
  # •	Field to Table and Snack Boxes = 2.5 and 0.25 meal respectively.  These are based on the relative costs to a meal and are recorded by providers offering grocery-type shopping and delivery services.

  rlog::log_info("Generating S248 statistic")

  calc_248 <- mis_visits |>
    filter_eligible("248") |>
    add_statistical_account(statistic = "248",
                            sr_category = "80",
                            detail = dplyr::case_when(stringr::str_detect(service_sub_service_name, "Hot") ~ "12",
                                                      stringr::str_detect(service_sub_service_name, "Cold") ~ "12",
                                                      stringr::str_detect(service_sub_service_name, "Frozen") ~ "14",
                                                      stringr::str_detect(service_sub_service_name, "Soup") |
                                                        stringr::str_detect(service_sub_service_name, "Dessert") |
                                                        stringr::str_detect(service_sub_service_name, "Side") ~ "16")) |>
    dplyr::rename(date = activity_date)

  report_248 <- function(grain) {
    calc_248 |>
      detail_by_grain(grain, value = na_to_zero(activity_count), reference = activity_id)
  }


  # S265 ** ** Service Provider Interactions with Time Intervals ----
  # A service provider interaction is reported each time service recipient activity (S4* series) is provided. The service recipient (SR) and/or significant others must be present during the interaction and the service provided must be longer than 5 minutes and documented.
  # If a service provider serves the SR multiple times, report each service provider interaction.
  # If a multi-disciplinary team provides service to a SR in the same FC, report a service provider interaction for each member of the team who provided the service.
  # Service provider interactions are only provided by UPP/NP/MED staff.
  # Each interaction may be reported according to the length of time a service provider provided direct service to the service recipient.
  # Note:  The term “present” does not only refer to the SR who is present during the face to face interaction; it also includes interactions via telephone or emails/ chats/videoconferencing. The service must be provided longer than five minutes and documented.
  # ** 4 & 5 digit represents SR
  # ** 6 & 7 digit represents time intervals:
  #   00 - Time interval not reported
  # 01 - More than 5 minutes to 30 minutes
  # 02 - 31 minutes to 1 hour
  # 03 - More than 1 hour to 2 hours
  # 04 - More than 2 hours to 5 hours
  # 05 - More than 5 hours
  #
  # The reporting of service provider interactions with time intervals provides information regarding the service provider activity, the service intensity and complexity.  Staff may report how many times they provided care to the service recipient detailed by how much time was spent with the SR for each contact.
  # Note:
  # •	Service Provider Interactions are not reported for contracted – out services
  # •	Service Provider Interactions cannot be reported when service is provided to not uniquely identified service recipients (SR 65).

  rlog::log_info("Generating S265 statistic")

  calc_265 <- mis_visits |>
    filter_eligible("265") |>
    # individual interactions only
    dplyr::filter(activity_individual_group == "Individual") |>
    # remove indirect and clinical entries
    dplyr::filter(stringr::str_detect(activity_type, "ace-to-face")) |>
    add_statistical_account(statistic = "265",
                            sr_category = default_sr_category(funder_service_code),
                            detail = get_time_intervals(activity_duration)) |>
    dplyr::rename(date = activity_date)

  report_265 <- function(grain) {
    calc_265 |>
      detail_by_grain(grain, value = na_to_zero(activity_count * activity_worker_count), reference = activity_id)
  }


  # S266 ** ** Service Provider Group Interactions with Time Intervals ----
  # A service provider group interaction is reported each time service activity is provided to the participants during an organized or structured group session.  Activity must be longer than 5 minutes and documented.
  # If a multi-disciplinary team provides service in a group session in the same FC, report a service provider group interaction for each member of the team who provided the service.
  # Each group interaction may be reported according to the length of time a service provider provided service to the group participants.
  # ** 6 & 7 digit represents time intervals:
  # 00 – Time interval not reported
  # 01 – More than 5 minutes to 30 minutes
  # 02 – 31 minutes to 1 hour
  # 03 – More than 1 hour to 2 hours
  # 04 – More than 2 hours to 5 hours
  # 05 – More than 5 hours
  # Service provider interactions for group sessions are reported using a separate statistical account because the staff resources provided for individual and group services is different and significant volume of services are providing in group sessions.

  rlog::log_info("Generating S266 statistic")

  calc_266 <- mis_visits |>
    dplyr::bind_rows(mis_unregistered_interactions) |>
    filter_eligible("266") |>
    dplyr::filter(activity_individual_group =="Group") |>
    add_statistical_account(statistic = "266",
                            sr_category = "00",
                            detail = get_time_intervals(activity_duration)) |>
    dplyr::rename(date = activity_date)

  report_266 <- function(grain) {
    calc_266 |>
      dplyr::distinct(dplyr::pick(dplyr::all_of(c("date", grain_cols(grain), "activity_group_id", "funder_statistical_account_code", "activity_worker_count")))) |>
      detail_by_grain(grain, value = na_to_zero(activity_worker_count), reference = activity_group_id)
  }


  # S401 4* *0 Resident Admissions (to the Functional Centre) ----
  # An individual is counted each time they are admitted to a bed in a CMH&A 7*5 40* residential service functional centre. Age category breakdown and service recipient category are required. The service recipient (SR) category is 45 or 44 (as noted in the 4th and 5th digits of the account number above).
  # Note:  S401 4* *0 Resident Admissions must be reported for all residential services functional centres, 7* 5 40 ** **, except the Supportive Housing functional centres (refer to section 7.6.8 for the list of Supportive Housing functional centres).  That is, if your organization is responsible for a residential treatment facility where clients reside for a period of time to receive treatment, this statistic must be reported each time a client arrives at your facility.
  # There is an optional edit rule for the trial balance submission to ensure this statistic is reported.  It is possible that for a reporting period, such as the Q2 trial balance submission, that there are no new admissions to report which is the reason for the trial balance edit rule being optional.  However, if any new admissions occur within a reporting period this statistic must be reported.

  rlog::log_info("Generating S401 statistic")

  # One admission per episode, on the day service first started (see fc_episodes())
  calc_401 <- mis_service_history |>
    filter_eligible("401")

  report_401 <- function(grain) {
    fc_episodes(calc_401, grain, report_date) |>
      dplyr::filter(!is.na(first_active_date)) |>
      add_statistical_account(statistic = "401",
                              sr_category = default_sr_category(funder_service_code),
                              detail = participant_funder_age_group_code) |>
      dplyr::rename(date = first_active_date) |>
      detail_by_grain(grain, reference = enrollment_ids, note = dated_note("Accepted", merged_start_date))
  }


  # S407 ** 20 Days Waited for Service Initiation ----
  # The number of days waited from the accepted for service date to service initiation date (date of actual “first” service).
  # These days are reported after the service has started and the client is no longer waiting
  # This statistic is recorded from the date that the client is deemed eligible for service and not from the date the service is dplyr::arranged.
  # This statistic is a cumulative figure number, year-to-date value and is recorded in the service delivery functional centre.  Service Recipient Category required.
  # The S407**20 does not include days waited when the service is available to the service recipient but the client requests to delay initiation of service.

  #RULES - only counted if at least one service received, and from date of first service
  # figure out if they were waitlisted (Status == "On Waitlist") and if service has since started !is.na(service_status_end_date)

  rlog::log_info("Generating S407 statistic")

  # Days On Waitlist in each episode where service started, reported on the day it started
  # (service starts at enrollment into the FC, Ch. 7 s7.7.8, step 8). Days stop when On
  # Waitlist ends, so On Hold days aren't counted. Waits that ended without service
  # (withdrawal, discharge) and people still waiting aren't counted. A return to the waitlist
  # after service started begins a new episode (see number_episodes()), so that wait counts
  # when service starts again.
  calc_407 <- mis_service_history |>
    filter_eligible("407") |>
    dplyr::group_by(service_enrollment_id, episode) |>
    dplyr::mutate(service_start_date = first_date(service_status_start_date[service_status == "Active"])) |>
    dplyr::ungroup() |>
    dplyr::filter(service_status == "On Waitlist", service_status_start_date < service_start_date)

  report_407 <- function(grain) {
    calc_407 |>
      earliest_overlaps(service_status_start_date, service_status_end_date, report_date, merge_level = grain, report_on = service_start_date) |>
      dplyr::mutate(days_waited = lubridate::interval(merged_start_date, merged_end_date)/lubridate::days(1)) |>
      # a same-day wait adds no days, so it has no detail row
      dplyr::filter(days_waited > 0) |>
      add_statistical_account(statistic = "407",
                              sr_category = default_sr_category(funder_service_code),
                              detail = "20") |>
      dplyr::rename(date = merged_report_date) |>
      detail_by_grain(grain, value = na_to_zero(days_waited), reference = enrollment_ids,
                      note = dated_note("Wait started", merged_start_date))
  }


  # S407 99 10 Days Waited for Assessment ----
  # The number of days a client waited from the date of application/referral to the assessment complete date by the organization.
  # This statistic is a cumulative figure and can only be recorded after the initial assessment for the client has been completed.
  # This statistic is used to produce the average wait time for client assessments (S4079910 divided by S489** *0).  If the client is not accepted for service, no days waited would be included.


  rlog::log_info("Generating S407 99 statistic")

  calc_407_99 <- mis_service_history |>
    filter_eligible("407 99") |>
    # Only waits that ended in acceptance count (Ch. 7 s7.7.6: no days if the client is not
    # accepted). service_decision_date is set when a client is made active or put on the
    # waitlist/hold, so it marks acceptance. The wait runs from referral to the decision date
    # (Ch. 7 s7.7.8, step 2). Waits with no decision yet, or that ended without one, are left out.
    dplyr::filter(service_status == "Waiting for Assessment",
                  !is.na(service_decision_date),
                  service_decision_date >= service_status_start_date,
                  service_decision_date <= report_date) |>
    dplyr::mutate(service_status_end_date = service_decision_date)

  report_407_99 <- function(grain) {
    calc_407_99 |>
      earliest_overlaps(service_status_start_date, service_status_end_date, report_date, merge_level = grain) |>
      dplyr::mutate(days_waited = lubridate::interval(merged_start_date, merged_end_date)/lubridate::days(1)) |>
      # a same-day wait adds no days, so it has no detail row
      dplyr::filter(days_waited > 0) |>
      add_statistical_account(statistic = "407",
                              sr_category = "99",
                              detail = "10") |>
      dplyr::rename(date = merged_end_date) |>
      detail_by_grain(grain, value = na_to_zero(days_waited), reference = enrollment_ids,
                      note = dated_note("Referred", merged_start_date))
  }


  # S410 4* *0 Resident Discharges (from the Functional Centre) ----
  # The official departure (from the Functional Centre) of live residents in the health service organization is recorded.
  # Individuals are counted each time clients are discharged from the CMH&A FC7*5 40* Residential Services functional centre.
  # Age category breakdown and service recipient category are required.
  # The service recipient (SR) category is 45 and/or 44 (as noted in the 4th and 5th digits of the account number above).
  # Note: S410 4**0, Resident Discharges (from the Functional Centre), must be reported for all residential services functional centres 7* 5 40 ** **, except the Supportive Housing functional centres (refer to section 7.6.8 for the list of Supportive Housing functional centres).
  # There is an edit rule for trial balance submission process to ensure this statistic is reported.  That is, if your organization is responsible for a residential treatment facility where clients reside for a period of time to receive treatment, this statistic must be reported in the residential services functional centre

  # S513 ** 90 Service Discharge (from the Functional Centre)
  # Clients/Service Recipients are discharged from a service delivery functional centre when the need for that service has ended or as per discharge criteria. If the client is registered with more than one functional centre, their organization file will remain open until all service plans have been completed or the client no longer requires service from the organization for any other reason.
  # Note: Service discharges from residential functional centre 7* 540 * are counted in  S410 45 *0 Resident Discharge (from functional centre).


  # S411 ** *0 Inpatient/Resident Deaths
  # Reported instead of S410 in the CSS residential FCs when the client died while in their care
  # (S410 counts live discharges only). OHRS Ch. 10, 10.9.4.3.3.

  rlog::log_info("Generating S410, S411 and S513 statistics")

  ## CSS residential FCs report S410 (or S411 for a death) instead of S513
  ## (OHRS Ch. 10, 10.9.4.3.3: 7* 5 82 40, 7* 5 82 45, 7* 5 83 45)
  css_residential_fcs <- c("72 5 82 40", "72 5 82 45", "72 5 83 45",
                           "73 5 82 40", "73 5 82 45", "73 5 83 45")

  # Counted when an FC episode ends (see fc_episodes()). A discharge is counted only if service
  # started in the episode, so S513 pairs with S506 and S410 with S401; a client who leaves the
  # waitlist without service is not discharged (OHRS Ch. 7, 7.7.4 and 7.7.8 step 12).
  fc_involvement <- mis_service_history |>
    filter_eligible("410|513")

  ## CSS version with discharge reason
  calc_410_513_css <- fc_involvement |>
    dplyr::filter(default_sr_category(funder_service_code) == "80")

  report_410_513_css <- function(grain) {
    fc_episodes(calc_410_513_css, grain, report_date) |>
      dplyr::filter(merged_end_date <= report_date, ever_active) |>

      # reason from the enrollment discharged last
      dplyr::mutate(service_status_reason_code = stringr::str_sub(last_service_status_reason, 1, 2),
                    service_status_reason_code = dplyr::if_else(stringr::str_detect(service_status_reason_code, "^\\d\\d$"), service_status_reason_code, "90", missing = "90")) |>
      # reason 20 = death while in the care of the organization
      add_statistical_account(statistic = dplyr::case_when(funder_service_code %in% css_residential_fcs & service_status_reason_code == "20" ~ "411",
                                                           funder_service_code %in% css_residential_fcs ~ "410",
                                                           .default = "513"),
                              sr_category = default_sr_category(funder_service_code),
                              detail = dplyr::if_else(funder_service_code %in% css_residential_fcs, participant_funder_age_group_code, service_status_reason_code)) |>
      dplyr::rename(date = merged_end_date) |>
      detail_by_grain(grain, reference = enrollment_ids, note = dated_note("Admitted", first_active_date))
  }


  ## CMHA version without discharge reason
  calc_410_513_cmha <- fc_involvement |>
    dplyr::filter(default_sr_category(funder_service_code) != "80")

  report_410_513_cmha <- function(grain) {
    fc_episodes(calc_410_513_cmha, grain, report_date) |>
      dplyr::filter(merged_end_date <= report_date, ever_active) |>
      add_statistical_account(statistic = dplyr::if_else(default_sr_category(funder_service_code) == "45", "410", "513"),
                              sr_category = default_sr_category(funder_service_code),
                              detail = dplyr::if_else(default_sr_category(funder_service_code) == "45", participant_funder_age_group_code, "90")) |>
      dplyr::rename(date = merged_end_date) |>
      detail_by_grain(grain, reference = enrollment_ids, note = dated_note("Admitted", first_active_date))
  }


  # S450 ** * * Visit – Face to Face, In-House ----
  # S450***0   Visit – Face to Face, In-House - In Person
  # S450***1    Visit - Face to Face In-House -Virtual
  # where
  # 4th and 5th digit represent SR
  # 6th digit represents SR age category
  # 7th digit represents mode of delivery (in person or virtual)
  # A visit is recorded when a uniquely identified service recipient (SR) is present to receive service from an organization’s employees as face-to-face or by videoconferencing on an individual basis. This includes service to the service recipient and/or significant other(s) in attendance on behalf of the service recipient.
  # The service is documented according to the health care organization’s policy and is provided for longer than five minutes.
  # A visit is recorded when a service recipient is provided service in a functional centre (FC) regardless of the number of service providers present and the length of service. When a service recipient is present to receive service more than once on the same calendar day in the same FC only one visit is reported.
  # Excludes face-to-face interactions with service recipients who are not uniquely identified (service recipient code 65).
  # This account cannot be used if service is provided by a Physician only.

  # S451 ** *0 Visit Non-Face to Face, In-House ----
  # A visit is recorded when a uniquely identified service recipient (SR) and/or significant other in attendance receives service on an individual basis from the organization’s employees by mean other than by face-to-face.  These occasions take the place of a face to face visit.  Examples may include visits via telephone, email or other forms of electronic communication.
  # The service is documented according to the health care organization’s policy and is provided for longer than five minutes.
  # If a service recipient receives service more than once on the same calendar day in the same functional centre only one visit is reported.
  # Note: Other communication includes texting and chatting, provided the organization has the appropriate privacy policies in place to allow these forms of communication to take the place of a face to face visit.  If your organization does not have these policies, then this activity cannot be reported as a Visit – Non-Face to Face.

  # from Ch. 10:
  # If a SR is present in person as well as non face to face to receive service for the same need on the same calendar day, report 1 face to face in person visit regardless of which occurred first.


  rlog::log_info("Generating S450 and S451 statistics")

  calc_450_451_cmha <- mis_visits |>
    filter_eligible("450|451") |>
    # non-residential CMHA
    dplyr::filter(default_sr_category(funder_service_code) == "25") |>
    # individual interactions only
    dplyr::filter(activity_individual_group == "Individual") |>
    # remove indirect and clinical entries
    dplyr::filter(stringr::str_detect(activity_type, "ace-to-face")) |>

    # count only one visit per day; preferentially choose FTF over NFTF by sorting by activity_type
    dplyr::mutate(activity_type_priority = dplyr::case_when(activity_type == "Face-to-face" ~ 1,
                                              activity_type == "Face-to-face Virtual" ~ 2,
                                              .default = 3)) |>

    # assemble statistical account
    add_statistical_account(statistic = dplyr::case_when(activity_type == "Non-face-to-face" ~ "451",
                                                         activity_type %in% c("Face-to-face", "Face-to-face Virtual") ~ "450"),
                            sr_category = default_sr_category(funder_service_code),
                            detail = paste0(age_digit(participant_funder_age_group_code),
                                            dplyr::if_else(activity_type == "Face-to-face Virtual", "1", "0")))

  report_450_451_cmha <- function(grain) {
    calc_450_451_cmha |>
      dplyr::rename(date = activity_date) |>
      one_per_person_per_day(grain) |>
      detail_by_grain(grain, reference = activity_id)
  }


  calc_450_451_css <- mis_visits |>
    filter_eligible("450|451") |>
    # clients must be uniquely identified
    dplyr::filter(!is.na(participant_id)) |>
    # non-residential CSS
    dplyr::filter(default_sr_category(funder_service_code) == "80") |>
    # individual interactions only
    dplyr::filter(activity_individual_group == "Individual") |>
    # remove indirect and clinical entries; assessments count as a visit only in the
    # CSS case management FC (OHRS Ch. 10, 10.9.4.4, S512)
    dplyr::filter(stringr::str_detect(activity_type, "ace-to-face") |
                    (activity_type == "Assessment" & funder_service_code %in% css_case_management_fcs)) |>

    # assemble statistical account (assessments are reported as in-person visits)
    add_statistical_account(statistic = dplyr::case_when(activity_type == "Non-face-to-face" ~ "451",
                                                         activity_type %in% c("Face-to-face", "Face-to-face Virtual", "Assessment") ~ "450"),
                            sr_category = default_sr_category(funder_service_code),
                            detail = paste0(age_digit(participant_funder_age_group_code),
                                            dplyr::if_else(activity_type == "Face-to-face Virtual", "1", "0"))) |>
    dplyr::rename(date = activity_date)

  report_450_451_css <- function(grain) {
    calc_450_451_css |>
      detail_by_grain(grain, value = na_to_zero(activity_count), reference = activity_id)
  }


  # S452 60 00 Not Uniquely Identified Service Recipient Interactions ----
  # The number of interactions face to face, or non - face to face with a service recipient/client or significant other who is not uniquely identified. This statistic is used when a health record or unique identifier has not been generated and/or there is no documentation according to the health care organization’s policy.  Only service recipient codes 60 and 65 can be used with this account.  Examples of functional centres where this account can be used are Education (7* 8), Health Promotion and Education (7* 5 50) and Information & Referral (7* 5 70).

  rlog::log_info("Generating S452 60 00 and S452 65 00 statistics")

  calc_452_60_00 <- mis_unregistered_interactions |>
    filter_eligible("452") |>
    dplyr::filter(default_sr_category(funder_service_code) == "80") |>
    dplyr::filter(activity_individual_group == "Individual") |>
    add_statistical_account(statistic = "452",
                            sr_category = dplyr::if_else(funder_service_code %in% info_referral_fcs & ir_version == "verCMHA", "65", "60"),
                            detail = "00") |>
    dplyr::rename(date = activity_date)

  report_452_60_00 <- function(grain) {
    calc_452_60_00 |>
      detail_by_grain(grain, value = na_to_zero(activity_count), reference = activity_id)
  }


  calc_452_65_00 <- mis_unregistered_interactions |>
    filter_eligible("452") |>
    dplyr::filter(default_sr_category(funder_service_code) != "80") |>
    dplyr::filter(activity_individual_group == "Individual") |>
    add_statistical_account(statistic = "452",
                            sr_category = "65",
                            detail = "00") |>
    dplyr::rename(date = activity_date)

  report_452_65_00 <- function(grain) {
    calc_452_65_00 |>
      detail_by_grain(grain, value = na_to_zero(activity_count), reference = activity_id)
  }


  # S454 ** ** Hours of Care / Hours of Service – In House ----
  # •	4th, 5th digits = SR category and 6th digit = Age Group.
  # •	Report the exact hours of direct service provided to SRs.
  # •	Use S453* and S454* to report direct care or service provided by third party service providers and in house employees respectively.
  # •	Hours of direct care/service excludes travel and documentation time.
  # •	Report the hours of service received by the SRs under this account rather than the hours paid by the organization to provide service to the SRs.

  rlog::log_info("Generating S454 statistic")

  calc_454 <- mis_visits |>
    filter_eligible("454") |>
    # remove indirect and clinical entries
    dplyr::filter(stringr::str_detect(activity_type, "ace-to-face")) |>
    # 6th digit = age group; 7th digit = 0 in person, 1 virtual (OHRS Appendix C: 454 ** 21/41/61).
    # There is no virtual account for age not known, so 90 stays 90.
    add_statistical_account(statistic = "454",
                            sr_category = default_sr_category(funder_service_code),
                            detail = dplyr::if_else(activity_type == "Face-to-face Virtual" & participant_funder_age_group_code != "90",
                                                    paste0(age_digit(participant_funder_age_group_code), "1"),
                                                    as.character(participant_funder_age_group_code))) |>
    dplyr::rename(date = activity_date) |>

    # group sessions: hours are the session length, not length x participants
    # (OHRS Ch. 10, 10.9.4.3.3 S454 example: two 3-hour sessions for 10 SRs = 6 hours).
    # Each participant row carries an equal share of the session, so mixed-age groups
    # still total the session length across age categories.
    dplyr::group_by(date, funder_service_code, activity_group_id) |>
    dplyr::mutate(hours = activity_duration / 60 *
                    dplyr::if_else(activity_individual_group == "Group" & !is.na(activity_group_id), 1 / dplyr::n(), 1)) |>
    dplyr::ungroup()

  report_454 <- function(grain) {
    calc_454 |>
      # group sessions reference the group, so all rows from one session share it
      detail_by_grain(grain, value = na_to_zero(hours),
                      reference = dplyr::if_else(activity_individual_group == "Group" & !is.na(activity_group_id),
                                                 as.character(activity_group_id), as.character(activity_id)))
  }

  # S483 ** ** Attendance Days – Face to Face ----
  # S483 ** *1   Attendance Days – Face to Face in Person
  # S483 ** *3  Attendance Days – Face to Face Virtual
  # where
  # 4th and 5th digit represent SR
  # 6th digit represents SR age category
  # 7th digit represents mode of delivery (in person or virtual)
  # The number of service delivery days (count once per 24-hour calendar day) which primary service recipient activities are provided face-to-face, or by videoconference, on an individual or group basis to a service recipient and /or significant other(s) provided by facility staff.  These services are documented according to the health care organization's policy and are provided for longer than five minutes.  This statistic counts days during which services occurred.
  # Only one attendance day is reported per service recipient per functional centre per day.
  # Note: group participant attendances and group sessions are not reported for the services provided on a group basis.

  # S484 ** *0 Attendance Days – Non-Face-to-Face ----
  # The number of calendar days (count once per 24-hour calendar day) which primary service recipient activities are provided by means other than by face-to-face to a service recipient and /or significant other(s) provided by facility therapy staff.  These calendar days take the place of an attendance day face-to-face.  Examples may include attendance days via telephone, email or other forms of electronic communication. These services are documented according to the health service organization's policy and are provided for longer than five minutes. This statistic counts days during which services occurred rather than number of services.
  # Note:  If services are provided face-to-face as well by non face-to-face on the same calendar day, only an Attendance Day Face-to-Face is recorded regardless of which occurred first.


  # In residential FCs (SR 45), residents are reported only as Resident Days (S403).
  # Attendance days there are only for clients before admission or after discharge,
  # and are reported under SR 25 (OHRS Ch 7 s7.7.3.1), so S483 45 / S484 45 never exist.

  rlog::log_info("Generating S483 and S484 statistics")

  resident_days <- mis_daily_census |>
    dplyr::filter(service_status == "Active", default_sr_category(funder_service_code) == "45") |>
    dplyr::distinct(participant_id, funder_service_code, date)

  calc_483_484 <- mis_visits |>
    filter_eligible("483|484") |>
    # clients must be uniquely identified
    dplyr::filter(!is.na(participant_id)) |>
    # drop days already counted as resident days
    dplyr::anti_join(resident_days, by = c("participant_id", "funder_service_code", "activity_date" = "date")) |>
    # remove indirect and clinical entries
    dplyr::filter(stringr::str_detect(activity_type, "ace-to-face")) |>

    # count only one attendance day per day; preferentially choose FTF over NFTF by sorting by activity_type
    dplyr::mutate(activity_type_priority = dplyr::case_when(activity_type == "Face-to-face" ~ 1,
                                              activity_type == "Face-to-face Virtual" ~ 2,
                                              .default = 3)) |>

    # assemble statistical account
    add_statistical_account(statistic = dplyr::case_when(activity_type == "Non-face-to-face" ~ "484",
                                                         activity_type %in% c("Face-to-face", "Face-to-face Virtual") ~ "483"),
                            # pre-admission / post-discharge clients in residential FCs are SR 25
                            sr_category = dplyr::if_else(default_sr_category(funder_service_code) == "45", "25", default_sr_category(funder_service_code)),
                            detail = paste0(age_digit(participant_funder_age_group_code),
                                            dplyr::case_when(activity_type == "Face-to-face" ~ "1",
                                                             activity_type == "Face-to-face Virtual" ~ "3",
                                                             activity_type == "Non-face-to-face" ~ "0",
                                                             activity_individual_group == "Group" ~ "1",
                                                             .default = "0"))) |>
    dplyr::rename(date = activity_date)


  report_483_484 <- function(grain) {
    calc_483_484 |>
      one_per_person_per_day(grain) |>
      detail_by_grain(grain, reference = activity_id)
  }


  # S489 ** *0 New Referral ----
  # (Internal Referral to the Functional Centre)
  # The number of service recipients accepted to receive service in a functional centre during the reporting period.  Includes all SRs who are eligible to receive service regardless of whether they had to wait or not for service to commence. There may be multiple referrals to various service functional centres as one SR may be referred to a number of functional centres. If a client has received service and later discharged from a functional centre and then re-admitted again to the same FC within the same reporting period, both referrals for the same client can be reported.
  # This statistic is a cumulative value for the reporting period and has the service recipient and age category.  Reporting is valid with age category – unknown, S489** 90.

  # One referral per FC episode, on the date it was accepted (see fc_episodes()), so each
  # S489 pairs with at most one S513


  rlog::log_info("Generating S489 statistic")

  calc_489 <- mis_service_history |>
    filter_eligible("489")

  report_489 <- function(grain) {
    fc_episodes(calc_489, grain, report_date) |>
      # a referral needs an eligibility decision date (see number_episodes())
      dplyr::filter(!is.na(episode_decision_date)) |>
      add_statistical_account(statistic = "489",
                              sr_category = default_sr_category(funder_service_code),
                              detail = participant_funder_age_group_code) |>
      dplyr::rename(date = episode_decision_date) |>
      detail_by_grain(grain, reference = enrollment_ids)
  }



  # S491 65 10 Group Participants, Not Uniquely Identified Service Recipient Attendance ----
  #This is reported when a not uniquely identified service recipient receives services in a group session.
  #Individuals are not associated with an Admission or Referral and no health record/documentation has been initiated.
  # SR category = 65.
  #Note: Not Uniquely Identified Service Recipients participating in group sessions are not reported in S455 ** **, Individuals served by FC, nor in S855 ** 76, Individuals served by Organization.


  rlog::log_info("Generating S491 60 10 and S491 65 10 statistics")

  calc_491_60_10 <- mis_unregistered_interactions |>
    filter_eligible("491") |>
    dplyr::filter(default_sr_category(funder_service_code) == "80") |>
    dplyr::filter(activity_individual_group == "Group") |>
    add_statistical_account(statistic = "491",
                            sr_category = dplyr::if_else(funder_service_code %in% info_referral_fcs & ir_version == "verCMHA", "65", "60"),
                            detail = "10") |>
    dplyr::rename(date = activity_date)

  report_491_60_10 <- function(grain) {
    calc_491_60_10 |>
      detail_by_grain(grain, value = na_to_zero(activity_count), reference = group_reference(activity_group_id, activity_id))
  }


  calc_491_65_10 <- mis_unregistered_interactions |>
    filter_eligible("491") |>
    dplyr::filter(default_sr_category(funder_service_code) != "80") |>
    dplyr::filter(activity_individual_group == "Group") |>
    add_statistical_account(statistic = "491",
                            sr_category = "65",
                            detail = "10") |>
    dplyr::rename(date = activity_date)

  report_491_65_10 <- function(grain) {
    calc_491_65_10 |>
      detail_by_grain(grain, value = na_to_zero(activity_count), reference = group_reference(activity_group_id, activity_id))
  }



  # S491 ** 20 Group Participants, Uniquely Identified Service Recipient Attendance ----
  #This is reported when a uniquely identified service recipient receives services in a group session.
  #The service is documented according to the health care organization’s policy.


  rlog::log_info("Generating S491 statistic")

  calc_491 <- mis_visits |>
    filter_eligible("491") |>
    dplyr::filter(activity_individual_group == "Group") |>
    add_statistical_account(statistic = "491",
                            sr_category = default_sr_category(funder_service_code),
                            detail = "20") |>
    dplyr::rename(date = activity_date)

  report_491 <- function(grain) {
    detail_by_grain(calc_491, grain, reference = group_reference(activity_group_id, activity_id))
  }


  # S492 00 *0 Group Sessions in Person (Number of Group Sessions) ----
  # The number of group sessions providing formal services activities that are material in length and are planned and delivered to two or more service recipients at the same time. One group session is reported by functional centre regardless of number of service providers. The individuals in the group can be uniquely identified registered and/or not uniquely identified.
  # Note:  Statistical accounts codes are available for group sessions that are of different duration.
  # 6th digit represents time intervals:
  # 1 - Time Intervals Not Reported
  # 2 – Up to 1 Hour
  # 3 – Between 1 to 2 Hours
  # 4 – Between 2 to 5 Hours
  # 5 – More than 5 Hours
  #

  # S 492 00 11 Group Sessions - Virtual - Time Intervals Not Reported ----
  # The number of group sessions providing formal service activities that are material in length,
  # and are planned and delivered to two or more service recipients, at the same time and in real-time through video encounter using communications or information technology, with service provider(s) and service recipient(s) in different physical locations.
  # One group session is reported by functional centre regardless of the number of service providers.
  # The individuals in the group may be uniquely identified and/or not uniquely identified.
  # The number of Group Participants, Uniquely Identified Service Recipient Attendance (S491 ** 20) and/or Group Participants, Not- Uniquely Identified Service Recipient Attendance (S491 65 10) also need to be reported.
  # Note: The total of Uniquely Identified and Not- Uniquely Identified group participant attendances divided by the number of group sessions would be the average size of the group sessions.


  rlog::log_info("Generating S492 statistic")

  calc_492 <- mis_visits |>
    dplyr::bind_rows(mis_unregistered_interactions) |>
    filter_eligible("492") |>
    dplyr::filter(activity_individual_group == "Group") |>
    dplyr::mutate(funder_statistical_account_code =
             dplyr::case_when(
               activity_type == "Face-to-face Virtual"             ~ "492 00 11",  # virtual, any duration
               activity_duration <= 60                             ~ "492 00 20",  # < 1 hr
               activity_duration > 60 & activity_duration <= 120   ~ "492 00 30",  # 1-2 hrs
               activity_duration > 120 & activity_duration <= 300  ~ "492 00 40",  # 2-5 hrs
               activity_duration > 300                             ~ "492 00 50",  # 5+ hrs
               .default =                                            "492 00 10"   # time interval not reported
             ))

  report_492 <- function(grain) {
    calc_492 |>
      dplyr::rename(date = activity_date) |>
      dplyr::distinct(dplyr::pick(dplyr::all_of(c("date", "activity_group_id", grain_cols(grain), "funder_statistical_account_code")))) |>
      detail_by_grain(grain, reference = activity_group_id)
  }


  # S501 ** *0 Admissions to Community Services (to the organization) ----
  # The number of individuals that are accepted for community services.  This is recorded each time an individual is accepted for service.
  # Individuals are counted each time a new file is created upon their admission to the agency. Service Recipient category is required. The decision date of eligibility is the admit date.
  # - May be counted more than once per year if a file has been closed and opened in the same year.
  # -	Reporting is valid with age category – unknown, S501** 90
  # -	Will always be equal to or greater than the S855* account value
  # Every Admission must have a File Closed (S511*) recorded when services end.  These are reported in the accounting centre 8 2 9 90.


  rlog::log_info("Generating S501 statistic")

  # Periods when a client is on the organization's books, shared by S501 (admission) and S511 (file closed)
  # so that every admission pairs with a file closure.
  # - starts at the episode's acceptance date (OHRS: "The decision date of eligibility is the admit date")
  # - covers On Waitlist, On Hold and Active; still-open enrollments run to tomorrow (prefilled end date)
  # - each episode's discharge reason (from its Inactive row) is used for S511
  # This is fc_episodes() merged across all FCs with the same SR code.
  org_involvement <- fc_episodes(mis_service_history, "organization", report_date)

  calc_org_501 <- org_involvement |>
    dplyr::rename(date = merged_start_date) |>

    # age group at the latest admission in the fiscal year
    min_age_group_by_fiscal_year(by = "sr_code") |>

    # add organizational functional centre
    dplyr::mutate(funder_service_code = "82 9 90") |>

    # assemble funder_statistical_account_code
    add_statistical_account(statistic = "501",
                            sr_category = sr_code,
                            detail = participant_funder_age_group_code) |>
    detail_by_grain("fc", reference = enrollment_ids)


  # S506 ** *0 Individuals Received First Service ----
  # The number of registered service recipients (SR) who have had their assessment/intake and received their first service in the functional centre (FC) during current fiscal year.  This count includes the SRs who received their first service with or without waiting.  If a client has received service and later discharged from the functional centre and then readmitted to the same FC within the same reporting period, another first service count is reported.
  # This is a cumulative number, year-to-date count.
  # Note: The days individuals have waited are reported in S407**20 Days Waited for Service Initiation only after the individual received the first service in the functional centre.


  rlog::log_info("Generating S506 statistic")

  if(fc_506_version == "verA"){

    ## Service history-based calculation: the day service first started in each episode
    ## (see fc_episodes()), so service that pauses and resumes is counted once
    calc_506 <- mis_service_history |>
      filter_eligible("506")

    # age group on the day service started, from date of birth (as in the daily census)
    age_on_day <- mis_daily_census |>
      dplyr::distinct(participant_id, date, age_group_on_day = participant_funder_age_group_code) |>
      dplyr::distinct(participant_id, date, .keep_all = TRUE)

    report_506 <- function(grain) {
      fc_episodes(calc_506, grain, report_date) |>
        dplyr::filter(!is.na(first_active_date), first_active_date >= generateMIS::dashboard_start_date) |>
        dplyr::left_join(age_on_day, by = c("participant_id", "first_active_date" = "date")) |>
        dplyr::mutate(participant_funder_age_group_code = dplyr::coalesce(age_group_on_day, participant_funder_age_group_code)) |>
        add_statistical_account(statistic = "506",
                                sr_category = default_sr_category(funder_service_code),
                                detail = participant_funder_age_group_code) |>
        dplyr::rename(date = first_active_date) |>
        detail_by_grain(grain, reference = enrollment_ids, note = dated_note("Accepted", merged_start_date))
    }
  }

  if(fc_506_version == "verB"){
    ## Visit-based calculation
    calc_506 <- mis_service_history |>
      filter_eligible("506") |>
      dplyr::filter(service_status == "Active" & !is.na(service_first_direct_activity))

    report_506 <- function(grain) {
      calc_506 |>
        merge_overlaps(service_status_start_date, service_status_end_date, report_date, merge_level = grain) |>
        dplyr::distinct(dplyr::pick(dplyr::all_of(c("participant_id", "participant_funder_age_group_code", "service_first_direct_activity", grain_cols(grain)))),
                        .keep_all = TRUE) |>
        add_statistical_account(statistic = "506",
                                sr_category = default_sr_category(funder_service_code),
                                detail = participant_funder_age_group_code) |>
        dplyr::rename(date = service_first_direct_activity) |>
        detail_by_grain(grain, reference = enrollment_ids, note = dated_note("Admitted", merged_start_date))
    }
  }


  # S511 ** 90 File Closed (from the organization) ----
  # This account indicates the number of clients/service recipients who are no longer the responsibility of the healthcare organization. A file is closed when the need for services(s) has ended, or the client no longer qualifies for services from the healthcare organization.
  # The client has been discharged from all service(s) that they had been receiving from the agency.  This is reported in the accounting centre 82990.  File must be closed at time of death.


  rlog::log_info("Generating S511 statistic")

  # A file is closed only when every enrollment in an org_involvement period has ended
  # (period end on or before today; open enrollments run to tomorrow). Dated at the last
  # discharge, with the reason from the enrollment that was discharged last.
  calc_511 <- org_involvement |>
    dplyr::filter(merged_end_date <= report_date) |>
    dplyr::mutate(service_status_reason_code = stringr::str_sub(last_service_status_reason, 1, 2),
                  funder_service_code = "82 9 90") |>
    add_statistical_account(statistic = "511",
                            sr_category = sr_code,
                            # CSS reports the discharge reason; CMH&A reports 90 (OHRS Ch. 7: S511 ** 90)
                            detail = dplyr::case_when(sr_code == "80" & stringr::str_detect(service_status_reason_code, "^\\d\\d$") ~ service_status_reason_code,
                                                      .default = "90")) |>
    dplyr::rename(date = merged_end_date) |>
    detail_by_grain("fc", reference = enrollment_ids, note = dated_note("Accepted", merged_start_date))


  # S512 ** ** Assessment Complete/Outcome (4th, 5th digits = SR category)
  # -	Number of assessments that have been completed, along with the outcome of that assessment.  The count of assessment occurs once the eligibility of the client has been determined.  Where applicable, the assessment will be used to develop the care plan for the client.
  # -	Cumulative year-to-date count of the number of assessments, i.e. both the completed initial and update assessments of the same SR can be included in the report period.  Basic demographic information is not an assessment for clients.
  # -	Mandatory reporting for the CSS sector.
  # -	This may include RAI-CHA, RAI-PS and other formal assessments (initial or update) which are used to develop care plan.
  # -	Only one S512* is reported for an assessment completed for a SR receiving multiple services in either the case management or a service delivery FC. When an assessment is completed and the client is deemed eligible but declines to receive service, report S512**11 (eligible in-home services – client declined).
  #
  # Report S512* in the Case Management FC
  # -	When a centralized or formal admission/intake process is used
  # -	Relevant S512* can be reported for a SR assessed as requiring only a specific service (e.g. S5128022 Assessment Complete/Outcome - Supportive Housing for SRs that will receive assisted living services)
  # -	S512**12 (eligible in-home services – admitted) is used when a client is assessed as requiring multi service
  #
  # Report in a 72 5* service delivery FC
  # -		The account can be reported in the FC where the assessment is completed
  # -		S512**12  (eligible in-home services – admitted) is usually reported for a completed assessment for a client who is eligible for one or more service(s).
  # The completion of a client assessment cannot be reported as CSS SR activity (e.g. visit) in a 7 25* service delivery FC except for the Case Management FC; where the client assessment is part of the mandate of the case management FC.  A client activity statistic is only reported when service is provided to or received by a SR and is within the scope of the individual FC’s service mandate/definition.

  rlog::log_info("Generating S512 statistic")

  report_512 <- function(grain) {
    mis_visits |>
      filter_eligible("512") |>
      dplyr::filter(stringr::str_detect(activity_assessment, "Assessment Complete/Outcome")) |>
      add_statistical_account(statistic = "512",
                              sr_category = default_sr_category(funder_service_code),
                              detail = dplyr::case_when(stringr::str_detect(activity_assessment, "Assessment Complete/Outcome – Eligible In-Home – Client Declined") ~ "11",
                                                        stringr::str_detect(activity_assessment, "Assessment Complete/Outcome – Eligible In-Home – Admitted") ~ "12",
                                                        stringr::str_detect(activity_assessment, "Assessment Complete/Outcome – Eligible – Adult Day") ~ "21",
                                                        stringr::str_detect(activity_assessment, "Assessment Complete/Outcome – Eligible – Supportive Housing") ~ "22",
                                                        stringr::str_detect(activity_assessment, "Assessment Complete/Outcome – Eligible – Enhanced Respite") ~ "23",
                                                        stringr::str_detect(activity_assessment, "Assessment Complete/Outcome – Eligible In-Home – Other Organizations") ~ "29",
                                                        .default = "12")) |>
      dplyr::rename(date = activity_date) |>
      detail_by_grain(grain, reference = activity_id)
  }


  # Detail rows: one per counted unit (an episode, a visit, a session), for every statistic.
  # The summary tables are their sums (summarise_detail()), so summary and detail always
  # agree. S501 and S511 are organization-level (AC 82 9 90), so they are in the FC tables only.
  detail_fc <- dplyr::bind_rows(report_248("fc"), report_265("fc"), report_266("fc"), report_401("fc"),
                                report_407("fc"), report_407_99("fc"), report_410_513_cmha("fc"), report_410_513_css("fc"),
                                report_450_451_cmha("fc"), report_450_451_css("fc"), report_452_60_00("fc"), report_452_65_00("fc"),
                                report_454("fc"), report_483_484("fc"), report_489("fc"), report_491_60_10("fc"),
                                report_491_65_10("fc"), report_491("fc"), report_492("fc"), calc_org_501,
                                report_506("fc"), calc_511, report_512("fc"))

  detail_service <- dplyr::bind_rows(report_248("service"), report_265("service"), report_266("service"),
                                     report_401("service"), report_407("service"), report_407_99("service"),
                                     report_410_513_cmha("service"), report_410_513_css("service"),
                                     report_450_451_cmha("service"), report_450_451_css("service"),
                                     report_452_60_00("service"), report_452_65_00("service"), report_454("service"),
                                     report_483_484("service"), report_489("service"), report_491_60_10("service"),
                                     report_491_65_10("service"), report_491("service"), report_492("service"),
                                     report_506("service"), report_512("service"))


  # Write mis_visit_and_service_counts_by_fc to file ----

  rlog::log_info("Writing mis_visit_and_service_counts_by_fc.csv")

  mis_visit_and_service_counts_by_fc <- summarise_detail(detail_fc, "fc") |>
    add_funder_key("fc")

  check_accounts(mis_visit_and_service_counts_by_fc, "mis_visit_and_service_counts_by_fc")
  readr::write_csv(mis_visit_and_service_counts_by_fc, paste0(data_folder, "/processed/mis_visit_and_service_counts_by_fc.csv"), na = "")

  rlog::log_info("Writing mis_visit_and_service_counts_by_service.csv")

  mis_visit_and_service_counts_by_service <- summarise_detail(detail_service, "service") |>
    add_funder_key("service")

  readr::write_csv(mis_visit_and_service_counts_by_service, paste0(data_folder, "/processed/mis_visit_and_service_counts_by_service.csv"), na = "")

  invisible(list(mis_visit_and_service_counts_by_fc = mis_visit_and_service_counts_by_fc,
                 mis_visit_and_service_counts_by_service = mis_visit_and_service_counts_by_service,
                 detail_by_fc = add_aggregation(detail_fc) |> add_funder_key("fc"),
                 detail_by_service = add_aggregation(detail_service) |> add_funder_key("service")))
}
