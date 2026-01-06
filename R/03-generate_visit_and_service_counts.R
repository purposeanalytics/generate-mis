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
                                              fc_506_version = "verA"){

  data_folder <- processed_data$data_folder
  mis_service_history <- processed_data$mis_service_history
  mis_visits <- processed_data$mis_visits
  mis_unregistered_interactions <- processed_data$mis_unregistered_interactions
  mis_daily_census <- processed_data$mis_daily_census

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
    dplyr::mutate(digits_1_3 = "248",
                  digits_4_5 = "80",
                  digits_6_7 = dplyr::case_when(stringr::str_detect(service_sub_service_name, "Hot") ~ "12",
                                  stringr::str_detect(service_sub_service_name, "Cold") ~ "12",
                                  stringr::str_detect(service_sub_service_name, "Frozen") ~ "14",
                                  stringr::str_detect(service_sub_service_name, "Soup") |
                                    stringr::str_detect(service_sub_service_name, "Dessert") |
                                    stringr::str_detect(service_sub_service_name, "Side") ~ "16")) |>
    assemble_statistical_account() |>
    dplyr::rename(date = activity_date)

  calc_fc_248 <- calc_248 |>
    dplyr::group_by(date, funder_service_code, funder_statistical_account_code) |>
    dplyr::summarize(value = sum(activity_count, na.rm = TRUE))

  calc_service_248 <- calc_248 |>
    dplyr::group_by(date, funder_service_code, service_name, funder_statistical_account_code) |>
    dplyr::summarize(value = sum(activity_count, na.rm = TRUE))


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

  if(fc_450_version == "verA"){

    # interactions <= 5 min will not be counted
    calc_265 <- mis_visits |>
      filter_eligible("265") |>
      # individual interactions only
      dplyr::filter(activity_individual_group == "Individual") |>
      # remove indirect and clinical entries
      dplyr::filter(stringr::str_detect(activity_type, "ace-to-face")) |>
      dplyr::mutate(digits_1_3 = "265",
                    digits_4_5 = get_sr_code(funder_service_code),
                    digits_6_7 = get_time_intervals(activity_duration)) |>
      assemble_statistical_account() |>
      dplyr::rename(date = activity_date)

  }

  if(fc_450_version == "verB"){

    # set interactions under 5 min to 0 min so that they are counted as "time not reported"
    calc_265 <- mis_visits |>
      filter_eligible("265") |>
      # individual interactions only
      dplyr::filter(activity_individual_group == "Individual") |>
      # remove indirect and clinical entries
      dplyr::filter(stringr::str_detect(activity_type, "ace-to-face")) |>
      dplyr::mutate(activity_duration == dplyr::if_else(activity_duration <= 5, 0, activity_duration)) |>
      dplyr::mutate(digits_1_3 = "265",
                    digits_4_5 = get_sr_code(funder_service_code),
                    digits_6_7 = get_time_intervals(activity_duration)) |>
      assemble_statistical_account() |>
      dplyr::rename(date = activity_date)

  }

  calc_fc_265 <- calc_265 |>
    dplyr::group_by(date, funder_service_code, funder_statistical_account_code) |>
    dplyr::summarize(value = sum(activity_count, na.rm = TRUE))

  calc_service_265 <- calc_265 |>
    dplyr::group_by(date, funder_service_code, service_name, funder_statistical_account_code) |>
    dplyr::summarize(value = sum(activity_count, na.rm = TRUE))


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
    dplyr::mutate(digits_1_3 = "266",
                  digits_4_5 = "00",
                  digits_6_7 = get_time_intervals(activity_duration)) |>
    assemble_statistical_account() |>
    dplyr::rename(date = activity_date)

  calc_fc_266 <- calc_266 |>
    dplyr::distinct(date, funder_service_code, activity_group_id, funder_statistical_account_code) |>
    dplyr::count(date, funder_service_code, funder_statistical_account_code, name = "value")

  calc_service_266 <- calc_266 |>
    dplyr::distinct(date, funder_service_code, service_name, activity_group_id, funder_statistical_account_code) |>
    dplyr::count(date, funder_service_code, service_name, funder_statistical_account_code, name = "value")


  # S401 4* *0 Resident Admissions (to the Functional Centre) ----
  # An individual is counted each time they are admitted to a bed in a CMH&A 7*5 40* residential service functional centre. Age category breakdown and service recipient category are required. The service recipient (SR) category is 45 or 44 (as noted in the 4th and 5th digits of the account number above).
  # Note:  S401 4* *0 Resident Admissions must be reported for all residential services functional centres, 7* 5 40 ** **, except the Supportive Housing functional centres (refer to section 7.6.8 for the list of Supportive Housing functional centres).  That is, if your organization is responsible for a residential treatment facility where clients reside for a period of time to receive treatment, this statistic must be reported each time a client arrives at your facility.
  # There is an optional edit rule for the trial balance submission to ensure this statistic is reported.  It is possible that for a reporting period, such as the Q2 trial balance submission, that there are no new admissions to report which is the reason for the trial balance edit rule being optional.  However, if any new admissions occur within a reporting period this statistic must be reported.

  rlog::log_info("Generating S401 statistic")

  calc_401 <- mis_service_history |>
    filter_eligible("401") |>
    dplyr::filter(service_status == "Active")

  calc_fc_401 <- calc_401 |>
    merge_overlaps(service_status_start_date, service_status_end_date, lubridate::today(), merge_level = "funder_service_code") |>
    dplyr::mutate(digits_1_3 = "401",
                  digits_4_5 = get_sr_code(funder_service_code),
                  digits_6_7 = participant_funder_age_group_code) |>
    assemble_statistical_account() |>
    dplyr::rename(date = merged_start_date)  |>
    dplyr::count(date, funder_service_code, funder_statistical_account_code, name = "value")

  calc_service_401 <- calc_401 |>
    merge_overlaps(service_status_start_date, service_status_end_date, lubridate::today(), merge_level = "service_name") |>
    dplyr::mutate(digits_1_3 = "401",
                  digits_4_5 = get_sr_code(funder_service_code),
                  digits_6_7 = participant_funder_age_group_code) |>
    assemble_statistical_account() |>
    dplyr::rename(date = merged_start_date)  |>
    dplyr::count(date, funder_service_code, service_name, funder_statistical_account_code, name = "value")


  # S407 ** 20 Days Waited for Service Initiation ----
  # The number of days waited from the accepted for service date to service initiation date (date of actual “first” service).
  # These days are reported after the service has started and the client is no longer waiting
  # This statistic is recorded from the date that the client is deemed eligible for service and not from the date the service is dplyr::arranged.
  # This statistic is a cumulative figure number, year-to-date value and is recorded in the service delivery functional centre.  Service Recipient Category required.
  # The S407**20 does not include days waited when the service is available to the service recipient but the client requests to delay initiation of service.

  #RULES - only counted if at least one service received, and from date of first service
  # figure out if they were waitlisted (Status == "On Waitlist") and if service has since started !is.na(service_status_end_date)

  rlog::log_info("Generating S407 statistic")

  calc_407 <- mis_service_history |>
    # filter_eligible("407") |>
    dplyr::filter(service_status == "On Waitlist" & !is.na(service_status_end_date))

  calc_fc_407 <- calc_407 |>
    earliest_overlaps(service_status_start_date, service_status_end_date, lubridate::today(), merge_level = "funder_service_code") |>
    dplyr::mutate(days_waited = lubridate::interval(merged_start_date, merged_end_date)/lubridate::days(1)) |>
    dplyr::mutate(digits_1_3 = "407",
                  digits_4_5 = get_sr_code(funder_service_code),
                  digits_6_7 = "20") |>
    assemble_statistical_account() |>
    dplyr::rename(date = merged_end_date) |>
    dplyr::group_by(date, funder_service_code, funder_statistical_account_code) |>
    dplyr::summarize(value = sum(days_waited, na.rm = TRUE))

  calc_service_407 <- calc_407 |>
    earliest_overlaps(service_status_start_date, service_status_end_date, lubridate::today(), merge_level = "service_name") |>
    dplyr::mutate(days_waited = lubridate::interval(merged_start_date, merged_end_date)/lubridate::days(1)) |>
    dplyr::mutate(digits_1_3 = "407",
                  digits_4_5 = get_sr_code(funder_service_code),
                  digits_6_7 = "20") |>
    assemble_statistical_account() |>
    dplyr::rename(date = merged_end_date) |>
    dplyr::group_by(date, funder_service_code, service_name, funder_statistical_account_code) |>
    dplyr::summarize(value = sum(days_waited, na.rm = TRUE))


  # S407 99 10 Days Waited for Assessment ----
  # The number of days a client waited from the date of application/referral to the assessment complete date by the organization.
  # This statistic is a cumulative figure and can only be recorded after the initial assessment for the client has been completed.
  # This statistic is used to produce the average wait time for client assessments (S4079910 divided by S489** *0).  If the client is not accepted for service, no days waited would be included.


  rlog::log_info("Generating S407 99 statistic")

  calc_407_99 <- mis_service_history |>
    filter_eligible("407 99") |>
    dplyr::filter(service_status == "Waiting for Assessment" & !is.na(service_status_end_date))

  calc_fc_407_99 <- calc_407_99 |>
    earliest_overlaps(service_status_start_date, service_status_end_date, lubridate::today(), merge_level = "funder_service_code") |>
    dplyr::mutate(days_waited = lubridate::interval(merged_start_date, merged_end_date)/lubridate::days(1)) |>
    dplyr::mutate(digits_1_3 = "407",
                  digits_4_5 = get_sr_code(funder_service_code),
                  digits_6_7 = "20") |>
    assemble_statistical_account() |>
    dplyr::rename(date = merged_end_date) |>
    dplyr::group_by(date, funder_service_code, funder_statistical_account_code) |>
    dplyr::summarize(value = sum(days_waited, na.rm = TRUE))

  calc_service_407_99 <- calc_407_99 |>
    earliest_overlaps(service_status_start_date, service_status_end_date, lubridate::today(), merge_level = "service_name") |>
    dplyr::mutate(days_waited = lubridate::interval(merged_start_date, merged_end_date)/lubridate::days(1)) |>
    dplyr::mutate(digits_1_3 = "407",
                  digits_4_5 = get_sr_code(funder_service_code),
                  digits_6_7 = "20") |>
    assemble_statistical_account() |>
    dplyr::rename(date = merged_end_date) |>
    dplyr::group_by(date, funder_service_code, service_name, funder_statistical_account_code) |>
    dplyr::summarize(value = sum(days_waited, na.rm = TRUE))


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


  rlog::log_info("Generating S410 & S513 statistic")

  ## CSS version with discharge reason
  calc_410_513_css <- mis_service_history |>
    filter_eligible("410|513") |>
    dplyr::filter(service_status == "Inactive" & get_sr_code(funder_service_code) == "80")

  calc_fc_410_513_css <- calc_410_513_css |>
    intersect_overlaps(service_decision_date, service_status_start_date, lubridate::today(), merge_level = "funder_service_code") |>

    # get first status reason
    dplyr::mutate(service_status_reason_code = stringr::str_sub(all_service_status_reasons, 1, 2),
           service_status_reason_code = dplyr::if_else(stringr::str_detect(service_status_reason_code, "\\d\\d"), service_status_reason_code, "90")) |>
    dplyr::mutate(digits_1_3 = dplyr::if_else(funder_service_code == "72 5 82 45", "410", "513"),
                  digits_4_5 = get_sr_code(funder_service_code),
                  digits_6_7 = dplyr::if_else(funder_service_code == "72 5 82 45", participant_funder_age_group_code, service_status_reason_code)) |>
    assemble_statistical_account() |>
    dplyr::rename(date = merged_end_date) |>
    dplyr::count(date, funder_service_code, funder_statistical_account_code, name = "value")


  calc_service_410_513_css <- calc_410_513_css |>
    intersect_overlaps(service_decision_date, service_status_start_date, lubridate::today(), merge_level = "service_name") |>

    # get first status reason
    dplyr::mutate(service_status_reason_code = stringr::str_sub(all_service_status_reasons, 1, 2),
           service_status_reason_code = dplyr::if_else(stringr::str_detect(service_status_reason_code, "\\d\\d"), service_status_reason_code, "90")) |>
    dplyr::mutate(digits_1_3 = dplyr::if_else(funder_service_code == "72 5 82 45", "410", "513"),
                  digits_4_5 = get_sr_code(funder_service_code),
                  digits_6_7 = dplyr::if_else(funder_service_code == "72 5 82 45", participant_funder_age_group_code, service_status_reason_code)) |>
    assemble_statistical_account() |>
    dplyr::rename(date = merged_end_date) |>
    dplyr::count(date, funder_service_code, service_name, funder_statistical_account_code, name = "value")


  ## CMHA version without discharge reason
  calc_410_513_cmha <- mis_service_history |>
    filter_eligible("410|513") |>
    dplyr::filter(service_status == "Inactive", get_sr_code(funder_service_code) != "80")

  calc_fc_410_513_cmha <- calc_410_513_cmha |>
    intersect_overlaps(service_decision_date, service_status_start_date, lubridate::today(), merge_level = "funder_service_code") |>
    dplyr::mutate(digits_1_3 = dplyr::if_else(get_sr_code(funder_service_code) == "45", "410", "513"),
                  digits_4_5 = get_sr_code(funder_service_code),
                  digits_6_7 = dplyr::if_else(get_sr_code(funder_service_code) == "45", participant_funder_age_group_code, "90")) |>
    assemble_statistical_account() |>
    dplyr::rename(date = merged_end_date) |>
    dplyr::count(date, funder_service_code, funder_statistical_account_code, name = "value")

  calc_service_410_513_cmha <- calc_410_513_cmha |>
    intersect_overlaps(service_decision_date, service_status_start_date, lubridate::today(), merge_level = "service_name") |>
    dplyr::mutate(digits_1_3 = dplyr::if_else(get_sr_code(funder_service_code) == "45", "410", "513"),
                  digits_4_5 = get_sr_code(funder_service_code),
                  digits_6_7 = dplyr::if_else(get_sr_code(funder_service_code) == "45", participant_funder_age_group_code, "90")) |>
    assemble_statistical_account() |>
    dplyr::rename(date = merged_end_date) |>
    dplyr::count(date, funder_service_code, service_name, funder_statistical_account_code, name = "value")


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


  rlog::log_info("Generating S450 & S451 statistic")


  if(fc_450_version == "verA"){

    # interactions must be > 5 min to count; include entries with no reported time interval and all CSS Transportation
    calc_450_451 <- mis_visits |>
      dplyr::filter(activity_duration > 5 | activity_duration == 0 | is.na(activity_duration) | funder_service_code == "72 5 82 14")

  }

  if(fc_450_version == "verB"){

    # all interactions regardless of duration (can be required for bulk notes where time is divided across clients)
    calc_450_451 <- mis_visits

  }

  calc_450_451_cmha <- calc_450_451 |>
    filter_eligible("450|451") |>
    # non-residential CMHA
    dplyr::filter(get_sr_code(funder_service_code) == "25") |>
    # individual interactions only
    dplyr::filter(activity_individual_group == "Individual") |>
    # remove indirect and clinical entries
    dplyr::filter(stringr::str_detect(activity_type, "ace-to-face")) |>

    # count only one visit per day; preferentially choose FTF over NFTF by sorting by activity_type
    dplyr::mutate(activity_type_priority = dplyr::case_when(activity_type == "Face-to-face" ~ 1,
                                              activity_type == "Face-to-face Virtual" ~ 2,
                                              .default = 3)) |>

    # assemble statistical account
    dplyr::mutate(digits_1_3 = dplyr::case_when(activity_type == "Non-face-to-face" ~ "451",
                                  activity_type %in% c("Face-to-face", "Face-to-face Virtual") ~ "450"),
                  digits_4_5 = get_sr_code(funder_service_code),
                  digits_6 = stringr::str_sub(participant_funder_age_group_code, 1, 1),
                  digits_7 = dplyr::if_else(activity_type == "Face-to-face Virtual", "1", "0"),
                  digits_6_7 = paste0(digits_6, digits_7)) |>
    assemble_statistical_account()

  calc_fc_450_451_cmha <- calc_450_451_cmha |>
    dplyr::group_by(funder_service_code, participant_id, activity_date) |>
    dplyr::arrange(funder_service_code, participant_id, activity_date, activity_type_priority) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::rename(date = activity_date) |>
    dplyr::count(date, funder_service_code, funder_statistical_account_code, name = "value")

  calc_service_450_451_cmha <- calc_450_451_cmha |>
    dplyr::group_by(funder_service_code, service_name, participant_id, activity_date) |>
    dplyr::arrange(funder_service_code, service_name, participant_id, activity_date, activity_type_priority) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::rename(date = activity_date) |>
    dplyr::count(date, funder_service_code, service_name, funder_statistical_account_code, name = "value")


  calc_450_451_css <- calc_450_451 |>
    filter_eligible("450|451") |>
    # clients must be uniquely identified
    dplyr::filter(!is.na(participant_id)) |>
    # non-residential CSS
    dplyr::filter(get_sr_code(funder_service_code) == "80") |>
    # individual interactions only
    dplyr::filter(activity_individual_group == "Individual") |>
    # remove indirect and clinical entries
    dplyr::filter(stringr::str_detect(activity_type, "ace-to-face")) |>

    # assemble statistical account
    dplyr::mutate(digits_1_3 = dplyr::case_when(activity_type == "Non-face-to-face" ~ "451",
                                  activity_type %in% c("Face-to-face", "Face-to-face Virtual") ~ "450"),
                  digits_4_5 = get_sr_code(funder_service_code),
                  digits_6 = stringr::str_sub(participant_funder_age_group_code, 1, 1),
                  digits_7 = dplyr::if_else(activity_type == "Face-to-face Virtual", "1", "0"),
                  digits_6_7 = paste0(digits_6, digits_7)) |>
    assemble_statistical_account() |>
    dplyr::rename(date = activity_date)

  calc_fc_450_451_css <- calc_450_451_css |>
    dplyr::group_by(date, funder_service_code, funder_statistical_account_code) |>
    dplyr::summarize(value = sum(activity_count, na.rm = TRUE))

  calc_service_450_451_css <- calc_450_451_css |>
    dplyr::group_by(date, funder_service_code, service_name, funder_statistical_account_code) |>
    dplyr::summarize(value = sum(activity_count, na.rm = TRUE))


  # S452 60 00 Not Uniquely Identified Service Recipient Interactions ----
  # The number of interactions face to face, or non - face to face with a service recipient/client or significant other who is not uniquely identified. This statistic is used when a health record or unique identifier has not been generated and/or there is no documentation according to the health care organization’s policy.  Only service recipient codes 60 and 65 can be used with this account.  Examples of functional centres where this account can be used are Education (7* 8), Health Promotion and Education (7* 5 50) and Information & Referral (7* 5 70).

  rlog::log_info("Generating S452 60 00 & S452 65 00 statistic")

  calc_452_60_00 <- mis_unregistered_interactions |>
    filter_eligible("452") |>
    dplyr::filter(get_sr_code(funder_service_code) == "80") |>
    dplyr::filter(activity_individual_group == "Individual") |>
    dplyr::mutate(digits_1_3 = "452",
                  digits_4_5 = dplyr::if_else(funder_service_code == "72 5 70 10" & ir_version == "verCMHA", "65", "60"),
                  digits_6_7 = "00") |>
    assemble_statistical_account() |>
    dplyr::rename(date = activity_date)

  calc_fc_452_60_00 <- calc_452_60_00 |>
    dplyr::group_by(date, funder_service_code, funder_statistical_account_code) |>
    dplyr::summarize(value = sum(activity_count, na.rm = TRUE))

  calc_service_452_60_00 <- calc_452_60_00 |>
    dplyr::group_by(date, funder_service_code, service_name, funder_statistical_account_code) |>
    dplyr::summarize(value = sum(activity_count, na.rm = TRUE))


  calc_452_65_00 <- mis_unregistered_interactions |>
    filter_eligible("452") |>
    dplyr::filter(get_sr_code(funder_service_code) != "80") |>
    dplyr::filter(activity_individual_group == "Individual") |>
    dplyr::mutate(digits_1_3 = "452",
                  digits_4_5 = "65",
                  digits_6_7 = "00") |>
    assemble_statistical_account() |>
    dplyr::rename(date = activity_date)

  calc_fc_452_65_00 <- calc_452_65_00 |>
    dplyr::group_by(date, funder_service_code, funder_statistical_account_code) |>
    dplyr::summarize(value = sum(activity_count, na.rm = TRUE))

  calc_service_452_65_00 <- calc_452_65_00 |>
    dplyr::group_by(date, funder_service_code, service_name, funder_statistical_account_code) |>
    dplyr::summarize(value = sum(activity_count, na.rm = TRUE))


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
    dplyr::mutate(digits_1_3 = "454",
                  digits_4_5 = get_sr_code(funder_service_code),
                  digits_6_7 = participant_funder_age_group_code) |>
    assemble_statistical_account() |>
    dplyr::rename(date = activity_date)

  calc_fc_454 <- calc_454 |>
    dplyr::group_by(date, funder_service_code, funder_statistical_account_code) |>
    dplyr::summarize(value = sum(activity_duration/60, na.rm = TRUE))

  calc_service_454 <- calc_454 |>
    dplyr::group_by(date, funder_service_code, service_name, funder_statistical_account_code) |>
    dplyr::summarize(value = sum(activity_duration/60, na.rm = TRUE))

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


  rlog::log_info("Generating S483 & S484 statistic")

  calc_483_484 <- mis_visits |>
    filter_eligible("483|484") |>
    # clients must be uniquely identified
    dplyr::filter(!is.na(participant_id)) |>
    # interactions must be > 5 min to count
    dplyr::filter(activity_duration > 5 | activity_duration == 0 | is.na(activity_duration)) |>
    # remove indirect and clinical entries
    dplyr::filter(stringr::str_detect(activity_type, "ace-to-face")) |>

    # count only one attendance day per day; preferentially choose FTF over NFTF by sorting by activity_type
    dplyr::mutate(activity_type_priority = dplyr::case_when(activity_type == "Face-to-face" ~ 1,
                                              activity_type == "Face-to-face Virtual" ~ 2,
                                              .default = 3)) |>

    # assemble statistical account
    dplyr::mutate(digits_1_3 = dplyr::case_when(activity_type == "Non-face-to-face" ~ "484",
                                  activity_type %in% c("Face-to-face", "Face-to-face Virtual") ~ "483"),
                  digits_4_5 = get_sr_code(funder_service_code),
                  digits_6 = stringr::str_sub(participant_funder_age_group_code, 1, 1),
                  digits_7 = dplyr::case_when(
                                activity_type == "Face-to-face" ~ 1,
                                activity_type == "Face-to-face Virtual" ~ 3,
                                activity_type == "Non-face-to-face" ~ 0,
                                activity_individual_group == "Group" ~ 1,
                                .default = 0),
                  digits_6_7 = paste0(digits_6, digits_7)) |>
    assemble_statistical_account() |>
    dplyr::rename(date = activity_date)


  calc_fc_483_484 <- calc_483_484 |>
    dplyr::group_by(funder_service_code, participant_id, date) |>
    dplyr::arrange(funder_service_code, participant_id, date, activity_type_priority) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::count(date, funder_service_code, funder_statistical_account_code, name = "value")

  calc_service_483_484 <- calc_483_484 |>
    dplyr::group_by(funder_service_code, service_name, participant_id, date) |>
    dplyr::arrange(funder_service_code, service_name, participant_id, date, activity_type_priority) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::count(date, funder_service_code, service_name, funder_statistical_account_code, name = "value")


  # S489 ** *0 New Referral ----
  # (Internal Referral to the Functional Centre)
  # The number of service recipients accepted to receive service in a functional centre during the reporting period.  Includes all SRs who are eligible to receive service regardless of whether they had to wait or not for service to commence. There may be multiple referrals to various service functional centres as one SR may be referred to a number of functional centres. If a client has received service and later discharged from a functional centre and then re-admitted again to the same FC within the same reporting period, both referrals for the same client can be reported.
  # This statistic is a cumulative value for the reporting period and has the service recipient and age category.  Reporting is valid with age category – unknown, S489** 90.

  # client accepted if "active" or "on waitlist"
  # service_decision_date is the date on waitlist or start of service if not waitlisted


  rlog::log_info("Generating S489 statistic")

  calc_489 <- mis_service_history |>
    filter_eligible("489") |>
    dplyr::filter(!is.na(service_decision_date))

  calc_fc_489 <- calc_489 |>
    merge_overlaps(service_decision_date, service_status_end_date, lubridate::today(), merge_level = "funder_service_code") |>
    dplyr::mutate(digits_1_3 = "489",
                  digits_4_5 = get_sr_code(funder_service_code),
                  digits_6_7 = participant_funder_age_group_code) |>
    assemble_statistical_account() |>
    dplyr::rename(date = merged_start_date) |>
    dplyr::count(date, funder_service_code, funder_statistical_account_code, name = "value")

  calc_service_489 <- calc_489 |>
    merge_overlaps(service_decision_date, service_status_end_date, lubridate::today(), merge_level = "service_name") |>
    dplyr::mutate(digits_1_3 = "489",
                  digits_4_5 = get_sr_code(funder_service_code),
                  digits_6_7 = participant_funder_age_group_code) |>
    assemble_statistical_account() |>
    dplyr::rename(date = merged_start_date) |>
    dplyr::count(date, funder_service_code, service_name, funder_statistical_account_code, name = "value")



  # S491 65 10 Group Participants, Not Uniquely Identified Service Recipient Attendance ----
  #This is reported when a not uniquely identified service recipient receives services in a group session.
  #Individuals are not associated with an Admission or Referral and no health record/documentation has been initiated.
  # SR category = 65.
  #Note: Not Uniquely Identified Service Recipients participating in group sessions are not reported in S455 ** **, Individuals served by FC, nor in S855 ** 76, Individuals served by Organization.


  rlog::log_info("Generating S491 60 10 & S491 65 10 statistic")

  calc_491_60_10 <- mis_unregistered_interactions |>
    filter_eligible("491") |>
    dplyr::filter(get_sr_code(funder_service_code) == "80") |>
    dplyr::filter(activity_individual_group == "Group") |>
    dplyr::mutate(digits_1_3 = "491",
                  digits_4_5 = dplyr::if_else(funder_service_code == "72 5 70 10" & ir_version == "verCMHA", "65", "60"),
                  digits_6_7 = "10") |>
    assemble_statistical_account() |>
    dplyr::rename(date = activity_date)

  calc_fc_491_60_10 <- calc_491_60_10 |>
    dplyr::group_by(date, funder_service_code, funder_statistical_account_code) |>
    dplyr::summarize(value = sum(activity_count, na.rm = TRUE))

  calc_service_491_60_10 <- calc_491_60_10 |>
    dplyr::group_by(date, funder_service_code, service_name, funder_statistical_account_code) |>
    dplyr::summarize(value = sum(activity_count, na.rm = TRUE))


  calc_491_65_10 <- mis_unregistered_interactions |>
    filter_eligible("491") |>
    dplyr::filter(get_sr_code(funder_service_code) != "80") |>
    dplyr::filter(activity_individual_group == "Group") |>
    dplyr::mutate(digits_1_3 = "491",
                  digits_4_5 = "65",
                  digits_6_7 = "10") |>
    assemble_statistical_account() |>
    dplyr::rename(date = activity_date)

  calc_fc_491_65_10 <- calc_491_65_10 |>
    dplyr::group_by(date, funder_service_code, funder_statistical_account_code) |>
    dplyr::summarize(value = sum(activity_count, na.rm = TRUE))

  calc_service_491_65_10 <- calc_491_65_10 |>
    dplyr::group_by(date, funder_service_code, service_name, funder_statistical_account_code) |>
    dplyr::summarize(value = sum(activity_count, na.rm = TRUE))



  # S491 ** 20 Group Participants, Uniquely Identified Service Recipient Attendance ----
  #This is reported when a uniquely identified service recipient receives services in a group session.
  #The service is documented according to the health care organization’s policy.


  rlog::log_info("Generating S491 statistic")

  calc_491 <- mis_visits |>
    filter_eligible("491") |>
    dplyr::filter(activity_individual_group == "Group") |>
    dplyr::mutate(digits_1_3 = "491",
                  digits_4_5 = get_sr_code(funder_service_code),
                  digits_6_7 = "20") |>
    assemble_statistical_account() |>
    dplyr::rename(date = activity_date)

  calc_fc_491 <- calc_491 |>
    dplyr::count(date, funder_service_code, funder_statistical_account_code, name = "value")

  calc_service_491 <- calc_491 |>
    dplyr::count(date, funder_service_code, service_name, funder_statistical_account_code, name = "value")


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

  calc_fc_492 <- calc_492 |>
    dplyr::distinct(activity_date, activity_group_id, funder_service_code, funder_statistical_account_code) |>
    dplyr::rename(date = activity_date) |>
    dplyr::count(date, funder_service_code, funder_statistical_account_code, name = "value")

  calc_service_492 <- calc_492 |>
    dplyr::distinct(activity_date, activity_group_id, funder_service_code, service_name, funder_statistical_account_code) |>
    dplyr::rename(date = activity_date) |>
    dplyr::count(date, funder_service_code, service_name, funder_statistical_account_code, name = "value")


  # S501 ** *0 Admissions to Community Services (to the organization) ----
  # The number of individuals that are accepted for community services.  This is recorded each time an individual is accepted for service.
  # Individuals are counted each time a new file is created upon their admission to the agency. Service Recipient category is required. The decision date of eligibility is the admit date.
  # - May be counted more than once per year if a file has been closed and opened in the same year.
  # -	Reporting is valid with age category – unknown, S501** 90
  # -	Will always be equal to or greater than the S855* account value
  # Every Admission must have a File Closed (S511*) recorded when services end.  These are reported in the accounting centre 8 2 9 90.


  rlog::log_info("Generating S501 statistic")

  calc_org_501 <- mis_service_history |>
    dplyr::filter(service_status == "Active") |>
    merge_overlaps(service_status_start_date, service_status_end_date, lubridate::today(), merge_level = "organization") |>

    # get min. age group code for fiscal year (i.e. age group on latest visit/end of period)
    dplyr::mutate(fiscal_year = lubridate::quarter(merged_start_date, fiscal_start = 4) |> stringr::str_sub(1, 4)) |>
    dplyr::group_by(participant_id, sr_code, fiscal_year) |>
    dplyr::mutate(participant_funder_age_group_code = min(participant_funder_age_group_code)) |>
    dplyr::ungroup() |>

    # add organizational functional centre
    dplyr::mutate(funder_service_code = "82 9 90") |>

    # assemble funder_statistical_account_code
    dplyr::mutate(digits_1_3 = "501",
                  digits_4_5 = sr_code,
                  digits_6_7 = participant_funder_age_group_code) |>
    assemble_statistical_account() |>

    dplyr::rename(date = merged_start_date) |>
    dplyr::count(date, funder_service_code, funder_statistical_account_code, name = "value")


  # S506 ** *0 Individuals Received First Service ----
  # The number of registered service recipients (SR) who have had their assessment/intake and received their first service in the functional centre (FC) during current fiscal year.  This count includes the SRs who received their first service with or without waiting.  If a client has received service and later discharged from the functional centre and then readmitted to the same FC within the same reporting period, another first service count is reported.
  # This is a cumulative number, year-to-date count.
  # Note: The days individuals have waited are reported in S407**20 Days Waited for Service Initiation only after the individual received the first service in the functional centre.


  rlog::log_info("Generating S506 statistic")

  if(fc_506_version == "verA"){

    ## Service history-based calculation
    calc_506 <- mis_daily_census |>
      filter_eligible("506") |>
      dplyr::filter(service_status == "Active")

    calc_fc_506 <- calc_506 |>

      # find first date within functional centre (sum of service durations == 0)
      dplyr::group_by(date, participant_id, participant_funder_age_group_code, funder_service_code) |>
      dplyr::summarize(service_status_duration = sum(service_status_duration)) |>
      dplyr::ungroup() |>
      dplyr::filter(service_status_duration == 0) |>

      dplyr::mutate(digits_1_3 = "506",
                    digits_4_5 = get_sr_code(funder_service_code),
                    digits_6_7 = participant_funder_age_group_code) |>
      assemble_statistical_account() |>
      dplyr::count(date, funder_service_code, funder_statistical_account_code, name = "value")

    calc_service_506 <- calc_506 |>
      # find first date within service (sum of service durations)
      dplyr::group_by(date, participant_id, participant_funder_age_group_code, funder_service_code, service_name) |>
      dplyr::summarize(service_status_duration = sum(service_status_duration)) |>
      dplyr::ungroup() |>
      dplyr::filter(service_status_duration == 0) |>

      dplyr::mutate(digits_1_3 = "506",
                    digits_4_5 = get_sr_code(funder_service_code),
                    digits_6_7 = participant_funder_age_group_code) |>
      assemble_statistical_account() |>
      dplyr::count(date, funder_service_code, service_name, funder_statistical_account_code, name = "value")
  }

  if(fc_506_version == "verB"){
    ## Visit-based calculation
    calc_506 <- mis_service_history |>
      filter_eligible("506") |>
      dplyr::filter(service_status == "Active" & !is.na(service_first_direct_activity))

    calc_fc_506 <- calc_506 |>
      merge_overlaps(service_status_start_date, service_status_end_date, lubridate::today(), merge_level = "funder_service_code") |>
      dplyr::distinct(participant_id, participant_funder_age_group_code, service_first_direct_activity, funder_service_code) |>
      dplyr::mutate(digits_1_3 = "506",
                    digits_4_5 = get_sr_code(funder_service_code),
                    digits_6_7 = participant_funder_age_group_code) |>
      assemble_statistical_account() |>
      dplyr::rename(date = service_first_direct_activity) |>
      dplyr::count(date, funder_service_code, funder_statistical_account_code, name = "value")

    calc_service_506 <- calc_506 |>
      merge_overlaps(service_status_start_date, service_status_end_date, lubridate::today(), merge_level = "service_name") |>
      dplyr::distinct(participant_id, participant_funder_age_group_code, service_first_direct_activity, funder_service_code, service_name) |>
      dplyr::mutate(digits_1_3 = "506",
                    digits_4_5 = get_sr_code(funder_service_code),
                    digits_6_7 = participant_funder_age_group_code) |>
      assemble_statistical_account() |>
      dplyr::rename(date = service_first_direct_activity) |>
      dplyr::count(date, funder_service_code, service_name, funder_statistical_account_code, name = "value")
  }


  # S511 ** 90 File Closed (from the organization) ----
  # This account indicates the number of clients/service recipients who are no longer the responsibility of the healthcare organization. A file is closed when the need for services(s) has ended, or the client no longer qualifies for services from the healthcare organization.
  # The client has been discharged from all service(s) that they had been receiving from the agency.  This is reported in the accounting centre 82990.  File must be closed at time of death.


  rlog::log_info("Generating S511 statistic")

  ## CSS version with discharge reason
  calc_fc_511_css <- mis_service_history |>
    dplyr::filter(service_status == "Inactive", get_sr_code(funder_service_code) == "80") |>
    intersect_overlaps(service_decision_date, service_status_start_date, lubridate::today(), merge_level = "organization") |>

    # get first discharge reason
    dplyr::mutate(service_status_reason_code = stringr::str_sub(all_service_status_reasons, 1, 2),
           service_status_reason_code = dplyr::if_else(stringr::str_detect(service_status_reason_code, "\\d\\d"), service_status_reason_code, "90")) |>

    # add organizational functional centre
    dplyr::mutate(funder_service_code = "82 9 90") |>

    # group by participant to set one discharge status
    dplyr::group_by(participant_id) |>
    dplyr::mutate(digits_1_3 = "511",
                  digits_4_5 = "80",
                  digits_6_7 = dplyr::first(service_status_reason_code)) |>
    dplyr::ungroup() |>
    assemble_statistical_account() |>
    dplyr::rename(date = merged_end_date) |>
    dplyr::mutate(funder_service_code = "82 9 90") |>
    dplyr::count(date, funder_service_code, funder_statistical_account_code, name = "value")

  ## CMHA version without discharge reason
  calc_fc_511_cmha <-  mis_service_history |>
    dplyr::filter(service_status == "Inactive", get_sr_code(funder_service_code) != "80") |>
    intersect_overlaps(service_decision_date, service_status_start_date, lubridate::today(), merge_level = "organization") |>

    # add organizational functional centre
    dplyr::mutate(funder_service_code = "82 9 90") |>

    dplyr::mutate(digits_1_3 = "511",
                  digits_4_5 = "25",
                  digits_6_7 = "90") |>
    assemble_statistical_account() |>
    dplyr::rename(date = merged_end_date) |>
    dplyr::count(date, funder_service_code, funder_statistical_account_code, name = "value")


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

  calc_fc_512 <- mis_visits |>
    filter_eligible("512") |>
    dplyr::filter(stringr::str_detect(activity_assessment, "Assessment Complete/Outcome")) |>
    dplyr::mutate(digits_1_3 = "512",
                  digits_4_5 = get_sr_code(funder_service_code),
                  digits_6_7 = dplyr::case_when(stringr::str_detect(activity_assessment, "Assessment Complete/Outcome – Eligible In-Home – Client Declined") ~ "11",
                                         stringr::str_detect(activity_assessment, "Assessment Complete/Outcome – Eligible In-Home – Admitted") ~ "12",
                                         stringr::str_detect(activity_assessment, "Assessment Complete/Outcome – Eligible – Adult Day") ~ "21",
                                         stringr::str_detect(activity_assessment, "Assessment Complete/Outcome – Eligible – Supportive Housing") ~ "22",
                                         stringr::str_detect(activity_assessment, "Assessment Complete/Outcome – Eligible – Enhanced Respite") ~ "23",
                                         stringr::str_detect(activity_assessment, "Assessment Complete/Outcome – Eligible In-Home – Other Organizations") ~ "29",
                                         .default = "12")) |>
    assemble_statistical_account() |>
    dplyr::rename(date = activity_date) |>
    dplyr::count(date, funder_service_code, funder_statistical_account_code, name = "value")

  calc_service_512 <- mis_visits |>
    filter_eligible("512") |>
    dplyr::filter(stringr::str_detect(activity_assessment, "Assessment Complete/Outcome")) |>
    dplyr::mutate(digits_1_3 = "512",
                         digits_4_5 = get_sr_code(funder_service_code),
                         digits_6_7 = dplyr::case_when(stringr::str_detect(activity_assessment, "Assessment Complete/Outcome – Eligible In-Home – Client Declined") ~ "11",
                                                stringr::str_detect(activity_assessment, "Assessment Complete/Outcome – Eligible In-Home – Admitted") ~ "12",
                                                stringr::str_detect(activity_assessment, "Assessment Complete/Outcome – Eligible – Adult Day") ~ "21",
                                                stringr::str_detect(activity_assessment, "Assessment Complete/Outcome – Eligible – Supportive Housing") ~ "22",
                                                stringr::str_detect(activity_assessment, "Assessment Complete/Outcome – Eligible – Enhanced Respite") ~ "23",
                                                stringr::str_detect(activity_assessment, "Assessment Complete/Outcome – Eligible In-Home – Other Organizations") ~ "29",
                                                .default = "12")) |>
    assemble_statistical_account() |>
    dplyr::rename(date = activity_date) |>
    dplyr::count(date, funder_service_code, service_name, funder_statistical_account_code, name = "value")


  # Write mis_visit_and_service_counts_by_fc to file ----

  rlog::log_info("Writing cumulative-type statistics files")

  mis_visit_and_service_counts_by_fc <- dplyr::bind_rows(calc_fc_248, calc_fc_265, calc_fc_266, calc_fc_401, calc_fc_407, calc_fc_407_99, calc_fc_410_513_cmha, calc_fc_410_513_css, calc_fc_450_451_cmha, calc_fc_450_451_css, calc_fc_452_60_00, calc_fc_452_65_00, calc_fc_454, calc_fc_483_484, calc_fc_489, calc_fc_491_60_10, calc_fc_491_65_10, calc_fc_491, calc_fc_492, calc_org_501, calc_fc_506, calc_fc_511_cmha, calc_fc_511_css, calc_fc_512) |>
    dplyr::mutate(funder_key = paste0(funder_service_code, "_", funder_statistical_account_code))

  readr::write_csv(mis_visit_and_service_counts_by_fc, paste0(data_folder, "/processed/mis_visit_and_service_counts_by_fc.csv"), na = "")


  mis_visit_and_service_counts_by_service <- dplyr::bind_rows(calc_service_248, calc_service_265, calc_service_266, calc_service_401,  calc_service_407, calc_service_407_99, calc_service_410_513_cmha, calc_service_410_513_css, calc_service_450_451_cmha, calc_service_450_451_css, calc_service_452_60_00, calc_service_452_65_00, calc_service_454, calc_service_483_484, calc_service_489, calc_service_491_60_10, calc_service_491_65_10, calc_service_491, calc_service_492, calc_service_506, calc_service_512) |>
    dplyr::mutate(funder_service_key = paste0(funder_service_code, "_", service_name, "_", funder_statistical_account_code))

  readr::write_csv(mis_visit_and_service_counts_by_service, paste0(data_folder, "/processed/mis_visit_and_service_counts_by_service.csv"), na = "")

}
