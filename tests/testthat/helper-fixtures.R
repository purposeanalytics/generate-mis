# Made-up clients for testing. Each client exercises one counting rule; the comments
# say which. All clients are adults (age group 40) with no special needs, and all
# dates fall in fiscal year 2025-26.

mh_fc   <- "72 5 09 76"  # Case Management - Mental Health (fund type 2)
mh3_fc  <- "73 5 09 76"  # Case Management - Mental Health (fund type 3)
ps_fc   <- "72 5 82 33"  # CSS Personal Support/Independence Training
cm_fc   <- "72 5 82 09"  # CSS Case Management
act_fc  <- "72 5 10 76 20"  # MH Assertive Community Treatment Teams (episode rules, R1-R6)
day_fc  <- "72 5 20 76 10"  # Day/Night Care - Mental Health (attendance days, S483)
tr3_fc  <- "73 5 82 14"     # CSS Transportation - Client (fund type 3)
al_fc   <- "72 5 82 45"     # CSS Assisted Living (residential: S410/S411 instead of S513)

service_names <- c("72 5 09 76" = "MH Case Management",
                   "73 5 09 76" = "MH Case Management FT3",
                   "72 5 82 33" = "Personal Support",
                   "72 5 82 09" = "CSS Case Management",
                   "72 5 10 76 20" = "ACT Team",
                   "72 5 20 76 10" = "MH Day Program",
                   "73 5 82 14" = "Transportation FT3",
                   "72 5 82 45" = "Assisted Living")

# One service status row
status <- function(participant_id, enrollment, fc, service_status, start,
                   decision = NA, reason = NA_character_, end = NA) {
  tibble::tibble(
    participant_id = participant_id,
    service_enrollment_id = enrollment,
    service_id = paste0("S-", enrollment),
    funder_service_code = fc,
    service_name = unname(service_names[fc]),
    service_status = service_status,
    service_status_start_date = as.Date(start),
    service_status_end_date = as.Date(end),
    service_decision_date = as.Date(decision),
    service_status_reason = reason,
    service_first_direct_activity = lubridate::NA_Date_,
    participant_funder_age_group_code = "40"
  )
}

# One completed activity
activity <- function(participant_id, enrollment, fc, date, type, minutes,
                     individual_group = "Individual", group_id = NA_character_) {
  tibble::tibble(
    participant_id = participant_id,
    service_id = paste0("S-", enrollment),
    funder_service_code = fc,
    service_name = unname(service_names[fc]),
    activity_date = as.Date(date),
    activity_type = type,
    activity_individual_group = individual_group,
    activity_duration = minutes,
    activity_status = "Completed",
    activity_count = 1,
    activity_worker_count = 1,
    activity_group_id = group_id,
    service_sub_service_name = NA_character_,
    participant_funder_age_group_code = "40",
    activity_assessment = NA_character_
  )
}

fixture_services <- function() {
  dplyr::bind_rows(
    # P1: referral -> assessment wait -> waitlist -> active -> discharged
    #   S407 99 10 = 10 days (Apr 1 to decision Apr 11), S407 25 20 = 20 days (Apr 11 to May 1),
    #   S489 and S501 on the decision date, S506 on May 1, S513 and S511 on Aug 15
    status("P1", "E1", mh_fc, "Waiting for Assessment", "2025-04-01", decision = "2025-04-11"),
    status("P1", "E1", mh_fc, "On Waitlist",            "2025-04-11", decision = "2025-04-11"),
    status("P1", "E1", mh_fc, "Active",                 "2025-05-01", decision = "2025-04-11"),
    status("P1", "E1", mh_fc, "Inactive",               "2025-08-15", decision = "2025-04-11", reason = "Goals met"),

    # P2: waitlist -> on hold -> active. Only the 20 waitlist days count in S407 25 20,
    #   reported on the day service starts (May 11), not the 20 days on hold.
    status("P2", "E2", mh_fc, "On Waitlist", "2025-04-01", decision = "2025-04-01"),
    status("P2", "E2", mh_fc, "On Hold",     "2025-04-21", decision = "2025-04-01"),
    status("P2", "E2", mh_fc, "Active",      "2025-05-11", decision = "2025-04-01"),

    # P3: still waiting for assessment at the end of Q2. Counted in S406 99 10 on Sep 30;
    #   no days waited (S407 99 10) because no decision has been made. Has one visit while
    #   waiting, so is served under fc_455_version = "verB" (visit-based) only.
    status("P3", "E3", mh_fc, "Waiting for Assessment", "2025-09-01"),

    # P4: on the waitlist, then withdrew before service started. In S406 25 20 while
    #   waiting; never in S407 25 20, S506 or S513.
    status("P4", "E4", mh_fc, "On Waitlist", "2025-06-01", decision = "2025-06-01"),
    status("P4", "E4", mh_fc, "Inactive",    "2025-07-01", decision = "2025-06-01", reason = "Withdrew"),

    # P5, P6: CSS personal support clients who attend one 3-hour group session together
    status("P5", "E5", ps_fc, "Active", "2025-04-01", decision = "2025-04-01"),
    status("P6", "E6", ps_fc, "Active", "2025-04-01", decision = "2025-04-01"),

    # P7: CSS personal support client whose only activity is an assessment, which is not
    #   client activity outside case management, so not served (S455 / S855)
    status("P7", "E7", ps_fc, "Active", "2025-07-01", decision = "2025-07-01"),

    # P8: CSS case management client whose only activity is an assessment, which does
    #   count there (visit and served)
    status("P8", "E8", cm_fc, "Active", "2025-07-01", decision = "2025-07-01"),

    # P9: CMH&A client served in both fund type 2 and fund type 3 FCs, so counted once in
    #   S855 at AC 82 9 90 and once at AC 83 9 90
    status("P9", "E9",  mh_fc,  "Active", "2025-04-01", decision = "2025-04-01"),
    status("P9", "E10", mh3_fc, "Active", "2025-04-01", decision = "2025-04-01"),

    # P10: CMH&A client whose only contact is a 3-minute call. Not a visit under
    #   fc_450_version = "verA", so not served (S455 / S855) either; both under "verB".
    status("P10", "E11", mh_fc, "Active", "2025-08-01", decision = "2025-08-01"),

    # P11: French-speaking CSS personal support client with a physical disability and one
    #   non-face-to-face visit. Requested service in French: S950 80 10 in the FC and
    #   S950 10 80 at AC 82 9 90. Physical disability: S955 80 10 in the FC.
    #   Language and special needs are on both the participant record and the activity.
    status("P11", "E12", ps_fc, "Active", "2025-08-01", decision = "2025-08-01"),

    # R1-R6: episode rules (test-episode-rules.R), in their own FC with no visits.
    # R1: service paused, then resumed: one episode
    status("R1", "E13", act_fc, "Active",  "2025-04-07", decision = "2025-04-07"),
    status("R1", "E13", act_fc, "On Hold", "2025-05-05", decision = "2025-04-07"),
    status("R1", "E13", act_fc, "Active",  "2025-06-16", decision = "2025-04-07"),
    # R2: discharged, then the file reopened: two episodes
    status("R2", "E14", act_fc, "On Waitlist", "2025-04-14", decision = "2025-04-14"),
    status("R2", "E14", act_fc, "Active",      "2025-05-12", decision = "2025-04-14"),
    status("R2", "E14", act_fc, "Inactive",    "2025-06-09", decision = "2025-04-14", reason = "10 - Goals met"),
    status("R2", "E14", act_fc, "On Waitlist", "2025-07-07", decision = "2025-07-07"),
    status("R2", "E14", act_fc, "Active",      "2025-08-04", decision = "2025-07-07"),
    # R3: back on the waitlist after service started: discharged, then a new episode
    #   (OHRS Ch. 10, 10.9.4.4 example 2)
    status("R3", "E15", act_fc, "On Waitlist", "2025-04-21", decision = "2025-04-21"),
    status("R3", "E15", act_fc, "Active",      "2025-05-19", decision = "2025-04-21"),
    status("R3", "E15", act_fc, "On Waitlist", "2025-06-23", decision = "2025-06-23"),
    status("R3", "E15", act_fc, "Active",      "2025-07-21", decision = "2025-06-23"),
    # R4: no decision date (an error in the source data): warned about, no referral (S489),
    #   but the service that started is still counted
    status("R4", "E16", act_fc, "On Waitlist", "2025-04-28"),
    status("R4", "E16", act_fc, "Active",      "2025-05-26"),
    # R5: Active entered and corrected the same day (back to waiting): the Active is dropped
    status("R5", "E17", act_fc, "On Waitlist", "2025-04-02", decision = "2025-04-02", end = "2025-05-05"),
    status("R5", "E17", act_fc, "Active",      "2025-05-05", decision = "2025-04-02", end = "2025-05-05"),
    status("R5", "E17", act_fc, "On Waitlist", "2025-05-05", decision = "2025-04-02", end = "2025-06-02"),
    status("R5", "E17", act_fc, "Active",      "2025-06-02", decision = "2025-04-02"),
    # R6: put on the waitlist and started the same day: the Active is kept
    status("R6", "E18", act_fc, "On Waitlist", "2025-04-08", decision = "2025-04-08", end = "2025-04-08"),
    status("R6", "E18", act_fc, "Active",      "2025-04-08", decision = "2025-04-08"),

    # P12: MH day program (see fixture_activities())
    status("P12", "E19", day_fc, "Active", "2025-04-15", decision = "2025-04-15"),
    # P13: CSS transportation, fund type 3
    status("P13", "E20", tr3_fc, "Active", "2025-04-01", decision = "2025-04-01"),

    # P14: assisted living, died in care on Jun 17 (S411, not S410)
    status("P14", "E21", al_fc, "Active",   "2025-04-22", decision = "2025-04-22"),
    status("P14", "E21", al_fc, "Inactive", "2025-06-17", decision = "2025-04-22", reason = "20 - Death"),
    # P15: assisted living, moved out on Jun 24 (S410)
    status("P15", "E22", al_fc, "Active",   "2025-04-22", decision = "2025-04-22"),
    status("P15", "E22", al_fc, "Inactive", "2025-06-24", decision = "2025-04-22", reason = "60 - Client preference")
  )
}

fixture_activities <- function() {
  dplyr::bind_rows(
    # P1, May 5: face-to-face and non-face-to-face on the same day = 1 face-to-face visit,
    #   but 2 service provider interactions (one 31-60 min, one 6-30 min)
    activity("P1", "E1", mh_fc, "2025-05-05", "Face-to-face", 60),
    activity("P1", "E1", mh_fc, "2025-05-05", "Non-face-to-face", 20),
    # P1, May 12: 3 minutes, under the 5-minute minimum (fc_450_version = "verA")
    activity("P1", "E1", mh_fc, "2025-05-12", "Non-face-to-face", 3),
    # P1, May 19: counts as a non-face-to-face visit
    activity("P1", "E1", mh_fc, "2025-05-19", "Non-face-to-face", 15),

    # P3: a visit while still waiting for assessment (not enrolled)
    activity("P3", "E3", mh_fc, "2025-09-10", "Face-to-face", 30),

    # P2: one visit after service starts
    activity("P2", "E2", mh_fc, "2025-05-20", "Face-to-face", 45),

    # P5 + P6: one 3-hour group session. Hours of service = 3 (session length),
    #   not 6 (length x participants).
    activity("P5", "E5", ps_fc, "2025-06-02", "Face-to-face", 180, "Group", "G1"),
    activity("P6", "E6", ps_fc, "2025-06-02", "Face-to-face", 180, "Group", "G1"),

    # P7: assessment only, in a CSS service delivery FC
    activity("P7", "E7", ps_fc, "2025-07-07", "Assessment", 60),

    # P8: assessment only, in the CSS case management FC
    activity("P8", "E8", cm_fc, "2025-07-07", "Assessment", 60),

    # P9: one visit in each fund type
    activity("P9", "E9",  mh_fc,  "2025-06-10", "Face-to-face", 30),
    activity("P9", "E10", mh3_fc, "2025-06-11", "Face-to-face", 30),

    # P10: 3-minute call only
    activity("P10", "E11", mh_fc, "2025-08-05", "Non-face-to-face", 3),

    # P11: phone visit
    activity("P11", "E12", ps_fc, "2025-08-12", "Non-face-to-face", 30),

    # P12: a 3-minute attendance on Jun 3 (S483 under fc_450_version = "verB" only), and an
    #   activity recorded as "Assessment" on Jun 10, which isn't evidence of service outside
    #   CSS case management (CMH&A records assessments as visits)
    activity("P12", "E19", day_fc, "2025-06-03", "Face-to-face", 3),
    activity("P12", "E19", day_fc, "2025-06-10", "Assessment", 60),

    # P13: a 3-minute trip. Transportation has no 5-minute rule, in either fund type.
    activity("P13", "E20", tr3_fc, "2025-06-04", "Face-to-face", 3)
  ) |>
    # Language and special needs as recorded on each activity, which is what
    # fc_455_version = "verB" reads; the other versions read the participant record.
    dplyr::mutate(participant_preferred_language = dplyr::if_else(participant_id == "P11", "French", "English"),
                  participant_ohrs_special_needs = dplyr::if_else(participant_id == "P11", "Physical Disability", NA_character_))
}

fixture_participants <- function() {
  tibble::tibble(
    participant_id = c(paste0("P", 1:15), paste0("R", 1:6)),
    participant_date_of_birth = as.Date("1980-01-01")
  ) |>
    dplyr::mutate(participant_ohrs_special_needs = dplyr::if_else(participant_id == "P11", "Physical Disability", NA_character_),
                  participant_preferred_language = dplyr::if_else(participant_id == "P11", "French", "English"))
}

fixture_fc_mapping <- function() {
  tibble::tibble(funder_service_code = names(service_names),
                 service_name = unname(service_names),
                 service_department = NA_character_)
}

# Write the fixtures to a temporary data folder and run the full pipeline.
# Results are cached per set of arguments so each scenario runs once per test session.
.mis_cache <- new.env()

run_mis <- function(report_date = "2025-09-30", ...) {
  key <- paste(report_date, paste(names(list(...)), list(...), collapse = ","))
  if (!is.null(.mis_cache[[key]])) return(.mis_cache[[key]])

  data_folder <- file.path(tempfile("mis-"), "data")
  dir.create(file.path(data_folder, "interim", "rds"), recursive = TRUE)
  dir.create(file.path(data_folder, "processed"), recursive = TRUE)

  readr::write_rds(fixture_participants(), file.path(data_folder, "interim/rds/participants.rds"))
  readr::write_rds(fixture_services(),     file.path(data_folder, "interim/rds/services.rds"))
  readr::write_rds(fixture_activities(),   file.path(data_folder, "interim/rds/activities.rds"))
  readr::write_rds(fixture_fc_mapping(),   file.path(data_folder, "interim/rds/functional_centre_mapping.rds"))

  withr::local_envvar(LOG_LEVEL = "ERROR")
  # record warnings (e.g. R4's missing decision date) instead of repeating them in every test
  warnings <- character()
  out <- withCallingHandlers(
    suppressMessages(generate_tables(data_folder, report_date = as.Date(report_date), ...)),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  attr(out, "warnings") <- warnings
  attr(out, "data_folder") <- data_folder
  .mis_cache[[key]] <- out
  out
}

# Sum of `value` for one FC, account and date (0 if there are no rows)
value_on <- function(tbl, fc, account, date) {
  rows <- tbl[tbl$funder_service_code == fc &
                tbl$funder_statistical_account_code == account &
                tbl$date == as.Date(date), ]
  sum(rows$value)
}

# Total `value` for one FC and account across all dates
value_total <- function(tbl, fc, account) {
  rows <- tbl[tbl$funder_service_code == fc & tbl$funder_statistical_account_code == account, ]
  sum(rows$value)
}

# Distinct people in a participant-level (count distinct) statistic, across all dates
people <- function(tbl, fc, account) {
  rows <- tbl[tbl$funder_service_code == fc & tbl$funder_statistical_account_code == account, ]
  sort(unique(rows$participant_id))
}
