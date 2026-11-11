# Detail tables: the rows behind every statistic (date, unit, account, aggregation, client,
# reference ID and note, value, key). Census statistics are one row per client per day; the visit and
# service summaries are the sums of their rows.

statistic_of <- function(account) substr(account, 1, 3)

# Visit and service detail summed back to date, reporting unit and account, compared with
# the summary rows
expect_detail_matches_summary <- function(out, grain) {
  summary <- out[[paste0("mis_visit_and_service_counts_by_", grain)]]
  detail <- out[[paste0("mis_detail_by_", grain)]] |>
    dplyr::filter(statistic_of(funder_statistical_account_code) %in% statistic_of(summary$funder_statistical_account_code))
  keys <- c("date", grain_cols(grain), "funder_statistical_account_code")

  from_detail <- detail |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::summarize(value = sum(value), .groups = "drop") |>
    dplyr::arrange(dplyr::across(dplyr::all_of(keys)))
  from_summary <- summary |>
    dplyr::select(dplyr::all_of(keys), value) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(keys)))

  expect_gt(nrow(from_detail), 0)
  expect_equal(from_detail, from_summary, ignore_attr = TRUE)
}

test_that("visit and service detail adds up to the summary rows, by FC and by service", {
  for (version in c("verA", "verB")) {
    out <- run_mis(fc_506_version = version)
    expect_detail_matches_summary(out, "fc")
    expect_detail_matches_summary(out, "service")
  }
})

test_that("census rows in the detail are one row per client per day, with value 1", {
  for (version in c("verA", "verB", "verC")) {
    out <- run_mis(fc_455_version = version)
    for (detail in list(out$mis_detail_by_fc, out$mis_detail_by_service)) {
      census <- detail[detail$aggregation != "sum_visits_and_services", ]
      expect_gt(nrow(census), 0)
      expect_false(anyNA(census$participant_id))
      expect_true(all(census$value == 1))
      expect_false(anyNA(census$reference_id))
      # only the waits (S406) have a note
      expect_true(all(is.na(census$reference_note[statistic_of(census$funder_statistical_account_code) != "406"])))
      expect_equal(nrow(dplyr::distinct(census)), nrow(census))
    }
  }
})

test_that("detail carries the aggregation type of each statistic", {
  detail <- run_mis()$mis_detail_by_fc
  expect_false(anyNA(detail$aggregation))
  agg <- function(stat) unique(detail$aggregation[statistic_of(detail$funder_statistical_account_code) == stat])
  expect_equal(agg("403"), "sum_census")
  expect_equal(agg("406"), "end_of_period")
  expect_equal(agg("455"), "distinct_cumulative")
  expect_equal(agg("450"), "sum_visits_and_services")
})

test_that("detail has the same few columns at both grains", {
  out <- run_mis()
  expect_equal(names(out$mis_detail_by_fc),
               c("date", "funder_service_code", "funder_statistical_account_code", "aggregation",
                 "participant_id", "reference_id", "reference_note", "value", "funder_key"))
  expect_equal(names(out$mis_detail_by_service),
               c("date", "funder_service_code", "service_name", "funder_statistical_account_code", "aggregation",
                 "participant_id", "reference_id", "reference_note", "value", "funder_service_key"))
  # the keys are built as in the summary tables, so the detail joins to the funder codes tables
  fc <- out$mis_detail_by_fc
  expect_equal(fc$funder_key, paste0(fc$funder_service_code, "_", fc$funder_statistical_account_code))
  service <- out$mis_detail_by_service
  expect_equal(service$funder_service_key,
               paste0(service$funder_service_code, "_", service$service_name, "_", service$funder_statistical_account_code))
  visit_keys <- out$mis_visit_and_service_counts_by_fc$funder_key
  expect_true(all(visit_keys %in% fc$funder_key))
  # every statistic in the visit and service table has detail rows
  expect_true(all(unique(statistic_of(out$mis_visit_and_service_counts_by_fc$funder_statistical_account_code)) %in%
                    statistic_of(out$mis_detail_by_fc$funder_statistical_account_code)))
})

test_that("episode detail references the enrollment", {
  detail <- run_mis()$mis_detail_by_fc
  stat <- statistic_of(detail$funder_statistical_account_code)

  # R3: back on the waitlist Jun 23, so discharged that day (S513) and referred again (S489)
  discharge <- detail[detail$participant_id %in% "R3" & stat == "513", ]
  expect_equal(discharge$date, as.Date("2025-06-23"))
  expect_equal(discharge$reference_id, "E15")
  expect_equal(sort(detail$date[detail$participant_id %in% "R3" & stat == "489"]),
               as.Date(c("2025-04-21", "2025-06-23")))

  # R2: file closed Jun 9, then a new admission Jul 7, both on enrollment E14 (AC 82 9 90)
  r2 <- detail[detail$participant_id %in% "R2" & detail$funder_service_code == "82 9 90", ]
  r2_stat <- statistic_of(r2$funder_statistical_account_code)
  expect_equal(r2$date[r2_stat == "511"], as.Date("2025-06-09"))
  expect_equal(r2$reference_id[r2_stat == "511"], "E14")
  expect_true(as.Date("2025-07-07") %in% r2$date[r2_stat == "501"])
})

test_that("merged enrollments: the reference lists every enrollment", {
  # Two overlapping enrollments in one FC: E1 starts Apr 1 and is discharged May 31; E2
  # starts Apr 15 and is discharged Jun 30. One episode, referencing both.
  history <- tibble::tibble(
    participant_id = "X1", funder_service_code = "72 5 10 76 20", service_name = "ACT",
    participant_funder_age_group_code = "40",
    service_enrollment_id = c("E1", "E1", "E2", "E2"),
    service_status = factor(c("Active", "Inactive", "Active", "Inactive"), levels = service_status_levels),
    service_status_start_date = as.Date(c("2025-04-01", "2025-05-31", "2025-04-15", "2025-06-30")),
    service_decision_date = as.Date(c("2025-04-01", "2025-04-01", "2025-04-15", "2025-04-15")),
    service_status_reason = c(NA, "10 - Goals met", NA, "60 - Client preference"),
    service_first_direct_activity = as.Date(NA)
  ) |>
    dplyr::group_by(service_enrollment_id) |>
    dplyr::mutate(service_status_end_date = dplyr::lead(service_status_start_date)) |>
    dplyr::ungroup() |>
    number_episodes()

  episode <- fc_episodes(history, "fc", as.Date("2025-09-30"))
  expect_equal(nrow(episode), 1)
  expect_equal(episode$enrollment_ids, "E1, E2")
  expect_equal(episode$last_service_status_reason, "60 - Client preference")
})

test_that("visit detail references the activity that was counted", {
  detail <- run_mis()$mis_detail_by_fc
  stat <- statistic_of(detail$funder_statistical_account_code)
  activities <- fixture_activities()

  # P1, May 5: face-to-face and non-face-to-face the same day. One visit (S450), from the
  # face-to-face activity; two interactions (S265), one row each.
  on_day <- detail$participant_id %in% "P1" & detail$date == as.Date("2025-05-05")
  visit <- detail[on_day & stat == "450", ]
  expect_equal(nrow(visit), 1)
  expect_equal(activities$activity_type[activities$activity_id == visit$reference_id], "Face-to-face")
  expect_equal(sum(detail$value[on_day & stat == "265"]), 2)
  expect_equal(nrow(detail[on_day & stat == "265", ]), 2)
})

test_that("group detail: hours shared across participants, sessions reference the group", {
  detail <- run_mis()$mis_detail_by_fc
  on_day <- detail[detail$funder_service_code == ps_fc & detail$date == as.Date("2025-06-02"), ]
  stat <- statistic_of(on_day$funder_statistical_account_code)

  # S454: the 3-hour session is split between P5 and P6, so it totals 3 hours, not 6
  expect_setequal(on_day$participant_id[stat == "454"], c("P5", "P6"))
  expect_equal(on_day$value[stat == "454"], c(1.5, 1.5))

  # S492: one session row, referencing the group, with no client
  session <- on_day[stat == "492", ]
  expect_equal(nrow(session), 1)
  expect_equal(session$reference_id, "G1")
  expect_true(is.na(session$participant_id))

  # attendance (S491) and hours (S454) reference the session too, so one filter shows it all
  expect_equal(unique(on_day$reference_id[stat %in% c("454", "491")]), "G1")
})

test_that("census rows reference the enrollment, or the visit that made the client count", {
  ids <- function(x) unique(unlist(strsplit(x, ", ", fixed = TRUE)))
  activity_ids <- fixture_activities()$activity_id
  enrollment_ids <- fixture_services()$service_enrollment_id

  for (version in c("verA", "verB", "verC")) {
    detail <- run_mis(fc_455_version = version)$mis_detail_by_fc
    stat <- statistic_of(detail$funder_statistical_account_code)
    # S403 and S406: the enrollment the client was on
    expect_true(all(ids(detail$reference_id[stat %in% c("403", "406")]) %in% enrollment_ids))
    # served statistics: the visits (verA, verB), or the active enrollment (verC). Resident
    # days with no visit reference the enrollment under every version.
    is_served <- stat %in% c("455", "855", "950", "955")
    served <- ids(detail$reference_id[is_served])
    expect_gt(length(served), 0)
    expect_true(all(served %in% c(activity_ids, enrollment_ids)), info = version)
    outside_residential <- is_served & !detail$funder_service_code %in% c(al_fc, "82 9 90", "83 9 90")
    expect_true(all(ids(detail$reference_id[outside_residential]) %in% if (version == "verC") enrollment_ids else activity_ids),
                info = version)
  }
})

test_that("reference note: the earlier event behind the row, with its date", {
  detail <- run_mis()$mis_detail_by_fc
  stat <- statistic_of(detail$funder_statistical_account_code)
  noted <- function(rows) as.Date(substring(rows$reference_note, nchar(rows$reference_note) - 9))
  days <- function(rows) as.numeric(rows$date - noted(rows))
  label <- function(rows) unique(sub(" [0-9-]{10}$", "", rows$reference_note))

  # assessment wait: from referral to decision (the row's date), so the value is the days between
  assessment <- detail[detail$funder_statistical_account_code == "407 99 10", ]
  expect_gt(nrow(assessment), 0)
  expect_equal(label(assessment), "Referred")
  expect_equal(assessment$value, days(assessment))

  # same-day waits add no days and have no rows
  expect_true(all(detail$value[stat == "407"] > 0))

  # wait for service: dated at service start; hold days aren't counted, so the value can be less
  wait <- detail[stat == "407" & detail$funder_statistical_account_code != "407 99 10", ]
  expect_gt(nrow(wait), 0)
  expect_equal(label(wait), "Wait started")
  expect_true(all(wait$value <= days(wait)))

  # admissions and first service note the acceptance; discharges the admission; file closed
  # the acceptance (the S501 date)
  for (s in c("401", "506", "511")) expect_equal(label(detail[stat == s, ]), "Accepted", info = s)
  expect_equal(label(detail[stat %in% c("410", "411", "513"), ]), "Admitted")
  episodes <- detail[stat %in% c("401", "410", "411", "506", "511", "513"), ]
  expect_true(all(noted(episodes) <= episodes$date))

  # R3 is accepted Apr 21 (on the waitlist), admitted May 19 (service starts) and discharged
  # Jun 23 (S513): the discharge notes the admission, not the acceptance
  r3 <- detail[detail$participant_id %in% "R3" & stat == "513", ]
  expect_equal(r3$reference_note, "Admitted 2025-05-19")

  # referrals and organization admissions are dated at the acceptance, so no note
  expect_true(all(is.na(detail$reference_note[!stat %in% c("401", "406", "407", "410", "411", "506", "511", "513")])))
})

test_that("waiting rows (S406) note when the wait began", {
  detail <- run_mis()$mis_detail_by_fc
  stat <- statistic_of(detail$funder_statistical_account_code)
  label <- function(rows) unique(sub(" [0-9-]{10}$", "", rows$reference_note))

  expect_equal(label(detail[detail$funder_statistical_account_code != "406 99 10" & stat == "406", ]), "Wait started")
  expect_equal(label(detail[detail$funder_statistical_account_code == "406 99 10", ]), "Referred")

  # R3 waits from Apr 21 (counted from the next day) until service starts May 19, then waits
  # again from Jun 23: each day notes the wait it belongs to
  r3 <- detail[detail$participant_id %in% "R3" & stat == "406", ]
  expect_equal(r3$reference_note[r3$date == as.Date("2025-04-22")], "Wait started 2025-04-21")
  expect_equal(r3$reference_note[r3$date == as.Date("2025-05-18")], "Wait started 2025-04-21")
  expect_equal(r3$reference_note[r3$date == as.Date("2025-06-24")], "Wait started 2025-06-23")

  # no longer waiting on the day service starts (OHRS Ch. 10, S406 ** 20): the first wait's
  # last day is May 18, the second's Jul 20
  expect_equal(max(r3$date[r3$date < as.Date("2025-06-23")]), as.Date("2025-05-18"))
  expect_equal(max(r3$date), as.Date("2025-07-20"))
})

test_that("detail files are written", {
  out <- run_mis()
  for (name in c("mis_detail_by_fc", "mis_detail_by_service")) {
    path <- file.path(attr(out, "data_folder"), "processed", paste0(name, ".csv"))
    expect_true(file.exists(path))
    written <- readr::read_csv(path, col_types = readr::cols(.default = "c"))
    expect_equal(nrow(written), nrow(out[[name]]))
    expect_equal(names(written), names(out[[name]]))
  }
})
