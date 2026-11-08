# Detail tables: the rows behind every statistic. Visit and service counts are the sums of
# their detail rows; census counts are their detail runs expanded back into days.

# Visit and service detail summed back to date, reporting unit and account, compared with
# the summary rows
expect_detail_matches_summary <- function(out, grain) {
  summary <- out[[paste0("mis_visit_and_service_counts_by_", grain)]]
  detail <- out[[paste0("mis_detail_by_", grain)]] |>
    dplyr::filter(statistic %in% substr(summary$funder_statistical_account_code, 1, 3))
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

test_that("detail rows add up to the summary rows, by FC and by service", {
  for (version in c("verA", "verB")) {
    out <- run_mis(fc_506_version = version)
    expect_detail_matches_summary(out, "fc")
    expect_detail_matches_summary(out, "service")
  }
})

test_that("detail rows name the client, enrollment and why they were counted", {
  out <- run_mis()
  detail <- out$mis_detail_by_fc
  # every statistic in the census and visit tables has detail rows, with its aggregation type
  expect_setequal(unique(detail$statistic),
                  unique(substr(c(out$mis_visit_and_service_counts_by_fc$funder_statistical_account_code,
                                  out$mis_census_counts_by_fc$funder_statistical_account_code), 1, 3)))
  expect_false(anyNA(detail$aggregation))

  # R3: back on the waitlist Jun 23, so discharged that day (S513) and referred again (S489)
  r3 <- detail[detail$participant_id %in% "R3", ]
  discharge <- r3[r3$statistic == "513", ]
  expect_equal(discharge$date, as.Date("2025-06-23"))
  expect_equal(discharge$service_enrollment_id, "E15")
  expect_equal(discharge$source_date, as.Date("2025-04-21"))  # the episode began Apr 21
  expect_equal(sort(r3$date[r3$statistic == "489"]), as.Date(c("2025-04-21", "2025-06-23")))

  # P14: the S411 death carries the discharge reason
  expect_equal(detail$source_note[detail$participant_id %in% "P14" & detail$statistic == "411"], "20 - Death")
})

test_that("merged enrollments: the triggering enrollment, plus the full list", {
  # Two overlapping enrollments in one FC: E1 starts Apr 1 and is discharged May 31; E2
  # starts Apr 15 and is discharged Jun 30. One episode: referred via E1, discharged via E2.
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
  expect_equal(episode$first_enrollment_id, "E1")
  expect_equal(episode$last_enrollment_id, "E2")
  expect_equal(episode$all_enrollment_ids, "E1, E2")
  expect_equal(episode$last_service_status_reason, "60 - Client preference")

  # a single enrollment has no list
  single <- fc_episodes(dplyr::filter(history, service_enrollment_id == "E1"), "fc", as.Date("2025-09-30"))
  expect_true(is.na(single$all_enrollment_ids))
})

test_that("visit detail points to the activity that was counted", {
  detail <- run_mis()$mis_detail_by_fc
  activities <- fixture_activities()

  # P1, May 5: face-to-face and non-face-to-face the same day. One visit (S450), from the
  # face-to-face activity; two interactions (S265), one row each.
  visit <- detail[detail$participant_id %in% "P1" & detail$date == as.Date("2025-05-05") & detail$statistic == "450", ]
  expect_equal(nrow(visit), 1)
  expect_equal(visit$source_note, "Face-to-face")
  expect_equal(activities$activity_type[activities$activity_id == visit$activity_id], "Face-to-face")
  interactions <- detail[detail$participant_id %in% "P1" & detail$date == as.Date("2025-05-05") & detail$statistic == "265", ]
  expect_equal(nrow(interactions), 2)
  expect_equal(sum(interactions$value), 2)
})

test_that("group detail: hours shared across participants, one row per session", {
  detail <- run_mis()$mis_detail_by_fc
  on_day <- detail[detail$funder_service_code == ps_fc & detail$date == as.Date("2025-06-02"), ]

  # S454: the 3-hour session is split between P5 and P6, so it totals 3 hours, not 6
  hours <- on_day[on_day$statistic == "454", ]
  expect_setequal(hours$participant_id, c("P5", "P6"))
  expect_equal(hours$value, c(1.5, 1.5))

  # S492: one session row, with the group and no client
  session <- on_day[on_day$statistic == "492", ]
  expect_equal(nrow(session), 1)
  expect_equal(session$activity_group_id, "G1")
  expect_true(is.na(session$participant_id))
})

test_that("organization detail: S501 and S511 rows in AC 82 9 90", {
  detail <- run_mis()$mis_detail_by_fc
  # R2: file closed Jun 9 (goals met), then a new admission Jul 7, both on enrollment E14
  r2 <- detail[detail$participant_id %in% "R2" & detail$funder_service_code == "82 9 90", ]
  closed <- r2[r2$statistic == "511", ]
  expect_equal(closed$date, as.Date("2025-06-09"))
  expect_equal(closed$source_note, "10 - Goals met")
  expect_equal(closed$service_enrollment_id, "E14")
  expect_true(as.Date("2025-07-07") %in% r2$date[r2$statistic == "501"])
})

# Census detail runs expanded back into one row per day, compared with the census table:
# counts per day for S403/S406, and the same client-days for S455, S855, S950 and S955
expect_runs_match_census <- function(out, grain) {
  census <- out[[paste0("mis_census_counts_by_", grain)]]
  keys <- c("date", grain_cols(grain), "funder_statistical_account_code")
  runs <- out[[paste0("mis_detail_by_", grain)]] |>
    dplyr::filter(statistic %in% substr(census$funder_statistical_account_code, 1, 3))
  days <- runs |>
    dplyr::mutate(date = purrr::map2(date, end_date, seq, by = "day")) |>
    tidyr::unnest(date)
  # a run's value is its number of days
  expect_equal(nrow(days), sum(runs$value))

  counted <- census |> dplyr::filter(!is.na(value))
  expect_equal(days |> dplyr::filter(statistic %in% c("403", "406")) |>
                 dplyr::count(dplyr::across(dplyr::all_of(keys)), name = "value") |>
                 dplyr::arrange(dplyr::across(dplyr::all_of(keys))),
               counted |> dplyr::select(dplyr::all_of(keys), value) |>
                 dplyr::arrange(dplyr::across(dplyr::all_of(keys))),
               ignore_attr = TRUE)

  served_keys <- c(keys, "participant_id")
  expect_equal(days |> dplyr::filter(!statistic %in% c("403", "406")) |>
                 dplyr::distinct(dplyr::across(dplyr::all_of(served_keys))) |>
                 dplyr::arrange(dplyr::across(dplyr::all_of(served_keys))),
               census |> dplyr::filter(is.na(value)) |>
                 dplyr::distinct(dplyr::across(dplyr::all_of(served_keys))) |>
                 dplyr::arrange(dplyr::across(dplyr::all_of(served_keys))),
               ignore_attr = TRUE)
}

test_that("census detail runs expand back to the census table, by FC and by service", {
  for (version in c("verA", "verB", "verC")) {
    out <- run_mis(fc_455_version = version)
    expect_runs_match_census(out, "fc")
    expect_runs_match_census(out, "service")
  }
})

test_that("census runs stop at month ends and between gaps", {
  rows <- tibble::tibble(
    funder_service_code = "72 5 40 76 10", funder_statistical_account_code = "403 45 40",
    participant_id = "X1",
    date = as.Date(c("2025-04-29", "2025-04-30", "2025-05-01", "2025-05-02", "2025-05-05"))
  )
  runs <- census_runs(rows, "fc") |> dplyr::arrange(date)
  expect_equal(runs$date, as.Date(c("2025-04-29", "2025-05-01", "2025-05-05")))
  expect_equal(runs$end_date, as.Date(c("2025-04-30", "2025-05-02", "2025-05-05")))
  expect_equal(runs$value, c(2L, 2L, 1L))
})
