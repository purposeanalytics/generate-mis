# Detail tables: one row per counted unit, and the summary tables are their sums.

# Detail summed back to date, reporting unit and account, compared with the summary rows for
# the same statistics
expect_detail_matches_summary <- function(out, grain) {
  detail <- out[[paste0("mis_detail_by_", grain)]]
  summary <- out[[paste0("mis_visit_and_service_counts_by_", grain)]]
  keys <- c("date", grain_cols(grain), "funder_statistical_account_code")

  from_detail <- detail |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::summarize(value = sum(value), .groups = "drop") |>
    dplyr::arrange(dplyr::across(dplyr::all_of(keys)))
  from_summary <- summary |>
    dplyr::filter(substr(funder_statistical_account_code, 1, 3) %in% unique(detail$statistic)) |>
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
  # every statistic in the visit and service table has detail rows
  expect_setequal(unique(detail$statistic),
                  unique(substr(out$mis_visit_and_service_counts_by_fc$funder_statistical_account_code, 1, 3)))
  expect_true(all(detail$aggregation == "sum_visits_and_services"))

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
