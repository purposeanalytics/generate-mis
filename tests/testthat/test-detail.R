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
  detail <- run_mis()$mis_detail_by_fc
  expect_setequal(unique(detail$statistic), c("410", "411", "489", "506", "513"))
  expect_true(all(detail$aggregation == "sum_visits_and_services"))

  # R3: back on the waitlist Jun 23, so discharged that day (S513) and referred again (S489)
  r3 <- detail[detail$participant_id == "R3", ]
  discharge <- r3[r3$statistic == "513", ]
  expect_equal(discharge$date, as.Date("2025-06-23"))
  expect_equal(discharge$service_enrollment_id, "E15")
  expect_equal(discharge$source_date, as.Date("2025-04-21"))  # the episode began Apr 21
  expect_equal(sort(r3$date[r3$statistic == "489"]), as.Date(c("2025-04-21", "2025-06-23")))

  # P14: the S411 death carries the discharge reason
  expect_equal(detail$source_note[detail$participant_id == "P14" & detail$statistic == "411"], "20 - Death")
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
