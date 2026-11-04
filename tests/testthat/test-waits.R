test_that("S407 99 10 counts days from referral to the service decision", {
  fc <- run_mis()$mis_visit_and_service_counts_by_fc
  # P1: Apr 1 to Apr 11, reported on the decision date
  expect_equal(value_on(fc, mh_fc, "407 99 10", "2025-04-11"), 10)
  # P3 has no decision yet, so P1 is the only wait counted
  expect_equal(value_total(fc, mh_fc, "407 99 10"), 10)
})

test_that("S407 25 20 counts waitlist days only, reported when service starts", {
  fc <- run_mis()$mis_visit_and_service_counts_by_fc
  # P1: waitlist Apr 11 to May 1
  expect_equal(value_on(fc, mh_fc, "407 25 20", "2025-05-01"), 20)
  # P2: waitlist Apr 1 to Apr 21; the 20 days on hold before May 11 don't count
  expect_equal(value_on(fc, mh_fc, "407 25 20", "2025-05-11"), 20)
  # P4 withdrew from the waitlist, so adds nothing
  expect_equal(value_total(fc, mh_fc, "407 25 20"), 40)
})

test_that("S406 counts people still waiting at the end of the period, from their second day", {
  census <- run_mis()$mis_census_counts_by_fc
  # P3 has been waiting for assessment since Sep 1
  expect_equal(value_on(census, mh_fc, "406 99 10", "2025-09-30"), 1)
  expect_equal(value_on(census, mh_fc, "406 99 10", "2025-09-01"), 0)
  # P4 on the waitlist from Jun 1 until withdrawing on Jul 1
  expect_equal(value_on(census, mh_fc, "406 25 20", "2025-06-15"), 1)
  expect_equal(value_on(census, mh_fc, "406 25 20", "2025-06-01"), 0)
  expect_equal(value_on(census, mh_fc, "406 25 20", "2025-09-30"), 0)
})

test_that("FC and service tables agree when each FC has one service", {
  out <- run_mis()
  strip <- function(x) {
    x |>
      dplyr::ungroup() |>
      dplyr::filter(!is.na(value)) |>
      dplyr::select(date, funder_service_code, funder_statistical_account_code, value) |>
      dplyr::arrange(date, funder_service_code, funder_statistical_account_code)
  }
  by_fc <- out$mis_visit_and_service_counts_by_fc |>
    # organization-level accounts only appear in the FC table
    dplyr::filter(!funder_service_code %in% c("82 9 90", "83 9 90"))
  expect_equal(strip(by_fc), strip(out$mis_visit_and_service_counts_by_service))
})
