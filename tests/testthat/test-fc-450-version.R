test_that("verA: a contact of 5 minutes or less is neither a visit nor evidence of service", {
  out <- run_mis(fc_450_version = "verA")
  fc <- out$mis_visit_and_service_counts_by_fc
  census <- out$mis_census_counts_by_fc
  # P10's only contact is a 3-minute call on Aug 5
  expect_equal(value_on(fc, mh_fc, "451 25 40", "2025-08-05"), 0)
  expect_false("P10" %in% people(census, mh_fc, "455 25 40"))
  expect_false("P10" %in% people(census, "82 9 90", "855 25 54"))
})

test_that("verB: every contact is a visit and evidence of service", {
  out <- run_mis(fc_450_version = "verB")
  fc <- out$mis_visit_and_service_counts_by_fc
  census <- out$mis_census_counts_by_fc
  expect_equal(value_on(fc, mh_fc, "451 25 40", "2025-08-05"), 1)
  expect_true("P10" %in% people(census, mh_fc, "455 25 40"))
  expect_true("P10" %in% people(census, "82 9 90", "855 25 54"))
  # P1's 3-minute call on May 12 also counts, and P1 was already served
  expect_equal(value_on(fc, mh_fc, "451 25 40", "2025-05-12"), 1)
})

test_that("an unknown fc_450_version is an error, not a silent verB", {
  expect_error(apply_fc_450_version(fixture_activities(), "verC"))
})
