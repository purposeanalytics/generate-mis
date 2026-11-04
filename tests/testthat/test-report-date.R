test_that("discharges after report_date are not counted", {
  # As at Aug 1, P1 (discharged Aug 15) is still open
  fc <- run_mis(report_date = "2025-08-01")$mis_visit_and_service_counts_by_fc
  expect_equal(value_total(fc, mh_fc, "513 25 90"), 0)
  expect_equal(value_on(fc, "82 9 90", "511 25 90", "2025-08-15"), 0)
})

test_that("records dated after report_date are ignored", {
  # As at Apr 30, P1 is still on the waitlist: in S406, and service hasn't started,
  # even though the data shows it starting on May 1
  out <- run_mis(report_date = "2025-04-30")
  fc <- out$mis_visit_and_service_counts_by_fc
  expect_equal(value_on(out$mis_census_counts_by_fc, mh_fc, "406 25 20", "2025-04-30"), 1)
  expect_equal(value_total(fc, mh_fc, "407 25 20"), 0)
  expect_false(any(fc$date > as.Date("2025-04-30")))
  # P4's acceptance on Jun 1 hasn't happened yet
  expect_equal(value_on(fc, mh_fc, "489 25 40", "2025-06-01"), 0)
})

test_that("activities after report_date are ignored", {
  # As at May 10, only P1's May 5 visit has happened
  fc <- run_mis(report_date = "2025-05-10")$mis_visit_and_service_counts_by_fc
  expect_equal(value_total(fc, mh_fc, "450 25 40"), 1)
  expect_equal(value_total(fc, mh_fc, "451 25 40"), 0)
})
