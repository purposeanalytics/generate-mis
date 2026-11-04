test_that("S454 counts a group session's length once, not once per participant", {
  fc <- run_mis()$mis_visit_and_service_counts_by_fc
  # P5 + P6 in one 3-hour session
  expect_equal(value_on(fc, ps_fc, "454 80 40", "2025-06-02"), 3)
})

test_that("group sessions count one session and one attendance per participant", {
  fc <- run_mis()$mis_visit_and_service_counts_by_fc
  expect_equal(value_on(fc, ps_fc, "491 80 20", "2025-06-02"), 2)
  expect_equal(value_on(fc, ps_fc, "492 00 40", "2025-06-02"), 1)  # 2 to 5 hours
})

test_that("S455 needs enrollment and a visit, and a CSS assessment is not a visit (fc_455_version = 'verA')", {
  census <- run_mis()$mis_census_counts_by_fc
  expect_equal(people(census, mh_fc, "455 25 40"), c("P1", "P2", "P9"))
  # P7 only had an assessment in a service delivery FC
  expect_equal(people(census, ps_fc, "455 80 40"), c("P5", "P6"))
  # P8's assessment counts in case management
  expect_equal(people(census, cm_fc, "455 80 40"), "P8")
})

test_that("CMH&A S855 is counted separately by fund type", {
  census <- run_mis()$mis_census_counts_by_fc
  expect_equal(people(census, "82 9 90", "855 25 54"), c("P1", "P2", "P9"))
  expect_equal(people(census, "83 9 90", "855 25 54"), "P9")
  # CSS
  expect_equal(people(census, "82 9 90", "855 80 54"), c("P5", "P6", "P8"))
})
