# P3 has a visit but is not enrolled (waiting for assessment).
# P7 is enrolled in a CSS FC but has no visit (only an assessment, which doesn't count there).

test_that("verA: served needs both an Active enrolment and a visit", {
  census <- run_mis(fc_455_version = "verA")$mis_census_counts_by_fc
  expect_false("P3" %in% people(census, mh_fc, "455 25 40"))
  expect_false("P3" %in% people(census, "82 9 90", "855 25 54"))
  expect_false("P7" %in% people(census, ps_fc, "455 80 40"))
  expect_false("P7" %in% people(census, "82 9 90", "855 80 54"))
})

test_that("verB: served is based on visits alone", {
  out <- run_mis(fc_455_version = "verB")
  census <- out$mis_census_counts_by_fc
  expect_true("P3" %in% people(census, mh_fc, "455 25 40"))
  expect_true("P3" %in% people(census, "82 9 90", "855 25 54"))
  expect_false("P7" %in% people(census, ps_fc, "455 80 40"))
  expect_false("P7" %in% people(census, "82 9 90", "855 80 54"))
  # same answer by service
  expect_true("P3" %in% people(out$mis_census_counts_by_service, mh_fc, "455 25 40"))
})

test_that("verC: served is based on Active enrolment alone", {
  census <- run_mis(fc_455_version = "verC")$mis_census_counts_by_fc
  expect_false("P3" %in% people(census, mh_fc, "455 25 40"))
  expect_false("P3" %in% people(census, "82 9 90", "855 25 54"))
  expect_true("P7" %in% people(census, ps_fc, "455 80 40"))
  expect_true("P7" %in% people(census, "82 9 90", "855 80 54"))
})
