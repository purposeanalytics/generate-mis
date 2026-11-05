# Activity rules that depend on the version arguments or on specific FCs

test_that("attendance days (S483) follow fc_450_version", {
  # P12: a 3-minute face-to-face attendance on Jun 3
  verA <- run_mis(fc_450_version = "verA")$mis_visit_and_service_counts_by_fc
  verB <- run_mis(fc_450_version = "verB")$mis_visit_and_service_counts_by_fc
  expect_equal(value_on(verA, day_fc, "483 25 41", "2025-06-03"), 0)
  expect_equal(value_on(verB, day_fc, "483 25 41", "2025-06-03"), 1)
})

test_that("transportation trips have no 5-minute rule in either fund type", {
  # P13: a 3-minute trip in 73 5 82 14, counted under fc_450_version = "verA"
  fc <- run_mis(fc_450_version = "verA")$mis_visit_and_service_counts_by_fc
  expect_equal(value_on(fc, tr3_fc, "450 80 40", "2025-06-04"), 1)
})

test_that("an activity recorded as an assessment outside CSS case management is not evidence of service", {
  # P12: "Assessment" on Jun 10 in a CMH&A FC; served on Jun 3 (verB, any length) only
  census <- run_mis(fc_450_version = "verB", fc_455_version = "verB")$mis_census_counts_by_fc
  p12 <- census[census$participant_id %in% "P12" & substr(census$funder_statistical_account_code, 1, 3) == "455", ]
  expect_equal(sort(unique(p12$date)), as.Date("2025-06-03"))
})
