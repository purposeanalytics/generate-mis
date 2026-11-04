test_that("S450/S451 count one visit per person per day, preferring face-to-face", {
  fc <- run_mis()$mis_visit_and_service_counts_by_fc
  # P1 on May 5: face-to-face and non-face-to-face = 1 face-to-face visit
  expect_equal(value_on(fc, mh_fc, "450 25 40", "2025-05-05"), 1)
  expect_equal(value_on(fc, mh_fc, "451 25 40", "2025-05-05"), 0)
  # P1 on May 19: non-face-to-face visit
  expect_equal(value_on(fc, mh_fc, "451 25 40", "2025-05-19"), 1)
})

test_that("activities of 5 minutes or less don't count as visits (fc_450_version = 'verA')", {
  fc <- run_mis()$mis_visit_and_service_counts_by_fc
  # P1 on May 12: 3 minutes
  expect_equal(value_on(fc, mh_fc, "451 25 40", "2025-05-12"), 0)
  # nothing else in P1's FC that day either (no interaction, no hours)
  expect_equal(sum(fc$value[fc$date == as.Date("2025-05-12") & fc$funder_service_code == mh_fc]), 0)
})

test_that("S265 counts each service provider interaction by time interval", {
  fc <- run_mis()$mis_visit_and_service_counts_by_fc
  # P1 on May 5: one 60-minute and one 20-minute interaction
  expect_equal(value_on(fc, mh_fc, "265 25 02", "2025-05-05"), 1)
  expect_equal(value_on(fc, mh_fc, "265 25 01", "2025-05-05"), 1)
})

test_that("assessments count as visits only in CSS case management", {
  fc <- run_mis()$mis_visit_and_service_counts_by_fc
  expect_equal(value_on(fc, cm_fc, "450 80 40", "2025-07-07"), 1)  # P8
  expect_equal(sum(fc$value[fc$funder_service_code == ps_fc & fc$date == as.Date("2025-07-07")]), 0)  # P7
})
