test_that("S489 counts each acceptance once, on the decision date", {
  fc <- run_mis()$mis_visit_and_service_counts_by_fc
  # P1's waitlist and active rows are one acceptance
  expect_equal(value_on(fc, mh_fc, "489 25 40", "2025-04-11"), 1)
  # P2 and P9 accepted on Apr 1
  expect_equal(value_on(fc, mh_fc, "489 25 40", "2025-04-01"), 2)
})

test_that("S506 counts first service on the first active day (fc_506_version = 'verA')", {
  fc <- run_mis()$mis_visit_and_service_counts_by_fc
  expect_equal(value_on(fc, mh_fc, "506 25 40", "2025-05-01"), 1)  # P1
  expect_equal(value_on(fc, mh_fc, "506 25 40", "2025-05-11"), 1)  # P2, after the hold
})

test_that("S513 is counted when the FC enrollment ends, only if service started", {
  fc <- run_mis()$mis_visit_and_service_counts_by_fc
  expect_equal(value_on(fc, mh_fc, "513 25 90", "2025-08-15"), 1)  # P1
  # P4 withdrew from the waitlist before service started: no S506, so no S513
  expect_equal(value_on(fc, mh_fc, "513 25 90", "2025-07-01"), 0)
  expect_equal(value_total(fc, mh_fc, "513 25 90"), 1)
})

test_that("S501 and S511 pair up at the organization level", {
  fc <- run_mis()$mis_visit_and_service_counts_by_fc
  # P1 admitted on the decision date and file closed at discharge
  expect_equal(value_on(fc, "82 9 90", "501 25 40", "2025-04-11"), 1)
  expect_equal(value_on(fc, "82 9 90", "511 25 90", "2025-08-15"), 1)
  # P9 has enrollments in two CMH&A FCs: one admission, still open
  # (P2 is the other Apr 1 admission)
  expect_equal(value_on(fc, "82 9 90", "501 25 40", "2025-04-01"), 2)
})
