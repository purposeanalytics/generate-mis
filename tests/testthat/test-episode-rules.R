# Episode rules (see number_episodes()), using clients R1-R6 in the ACT team FC (act_fc).
# All are CMH&A adults, so accounts are 489 25 40, 506 25 40, 407 25 20, 513 25 90, 406 25 20.

test_that("service that pauses and resumes is one episode: one referral, one first service", {
  fc <- run_mis()$mis_visit_and_service_counts_by_fc
  # R1: Active Apr 7, On Hold May 5, Active again Jun 16
  expect_equal(value_on(fc, act_fc, "489 25 40", "2025-04-07"), 1)
  expect_equal(value_on(fc, act_fc, "506 25 40", "2025-04-07"), 1)
  expect_equal(value_on(fc, act_fc, "506 25 40", "2025-06-16"), 0)
})

test_that("a file reopened after Inactive is a new episode with its own referral, wait, first service and discharge", {
  fc <- run_mis()$mis_visit_and_service_counts_by_fc
  # R2: Waitlist Apr 14, Active May 12, Inactive Jun 9, Waitlist Jul 7, Active Aug 4.
  #   Only the Apr 14 decision is recorded; the reopened episode is referred on Jul 7.
  expect_equal(value_on(fc, act_fc, "489 25 40", "2025-04-14"), 1)
  expect_equal(value_on(fc, act_fc, "489 25 40", "2025-07-07"), 1)
  expect_equal(value_on(fc, act_fc, "506 25 40", "2025-05-12"), 1)
  expect_equal(value_on(fc, act_fc, "506 25 40", "2025-08-04"), 1)
  expect_equal(value_on(fc, act_fc, "407 25 20", "2025-05-12"), 28)
  expect_equal(value_on(fc, act_fc, "407 25 20", "2025-08-04"), 28)
  expect_equal(value_on(fc, act_fc, "513 25 90", "2025-06-09"), 1)
  # and at the organization: file closed, then a new admission
  expect_equal(value_on(fc, "82 9 90", "511 25 90", "2025-06-09"), 1)
  expect_equal(value_on(fc, "82 9 90", "501 25 40", "2025-07-07"), 1)
})

test_that("a return to the waitlist after service started is a discharge and a new referral", {
  out <- run_mis()
  fc <- out$mis_visit_and_service_counts_by_fc
  census <- out$mis_detail_by_fc
  # R3: Waitlist Apr 21, Active May 19, Waitlist Jun 23, Active Jul 21 (OHRS Ch. 10, 10.9.4.4 ex. 2)
  expect_equal(value_on(fc, act_fc, "489 25 40", "2025-04-21"), 1)
  expect_equal(value_on(fc, act_fc, "506 25 40", "2025-05-19"), 1)
  expect_equal(value_on(fc, act_fc, "407 25 20", "2025-05-19"), 28)
  # discharged and referred again on Jun 23 (no new decision date: the return to the
  # waitlist is the referral date), then a second wait and first service
  expect_equal(value_on(fc, act_fc, "513 25 90", "2025-06-23"), 1)
  expect_equal(value_on(fc, act_fc, "489 25 40", "2025-06-23"), 1)
  expect_equal(value_on(fc, act_fc, "506 25 40", "2025-07-21"), 1)
  expect_equal(value_on(fc, act_fc, "407 25 20", "2025-07-21"), 28)
  expect_equal(value_total(fc, act_fc, "489 25 40"), 7)  # R1, R2 twice, R3 twice, R5, R6 (not R4)
  expect_equal(value_total(fc, act_fc, "513 25 90"), 2)  # R2, R3
  # still the organization's client: no file closed or new admission
  expect_equal(value_on(fc, "82 9 90", "511 25 90", "2025-06-23"), 0)
  expect_equal(value_on(fc, "82 9 90", "501 25 40", "2025-06-23"), 0)
  # counted as waiting again: R3 on Jun 30; R2 and R3 on Jul 10
  expect_equal(value_on(census, act_fc, "406 25 20", "2025-06-30"), 1)
  expect_equal(value_on(census, act_fc, "406 25 20", "2025-07-10"), 2)
})

test_that("a referral (S489) needs an eligibility decision date, and a missing one is warned about", {
  out <- run_mis()
  fc <- out$mis_visit_and_service_counts_by_fc
  expect_equal(attr(out, "warnings"),
               paste("1 episode(s) were moved to Waitlist, Hold or Active with no service_decision_date,",
                     "so they have no referral (S489) but can still have a first service (S506/S401).",
                     "Correct the source data. Enrollment(s): E16"))
  # R4: Waitlist Apr 28 and Active May 26, with no decision date
  expect_equal(value_on(fc, act_fc, "489 25 40", "2025-04-28"), 0)
  expect_equal(value_on(fc, act_fc, "506 25 40", "2025-05-26"), 1)
})

test_that("an Active corrected the same day is dropped; a same-day start is kept", {
  fc <- run_mis()$mis_visit_and_service_counts_by_fc
  # R5: Waitlist Apr 2, Active May 5 (ended May 5), Waitlist May 5 to Jun 2, Active Jun 2
  expect_equal(value_on(fc, act_fc, "489 25 40", "2025-04-02"), 1)
  expect_equal(value_on(fc, act_fc, "506 25 40", "2025-05-05"), 0)
  expect_equal(value_on(fc, act_fc, "506 25 40", "2025-06-02"), 1)
  expect_equal(value_on(fc, act_fc, "407 25 20", "2025-06-02"), 61)  # Apr 2 to Jun 2
  # R6: Waitlist and Active both on Apr 8; the waitlist ended that day
  expect_equal(value_on(fc, act_fc, "506 25 40", "2025-04-08"), 1)
})
