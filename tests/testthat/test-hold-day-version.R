# hold_day_version: whether the day a client goes on hold is counted as Active.
# P16 (assisted living) is Active from Apr 1, On Hold May 1-10, and Active again from May 11.
# P14 and P15 are assisted living clients discharged on Jun 17 and Jun 24.

census <- function(version) {
  run_mis(hold_day_version = version)$mis_census_counts_by_fc
}

test_that("verA does not count the day a client goes on hold", {
  # P14 and P15 are resident on May 1; P16 is not
  expect_equal(value_on(census("verA"), al_fc, "403 80 40", "2025-05-01"), 2)
  # Apr 30, the day before the hold, is counted for all three
  expect_equal(value_on(census("verA"), al_fc, "403 80 40", "2025-04-30"), 3)
})

test_that("verB counts the day a client goes on hold, and only that day", {
  expect_equal(value_on(census("verB"), al_fc, "403 80 40", "2025-05-01"), 3)
  # the rest of the hold is not counted
  expect_equal(value_on(census("verB"), al_fc, "403 80 40", "2025-05-02"), 2)
  expect_equal(value_on(census("verB"), al_fc, "403 80 40", "2025-05-10"), 2)
  # the return day is counted under both versions
  expect_equal(value_on(census("verB"), al_fc, "403 80 40", "2025-05-11"), 3)
  expect_equal(value_on(census("verA"), al_fc, "403 80 40", "2025-05-11"), 3)
  # one hold, so one more resident day in total
  expect_equal(value_total(census("verB"), al_fc, "403 80 40") -
                 value_total(census("verA"), al_fc, "403 80 40"), 1)
})

test_that("the day of discharge is not counted under either version", {
  for (version in c("verA", "verB")) {
    # P14 died Jun 17 and P15 moved out Jun 24: neither day is a resident day
    expect_equal(value_on(census(version), al_fc, "403 80 40", "2025-06-16"), 3)
    expect_equal(value_on(census(version), al_fc, "403 80 40", "2025-06-17"), 2)
    expect_equal(value_on(census(version), al_fc, "403 80 40", "2025-06-24"), 1)
  }
})

test_that("hold_day_version rejects unknown values", {
  expect_error(run_mis(hold_day_version = "verZ"))
})
