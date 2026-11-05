test_that("unrecognised statuses and missing enrollment ids are dropped with a warning", {
  services <- tibble::tibble(
    service_enrollment_id = c("E1", "E1", "E1", NA, "E2"),
    service_status = c("On Waitlist", "Active ", "Discharged", "Active", NA)
  )

  warnings <- character()
  kept <- withCallingHandlers(
    drop_unusable_statuses(services),
    generateMIS_data_warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_equal(kept$service_status, "On Waitlist")
  expect_length(warnings, 3)
  expect_match(warnings[1], '"Active " (1), "Discharged" (1)', fixed = TRUE)
  expect_match(warnings[2], "no service_status")
  expect_match(warnings[3], "no service_enrollment_id")
})

test_that("clean statuses pass through without warnings", {
  services <- tibble::tibble(service_enrollment_id = c("E1", "E1"),
                             service_status = c("On Waitlist", "Active"))
  expect_no_warning(kept <- drop_unusable_statuses(services))
  expect_equal(kept, services)
})
