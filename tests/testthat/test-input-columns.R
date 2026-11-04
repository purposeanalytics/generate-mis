test_that("a missing required column stops the run and names the column", {
  expect_error(check_input_columns(dplyr::select(fixture_activities(), -activity_date), "activities"),
               "activities.rds is missing required column(s): activity_date", fixed = TRUE)
})

test_that("a missing expected column warns and is filled with blanks", {
  activities <- dplyr::select(fixture_activities(), -participant_preferred_language)
  expect_warning(out <- check_input_columns(activities, "activities"),
                 "so S950 (fc_455_version verB) will be undercounted", fixed = TRUE)
  expect_true(all(is.na(out$participant_preferred_language)))
})

test_that("a missing date column is filled with a blank date", {
  services <- dplyr::select(fixture_services(), -service_first_direct_activity)
  out <- suppressWarnings(check_input_columns(services, "services"))
  expect_s3_class(out$service_first_direct_activity, "Date")
})

test_that("complete inputs pass without warnings", {
  expect_no_warning(check_input_columns(fixture_participants(), "participants"))
  expect_no_warning(check_input_columns(fixture_services(), "services"))
  expect_no_warning(check_input_columns(fixture_activities(), "activities"))
  expect_no_warning(check_input_columns(fixture_fc_mapping(), "functional_centre_mapping"))
})
