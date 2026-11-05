test_that("a missing required column stops the run and names the column", {
  expect_error(check_input_columns(dplyr::select(fixture_activities(), -activity_date), "activities"),
               "activities.rds is missing required column(s): activity_date", fixed = TRUE)
})

test_that("a missing expected column warns and is filled with blanks", {
  participants <- dplyr::select(fixture_participants(), -participant_preferred_language)
  expect_warning(out <- check_input_columns(participants, "participants"),
                 "so S950 will be undercounted", fixed = TRUE)
  expect_true(all(is.na(out$participant_preferred_language)))
})

test_that("special needs and language come from participants, not copies on activities", {
  data_folder <- file.path(withr::local_tempdir(), "data")
  dir.create(file.path(data_folder, "interim", "rds"), recursive = TRUE)
  dir.create(file.path(data_folder, "processed"), recursive = TRUE)
  # a stale copy on every activity row that disagrees with the participant record
  activities <- fixture_activities() |>
    dplyr::mutate(participant_preferred_language = "English", participant_ohrs_special_needs = "High Risk Senior")
  readr::write_rds(fixture_participants(), file.path(data_folder, "interim/rds/participants.rds"))
  readr::write_rds(fixture_services(),     file.path(data_folder, "interim/rds/services.rds"))
  readr::write_rds(activities,             file.path(data_folder, "interim/rds/activities.rds"))
  readr::write_rds(fixture_fc_mapping(),   file.path(data_folder, "interim/rds/functional_centre_mapping.rds"))

  withr::local_envvar(LOG_LEVEL = "ERROR")
  visits <- suppressWarnings(process_data(data_folder, report_date = as.Date("2025-09-30")))$mis_visits
  p11 <- visits[visits$participant_id == "P11", ]
  expect_equal(unique(p11$participant_preferred_language), "French")
  expect_equal(unique(p11$participant_ohrs_special_needs), "Physical Disability")
  expect_true(all(is.na(visits$participant_ohrs_special_needs[visits$participant_id != "P11"])))
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
