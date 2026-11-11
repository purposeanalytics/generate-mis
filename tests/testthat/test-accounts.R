# Building and checking statistical account codes

test_that("default_sr_category() gives the FC's usual SR category", {
  expect_equal(default_sr_category(c("72 5 40 76 10", mh_fc, mh3_fc, "72 5 08", ps_fc)),
               c("45", "25", "25", "25", "80"))
})

test_that("add_statistical_account() builds the account from its parts", {
  rows <- tibble::tibble(funder_service_code = c(mh_fc, ps_fc), participant_funder_age_group_code = c("40", "20"))
  out <- add_statistical_account(rows, statistic = "506",
                                 sr_category = default_sr_category(funder_service_code),
                                 detail = participant_funder_age_group_code)
  expect_equal(out$funder_statistical_account_code, c("506 25 40", "506 80 20"))
  # no helper columns are left behind
  expect_equal(colnames(out), c(colnames(rows), "funder_statistical_account_code"))
})

test_that("an account part with the wrong number of digits stops the run", {
  rows <- tibble::tibble(x = 1)
  expect_error(add_statistical_account(rows, statistic = "45", sr_category = "25", detail = "40"),
               "statistic must be 3 digits; got 45", fixed = TRUE)
})

test_that("a blank account part is kept with a data warning", {
  rows <- tibble::tibble(meal = c("20", NA))
  expect_warning(out <- add_statistical_account(rows, statistic = "248", sr_category = "80", detail = meal),
                 "(248 80 NA)", fixed = TRUE, class = "generateMIS_data_warning")
  expect_equal(out$funder_statistical_account_code, c("248 80 20", "248 80 NA"))
})

test_that("check_accounts() warns about pairs that aren't in the OHRS code table", {
  output <- tibble::tibble(funder_service_code = c(ps_fc, "83 9 90"),
                           funder_statistical_account_code = c("999 80 40", "855 25 54"))
  # 83 9 90 is checked against 82 9 90, so only the made-up account is named
  expect_warning(check_accounts(output, "test table"),
                 "1 FC/account pair(s) in test table aren't in the OHRS code table: 72 5 82 33: 999 80 40.",
                 fixed = TRUE, class = "generateMIS_data_warning")
})

test_that("the fixtures produce only accounts in the OHRS code table", {
  out <- run_mis()
  expect_false(any(grepl("aren't in the OHRS code table", attr(out, "warnings"), fixed = TRUE)))
})

test_that("a discharge reason that isn't an account for the FC is reported as 90, with a warning", {
  # 13 isn't an OHRS discharge reason; 10 (goals met) is
  rows <- tibble::tibble(funder_service_code = c("72 5 82 33", "72 5 82 33", "82 9 90", "83 9 90"),
                         funder_statistical_account_code = c("513 80 10", "513 80 13", "511 80 13", "511 80 10"))

  expect_warning(out <- fix_unknown_reasons(rows, "513"), "513 80 13 in 72 5 82 33", class = "generateMIS_data_warning")
  expect_equal(out$funder_statistical_account_code, c("513 80 10", "513 80 90", "511 80 13", "511 80 10"))

  # S511 in the organizational AC; 83 9 90 is checked as 82 9 90
  expect_warning(out <- fix_unknown_reasons(rows, "511"), "511 80 13 in 82 9 90 (1)", fixed = TRUE, class = "generateMIS_data_warning")
  expect_equal(out$funder_statistical_account_code[3:4], c("511 80 90", "511 80 10"))

  # nothing to fix, no warning
  expect_no_warning(fix_unknown_reasons(rows[1, ], "513"))
})
