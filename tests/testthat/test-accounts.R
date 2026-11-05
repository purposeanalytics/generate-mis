# Building and checking statistical account codes

test_that("default_sr_category() gives the FC's usual SR category", {
  expect_equal(default_sr_category(c("72 5 40 76 10", mh_fc, mh3_fc, "72 5 08", ps_fc)),
               c("45", "25", "25", "25", "80"))
})

test_that("an account part with the wrong number of digits stops the run", {
  parts <- tibble::tibble(digits_1_3 = "45", digits_4_5 = "25", digits_6_7 = "40")
  expect_error(assemble_statistical_account(parts), "digits_1_3 must be 3 digits; got 45", fixed = TRUE)
})

test_that("a blank account part is kept with a data warning, and the helper columns are dropped", {
  parts <- tibble::tibble(digits_1_3 = "248", digits_4_5 = "80", digits_6_7 = c("20", NA))
  expect_warning(out <- assemble_statistical_account(parts), "(248 80 NA)", fixed = TRUE,
                 class = "generateMIS_data_warning")
  expect_equal(out$funder_statistical_account_code, c("248 80 20", "248 80 NA"))
  expect_equal(colnames(out), "funder_statistical_account_code")
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
