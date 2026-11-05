# statistic_aggregation: how each statistic rolls up over a period (R/statistic_aggregation.R)

test_that("every statistic in the output has one aggregation type", {
  out <- run_mis()
  produced <- unique(substr(unlist(lapply(out, function(t) t$funder_statistical_account_code)), 1, 3))
  expect_true(all(produced %in% statistic_aggregation$statistic),
              info = paste("missing:", paste(setdiff(produced, statistic_aggregation$statistic), collapse = ", ")))
  expect_false(anyDuplicated(statistic_aggregation$statistic) > 0)
  expect_true(all(statistic_aggregation$aggregation %in%
                    c("sum_visits_and_services", "sum_census", "end_of_period", "distinct_cumulative")))
})

test_that("the aggregation type matches the file each statistic is written to", {
  out <- run_mis()
  census <- unique(substr(c(out$mis_census_counts_by_fc$funder_statistical_account_code,
                            out$mis_census_counts_by_service$funder_statistical_account_code), 1, 3))
  visits <- unique(substr(c(out$mis_visit_and_service_counts_by_fc$funder_statistical_account_code,
                            out$mis_visit_and_service_counts_by_service$funder_statistical_account_code), 1, 3))
  type <- function(stats) statistic_aggregation$aggregation[match(stats, statistic_aggregation$statistic)]
  expect_true(all(type(census) %in% c("sum_census", "end_of_period", "distinct_cumulative")))
  expect_true(all(type(visits) == "sum_visits_and_services"))
})

test_that("service_list.csv carries the aggregation type", {
  service_list <- readr::read_csv(file.path(attr(run_mis(), "data_folder"), "processed", "service_list.csv"),
                                  show_col_types = FALSE)
  agg <- function(account) unique(service_list$aggregation[startsWith(service_list$funder_statistical_account_code, account)])
  expect_equal(agg("406"), "end_of_period")
  expect_equal(agg("403"), "sum_census")
  expect_equal(agg("455"), "distinct_cumulative")
  expect_equal(agg("450"), "sum_visits_and_services")
})
