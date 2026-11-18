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

test_that("the visit and service summaries hold only sum_visits_and_services statistics", {
  out <- run_mis()
  stats <- function(tbl) unique(substr(tbl$funder_statistical_account_code, 1, 3))
  type <- function(stats) statistic_aggregation$aggregation[match(stats, statistic_aggregation$statistic)]
  visits <- c(stats(out$mis_visit_and_service_counts_by_fc), stats(out$mis_visit_and_service_counts_by_service))
  expect_true(all(type(visits) == "sum_visits_and_services"))
  # the detail tables hold the rest: the census statistics
  census <- setdiff(c(stats(out$mis_detail_by_fc), stats(out$mis_detail_by_service)), visits)
  expect_true(all(type(census) %in% c("sum_census", "end_of_period", "distinct_cumulative")))
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
