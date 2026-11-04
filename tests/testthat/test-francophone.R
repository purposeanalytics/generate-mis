# S950 and S955 should give the same people under every fc_455_version. Language and special
# needs are only on the participant record in the fixtures, as in some client exports.
for (version in c("verA", "verB", "verC")) {
  test_that(paste("S950 counts a French-speaking client in the FC, service and organization,", version), {
    out <- run_mis(fc_455_version = version)
    by_fc <- out$mis_census_counts_by_fc
    by_service <- out$mis_census_counts_by_service
    # P11's only visit is by phone, which counts as service like any other visit
    expect_equal(people(by_fc, ps_fc, "950 80 10"), "P11")
    expect_equal(people(by_service, ps_fc, "950 80 10"), "P11")
    expect_equal(people(by_fc, "82 9 90", "950 10 80"), "P11")
  })

  test_that(paste("S955 counts a client by special need in the FC and service,", version), {
    out <- run_mis(fc_455_version = version)
    expect_equal(people(out$mis_census_counts_by_fc, ps_fc, "955 80 10"), "P11")
    expect_equal(people(out$mis_census_counts_by_service, ps_fc, "955 80 10"), "P11")
  })
}
