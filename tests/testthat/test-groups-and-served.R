test_that("S454 counts a group session's length once, not once per participant", {
  fc <- run_mis()$mis_visit_and_service_counts_by_fc
  # P5 + P6 in one 3-hour session
  expect_equal(value_on(fc, ps_fc, "454 80 40", "2025-06-02"), 3)
})

test_that("group sessions count one session and one attendance per participant", {
  fc <- run_mis()$mis_visit_and_service_counts_by_fc
  expect_equal(value_on(fc, ps_fc, "491 80 20", "2025-06-02"), 2)
  expect_equal(value_on(fc, ps_fc, "492 00 40", "2025-06-02"), 1)  # 2 to 5 hours
})

test_that("S455 needs enrollment and a visit, and a CSS assessment is not a visit (fc_455_version = 'verA')", {
  census <- run_mis()$mis_detail_by_fc
  expect_equal(people(census, mh_fc, "455 25 40"), c("P1", "P2", "P9"))
  # P7 only had an assessment in a service delivery FC
  expect_equal(people(census, ps_fc, "455 80 40"), c("P11", "P5", "P6"))
  # P8's assessment counts in case management
  expect_equal(people(census, cm_fc, "455 80 40"), "P8")
})

test_that("CMH&A S855 is counted separately by fund type", {
  census <- run_mis()$mis_detail_by_fc
  expect_equal(people(census, "82 9 90", "855 25 54"), c("P1", "P2", "P9"))
  expect_equal(people(census, "83 9 90", "855 25 54"), "P9")
  # CSS
  # CSS: one count for the organization across fund types (P13 is in fund type 3); P14-P16
  # are assisted living residents, served without recorded visits
  expect_equal(people(census, "82 9 90", "855 80 54"), c("P11", "P13", "P14", "P15", "P16", "P5", "P6", "P8"))
})

test_that("residents are served in a residential FC without a recorded visit, under every version", {
  # P14, P15 and P16 live in Assisted Living (a residential FC that reports S403) and have no
  # visits recorded there. A resident day is service (OHRS Ch. 10, Assisted Living and Day
  # Services; Ch. 7, intake example step 8), so they are served whatever fc_455_version is.
  for (version in c("verA", "verB", "verC")) {
    detail <- run_mis(fc_455_version = version, fc_506_version = matching_506(version))$mis_detail_by_fc
    stat <- substr(detail$funder_statistical_account_code, 1, 3)
    al <- detail$funder_service_code == al_fc

    residents <- unique(detail$participant_id[al & stat == "403"])
    expect_true(all(c("P14", "P15", "P16") %in% residents), info = version)
    # every resident is served in the FC, from their first resident day, and by the organization
    expect_true(all(residents %in% detail$participant_id[al & stat == "455"]), info = version)
    expect_equal(min(detail$date[al & stat == "455" & detail$participant_id %in% "P14"]), as.Date("2025-04-22"),
                 info = version)
    expect_true(all(residents %in% detail$participant_id[stat == "855"]), info = version)
    # with no visit, the reference is the enrollment
    expect_equal(unique(detail$reference_id[al & stat == "455" & detail$participant_id %in% "P14"]), "E21",
                 info = version)
  }
})

test_that("a CMH&A client who was also a resident is counted once at the organization level, as SR 45", {
  for (version in c("verA", "verB", "verC")) {
    out <- run_mis(fc_455_version = version, fc_506_version = matching_506(version))
    org <- out$mis_detail_by_fc
    org <- org[org$funder_service_code == "82 9 90" & org$participant_id == "P17", ]

    # one organization file (the stays overlap), one admission and one file closure
    expect_equal(sort(unique(org$funder_statistical_account_code)),
                 c("501 45 40", "511 45 90", "855 45 54"), info = version)
    expect_equal(sum(org$funder_statistical_account_code == "501 45 40"), 1, info = version)
    expect_equal(org$reference_id[org$funder_statistical_account_code == "501 45 40"], "E26, E27", info = version)
    # every served day, including case management before the stay, is SR 45 (verC counts
    # from the enrollment on Apr 1, the others from the visit on Apr 15)
    first_served <- if (version == "verC") "2025-04-01" else "2025-04-15"
    expect_equal(min(org$date[org$funder_statistical_account_code == "855 45 54"]), as.Date(first_served), info = version)
  }
})
