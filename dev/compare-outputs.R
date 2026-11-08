# Check that a code change leaves the output unchanged.
#
# Builds a reproducible random dataset (made-up people, no client data) covering 11 FCs with
# two services each, including residential, CSS residential, meals, transportation, adult
# day, I&R, unregistered contacts, group sessions and assessments. Runs generate_tables()
# for all 24 combinations of fc_450_version, fc_455_version, fc_506_version and ir_version,
# then saves the four output tables from each run, or compares them with a saved copy.
#
# Run from the repo root (takes a few minutes):
#   Rscript dev/compare-outputs.R save      # before the change: save a baseline
#   Rscript dev/compare-outputs.R compare   # after the change: compare with the baseline
# An optional second argument sets the baseline file (default dev/baseline.rds, not committed).
# `compare` lists each statistic whose rows differ and exits with status 1.
#
# Use it for refactors, where the output should not change. For an intended rule change,
# expect differences in the statistics you meant to change, and nowhere else.

args <- commandArgs(trailingOnly = TRUE)
mode <- match.arg(args[1], c("save", "compare"))
out_path <- if (length(args) >= 2) args[2] else "dev/baseline.rds"
if (!file.exists("DESCRIPTION")) stop("Run this from the repo root.", call. = FALSE)

suppressMessages(devtools::load_all(".", quiet = TRUE))
Sys.setenv(LOG_LEVEL = "ERROR")
set.seed(20261104)

fcs <- c("72 5 09 76", "73 5 09 76", "72 5 40 76 10", "72 5 82 33", "72 5 82 09", "72 5 82 10",
         "72 5 82 14", "72 5 82 45", "72 5 82 20", "72 5 70 10", "72 5 08")
services <- tidyr::expand_grid(funder_service_code = fcs, k = 1:2) |>
  dplyr::mutate(service_name = paste0("Svc ", funder_service_code, " #", k))

n <- 400
participants <- tibble::tibble(
  participant_id = paste0("P", seq_len(n)),
  participant_date_of_birth = as.Date("1930-01-01") + sample(0:30000, n, TRUE),
  participant_preferred_language = sample(c("English", "French", NA), n, TRUE, c(.7, .25, .05)),
  participant_ohrs_special_needs = sample(c(NA, "Physical Disability", "Cognitive Impairment", "Frail And-or Elderly",
                                            "High Risk Senior", "Living With Effects Of HIV-AIDS"), n, TRUE, c(.5, rep(.1, 5)))
)
age_code <- function(dob, on) {
  a <- as.numeric(on - dob) / 365.25
  dplyr::case_when(a < 18 ~ "60", a < 65 ~ "40", TRUE ~ "20")
}
reasons <- c("10 - Goals met", "20 - Moved", "30 - Deceased", "Withdrew", NA)

svc_rows <- list(); act_rows <- list(); e <- 0
for (i in seq_len(n)) {
  p <- participants[i, ]
  for (j in seq_len(sample(1:3, 1))) {
    e <- e + 1
    s <- services[sample(nrow(services), 1), ]
    d <- as.Date("2024-03-01") + sample(0:560, 1)
    path <- c(if (runif(1) < .4) "Waiting for Assessment",
              if (runif(1) < .5) "On Waitlist",
              if (runif(1) < .15) "On Hold",
              if (runif(1) < .8) "Active",
              if (runif(1) < .4) "On Hold" else NULL,
              if (runif(1) < .5) "Inactive",
              if (runif(1) < .05) "Not Admitted" else NULL)
    if (length(path) == 0) path <- "Active"
    starts <- d + cumsum(c(0, sample(0:60, length(path) - 1, TRUE)))
    decision <- if (any(path %in% c("On Waitlist", "Active"))) starts[which(path != "Waiting for Assessment")[1]] else as.Date(NA)
    if (runif(1) < .1) decision <- as.Date(NA)
    svc_rows[[e]] <- tibble::tibble(
      participant_id = p$participant_id, service_enrollment_id = paste0("E", e),
      service_id = paste0("S", e, if (runif(1) < .03) "--99" else ""),
      funder_service_code = s$funder_service_code, service_name = s$service_name,
      service_status = path, service_status_start_date = starts, service_decision_date = decision,
      service_status_reason = ifelse(path == "Inactive", sample(reasons, length(path), TRUE), NA),
      service_first_direct_activity = if (runif(1) < .7) starts[1] + sample(0:40, 1) else as.Date(NA),
      participant_funder_age_group_code = age_code(p$participant_date_of_birth, starts[1])
    )
    # activities across the enrollment
    k <- sample(0:12, 1)
    if (k > 0) {
      dates <- d + sample(-10:250, k, TRUE)
      type <- sample(c("Face-to-face", "Face-to-face Virtual", "Non-face-to-face", "Assessment", "Meal delivery", "Indirect"),
                     k, TRUE, c(.35, .1, .25, .1, .1, .1))
      act_rows[[e]] <- tibble::tibble(
        participant_id = p$participant_id, service_id = paste0("S", e),
        funder_service_code = s$funder_service_code, service_name = s$service_name,
        activity_date = dates, activity_type = type,
        activity_individual_group = "Individual", activity_group_id = NA_character_,
        activity_duration = sample(c(NA, 0, 3, 5, 15, 45, 90, 200, 400), k, TRUE),
        activity_status = sample(c("Completed", "Cancelled"), k, TRUE, c(.9, .1)),
        activity_count = sample(c(1, 1, 1, 2), k, TRUE), activity_worker_count = sample(c(1, 1, 2), k, TRUE),
        participant_funder_age_group_code = age_code(p$participant_date_of_birth, dates),
        participant_preferred_language = p$participant_preferred_language,
        participant_ohrs_special_needs = p$participant_ohrs_special_needs,
        activity_assessment = ifelse(type == "Assessment",
                                     sample(c("Assessment Complete/Outcome – Eligible In-Home – Admitted",
                                              "Assessment Complete/Outcome – Eligible In-Home – Client Declined",
                                              "Assessment Complete/Outcome – Eligible – Adult Day", "Other"), k, TRUE), NA),
        service_sub_service_name = sample(c("Hot", "Frozen", "Soup", "Side", NA), k, TRUE)
      )
    }
  }
}
services_df <- dplyr::bind_rows(svc_rows)
activities <- dplyr::bind_rows(act_rows)

# group sessions: random registered attendees sharing a group id, plus unregistered attendees
grp_fcs <- c("72 5 09 76", "72 5 82 33", "72 5 08", "72 5 40 76 10")
grp <- list()
for (g in 1:60) {
  s <- services[services$funder_service_code %in% grp_fcs, ][sample(8, 1), ]
  who <- services_df |> dplyr::filter(service_name == s$service_name) |> dplyr::distinct(participant_id, service_enrollment_id)
  if (nrow(who) == 0) next
  who <- who[sample(nrow(who), min(nrow(who), sample(2:5, 1))), ]
  dt <- as.Date("2024-06-01") + sample(0:480, 1); dur <- sample(c(NA, 45, 90, 200, 400), 1)
  typ <- sample(c("Face-to-face", "Face-to-face Virtual"), 1, prob = c(.8, .2))
  grp[[g]] <- dplyr::bind_rows(
    tibble::tibble(participant_id = who$participant_id, service_id = paste0("S", sub("E", "", who$service_enrollment_id))),
    tibble::tibble(participant_id = rep("Unregistered", sample(0:3, 1)), service_id = "S-unreg")
  ) |>
    dplyr::mutate(funder_service_code = s$funder_service_code, service_name = s$service_name,
                  activity_date = dt, activity_type = typ, activity_individual_group = "Group",
                  activity_group_id = paste0("G", g), activity_duration = dur, activity_status = "Completed",
                  activity_count = 1, activity_worker_count = sample(1:2, 1),
                  participant_funder_age_group_code = "40",
                  participant_preferred_language = "English", participant_ohrs_special_needs = NA_character_,
                  activity_assessment = NA_character_, service_sub_service_name = NA_character_)
}
# unregistered individual contacts (incl. I&R)
unreg <- tibble::tibble(
  participant_id = "Unregistered", service_id = "S-unreg",
  funder_service_code = sample(c("72 5 70 10", "72 5 09 76", "72 5 08", "72 5 82 33"), 150, TRUE)) |>
  dplyr::left_join(services |> dplyr::filter(k == 1) |> dplyr::select(-k), by = "funder_service_code") |>
  dplyr::mutate(activity_date = as.Date("2024-04-01") + sample(0:540, dplyr::n(), TRUE),
                activity_type = "Non-face-to-face", activity_individual_group = "Individual",
                activity_group_id = NA_character_, activity_duration = 10, activity_status = "Completed",
                activity_count = sample(1:3, dplyr::n(), TRUE), activity_worker_count = 1,
                participant_funder_age_group_code = NA_character_, participant_preferred_language = NA_character_,
                participant_ohrs_special_needs = NA_character_, activity_assessment = NA_character_,
                service_sub_service_name = NA_character_)
activities <- dplyr::bind_rows(activities, dplyr::bind_rows(grp), unreg) |>
  dplyr::mutate(activity_id = paste0("A", dplyr::row_number()))
fc_mapping <- services |> dplyr::distinct(funder_service_code, service_name) |>
  dplyr::mutate(service_department_name = "Dept")

data_folder <- file.path(tempfile("golden-"), "data")
dir.create(file.path(data_folder, "interim", "rds"), recursive = TRUE)
dir.create(file.path(data_folder, "processed"), recursive = TRUE)
readr::write_rds(participants, file.path(data_folder, "interim/rds/participants.rds"))
readr::write_rds(services_df, file.path(data_folder, "interim/rds/services.rds"))
readr::write_rds(activities, file.path(data_folder, "interim/rds/activities.rds"))
readr::write_rds(fc_mapping, file.path(data_folder, "interim/rds/functional_centre_mapping.rds"))

combos <- tidyr::expand_grid(fc_450_version = c("verA", "verB"), fc_455_version = c("verA", "verB", "verC"),
                             fc_506_version = c("verA", "verB"), ir_version = c("verCMHA", "verCSS"))
canon <- function(df) {
  df <- dplyr::ungroup(df) |> as.data.frame()
  df <- df[, sort(names(df)), drop = FALSE]
  df[do.call(order, unname(as.list(df))), , drop = FALSE] |> `rownames<-`(NULL)
}
results <- list()
for (i in seq_len(nrow(combos))) {
  v <- combos[i, ]
  out <- suppressMessages(generate_tables(data_folder, v$fc_450_version, v$fc_455_version, v$fc_506_version,
                                          v$ir_version, report_date = as.Date("2025-09-30")))
  key <- paste(unlist(v), collapse = "-")
  results[[key]] <- list(tables = lapply(out, canon), colnames = lapply(out, names),
                         groups = lapply(out, dplyr::group_vars))
}

if (mode == "save") {
  saveRDS(results, out_path)
  cat("saved", length(results), "runs to", out_path, "\nrows per table in first run:\n")
  print(sapply(results[[1]]$tables, nrow))
  codes <- unlist(lapply(results, function(r) unlist(lapply(r$tables, function(t) t$funder_statistical_account_code))))
  stats <- unique(substr(codes, 1, 3))
  cat("statistics present:", sort(stats), "\n")
} else {
  before <- readRDS(out_path)
  bad <- 0
  for (key in names(before)) for (tb in names(before[[key]]$tables)) {
    a <- before[[key]]$tables[[tb]]; b <- results[[key]]$tables[[tb]]
    eq <- isTRUE(all.equal(a, b, check.attributes = FALSE))
    colorder <- identical(before[[key]]$colnames[[tb]], results[[key]]$colnames[[tb]])
    groups <- identical(before[[key]]$groups[[tb]], results[[key]]$groups[[tb]])
    if (!eq || !colorder || !groups) {
      bad <- bad + 1
      cat("DIFF", key, tb, if (!colorder) "(column order)", if (!groups) "(grouping)", "\n")
      if (!eq) {
        acc <- union(a$funder_statistical_account_code, b$funder_statistical_account_code)
        for (s in unique(substr(acc, 1, 3))) {
          x <- a[substr(a$funder_statistical_account_code, 1, 3) == s, ]; y <- b[substr(b$funder_statistical_account_code, 1, 3) == s, ]
          if (!isTRUE(all.equal(x, y, check.attributes = FALSE))) cat("   stat", s, ": before", nrow(x), "rows, after", nrow(y), "\n")
        }
      }
    }
  }
  cat(if (bad == 0) "IDENTICAL across all runs\n" else paste(bad, "table(s) differ\n"))
  if (bad > 0) quit(status = 1)
}
