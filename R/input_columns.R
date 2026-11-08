# Columns expected in each input file in <data_folder>/interim/rds/. Keep in sync with
# the "Input files" section of README.md.
#
# - required: the run stops with an error if the column is missing.
# - expected: the run warns and continues with the column blank (NA), so the
#   statistics in used_for will be undercounted.
input_columns <- dplyr::tribble(
  ~input,                       ~column,                             ~level,      ~used_for,
  "participants",               "participant_id",                    "required",  "all",
  "participants",               "participant_date_of_birth",         "required",  "age groups in the daily census",
  "participants",               "participant_preferred_language",    "expected",  "S950",
  "participants",               "participant_ohrs_special_needs",    "expected",  "S955",

  "services",                   "participant_id",                    "required",  "all",
  "services",                   "service_enrollment_id",             "required",  "all",
  "services",                   "service_id",                        "required",  "all",
  "services",                   "funder_service_code",               "required",  "all",
  "services",                   "service_name",                      "required",  "counts by service",
  "services",                   "service_status",                    "required",  "all",
  "services",                   "service_status_start_date",         "required",  "all",
  "services",                   "service_decision_date",             "required",  "S407, S489, S501",
  "services",                   "service_status_reason",             "required",  "S511, S513 reason codes",
  "services",                   "participant_funder_age_group_code", "required",  "age groups",
  "services",                   "service_status_end_date",           "expected",  "same-day Active corrections",
  "services",                   "service_first_direct_activity",     "expected",  "service start date",

  "activities",                 "participant_id",                    "required",  "all",
  "activities",                 "service_id",                        "required",  "all",
  "activities",                 "funder_service_code",               "required",  "all",
  "activities",                 "service_name",                      "required",  "counts by service",
  "activities",                 "activity_date",                     "required",  "all",
  "activities",                 "activity_type",                     "required",  "all",
  "activities",                 "activity_individual_group",         "required",  "all",
  "activities",                 "activity_duration",                 "required",  "S450/451 (fc_450_version), hours of care",
  "activities",                 "activity_status",                   "required",  "all",
  "activities",                 "activity_count",                    "required",  "visits, interactions",
  "activities",                 "activity_worker_count",             "required",  "interactions, hours of care",
  "activities",                 "activity_group_id",                 "required",  "group sessions and attendance",
  "activities",                 "activity_id",                       "expected",  "detail tables (the visit behind each count)",
  "activities",                 "participant_funder_age_group_code", "required",  "age groups",
  "activities",                 "activity_assessment",               "expected",  "CSS assessments",
  "activities",                 "service_sub_service_name",          "expected",  "S248 meal types",

  "functional_centre_mapping",  "funder_service_code",               "required",  "all",
  "functional_centre_mapping",  "service_name",                      "expected",  "service list for the Power BI report",
  "functional_centre_mapping",  "service_department_name",           "expected",  "service list"
)

# Stop if a required column is missing; warn and fill with NA if an expected one is.
check_input_columns <- function(data, input) {
  spec <- input_columns[input_columns$input == input, ]

  missing_required <- setdiff(spec$column[spec$level == "required"], colnames(data))
  if (length(missing_required) > 0) {
    stop(glue::glue("{input}.rds is missing required column(s): {paste(missing_required, collapse = ', ')}"),
         call. = FALSE)
  }

  expected <- spec[spec$level == "expected", ]
  missing_expected <- expected[!expected$column %in% colnames(data), ]
  for (i in seq_len(nrow(missing_expected))) {
    data_warning(glue::glue("{input}.rds has no {missing_expected$column[i]} column; it is treated as blank, ",
                            "so {missing_expected$used_for[i]} will be undercounted."))
    column <- missing_expected$column[i]
    data[[column]] <- if (column %in% c("service_first_direct_activity", "service_status_end_date")) lubridate::NA_Date_ else NA_character_
  }

  data
}
