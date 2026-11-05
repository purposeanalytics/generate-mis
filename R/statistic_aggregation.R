# How each statistic rolls up over a period, so the Power BI report can pick the right
# measure from the data instead of a list of statistic codes. Written to service_list.csv as
# the `aggregation` column, matched on the first three digits of the account.
# - sum_visits_and_services: add `value` across dates (visit and service counts file)
# - sum_census: add `value` across dates (census counts file)
# - end_of_period: `value` on the last day of the period, a snapshot (census counts file)
# - distinct_cumulative: count distinct participant_id from the start of the fiscal year
#   (census counts file)
statistic_aggregation <- dplyr::tribble(
  ~statistic, ~aggregation,
  "248",      "sum_visits_and_services",  # meals delivered
  "265",      "sum_visits_and_services",  # service provider interactions
  "266",      "sum_visits_and_services",  # service provider group interactions
  "401",      "sum_visits_and_services",  # resident admissions
  "403",      "sum_census",               # resident days
  "406",      "end_of_period",            # individuals currently waiting (incl. 406 99)
  "407",      "sum_visits_and_services",  # days waited
  "410",      "sum_visits_and_services",  # resident discharges
  "411",      "sum_visits_and_services",  # resident deaths
  "450",      "sum_visits_and_services",  # visits, face-to-face
  "451",      "sum_visits_and_services",  # visits, non-face-to-face
  "452",      "sum_visits_and_services",  # not uniquely identified interactions
  "454",      "sum_visits_and_services",  # hours of care
  "455",      "distinct_cumulative",      # individuals served by FC
  "483",      "sum_visits_and_services",  # attendance days, face-to-face
  "484",      "sum_visits_and_services",  # attendance days, non-face-to-face
  "489",      "sum_visits_and_services",  # new referrals
  "491",      "sum_visits_and_services",  # group participant attendances
  "492",      "sum_visits_and_services",  # group sessions
  "501",      "sum_visits_and_services",  # admissions to the organization
  "506",      "sum_visits_and_services",  # individuals received first service
  "511",      "sum_visits_and_services",  # files closed
  "512",      "sum_visits_and_services",  # assessments complete
  "513",      "sum_visits_and_services",  # service discharges
  "855",      "distinct_cumulative",      # individuals served by organization
  "950",      "distinct_cumulative",      # francophone individuals
  "955",      "distinct_cumulative"       # individuals served by special needs
)
