
# generateMIS

<!-- badges: start -->
<!-- badges: end -->

The generateMIS package transforms program data saved in Purpose Analytics' standard format into statistics required by the Ministry of Health and Long-Term Care for quarterly reporting (Chapters 7 and 10).

The statistics are saved in a "pre-aggregated" format so that they can be consumed by Purpose Analytics' Power BI MIS Report. The Power BI MIS Report supports dynamic aggregation so that MIS statistics and targets for the year can be calculated on the fly for any point in time.

The generateMIS package requires a companion script which carries out the first part of process by reading raw data exports from a folder `data/raw` and transforming them into the standard format in a folder `data/interim`. generateMIS completes the second part of the process by reading the files in `data/interim/rds` and transforming them into files in `data/processed` that will be read by the Power BI MIS Report. An additional file, `MSAA targets` is required in the `data/external` folder. 

## Installation

You can install the development version of generateMIS from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("purposeanalytics/generateMIS")
```

## Usage

``` r
library(generateMIS)

generate_tables(
  data_folder, 
  fc_450_version = "verA"
  fc_455_version = "verA", 
  fc_506_version = "verA",
  ir_version = "verCMHA",
  report_date = Sys.Date(),
  detail = FALSE
)

```

## Arguments

**data_folder**

The location on your local drive of the `data` folder containing the following sub-folders: `raw`, `interim/csv`, `interim/rds`, `processed`, `external` 

**fc_450_version**
This can take one of two conditions for counting visits under the "Visits" and "Service Provider Interactions" statistics (450 \*\* \*\*, 451 \*\* \*\*, 265 \*\* \*\*):

* `verA` which excludes visits that were 5 or fewer minutes in duration (but includes entries with no reported time interval)
* `verB` which counts all visits regardless of duration (can be necessary for some bulk notes where client management system divides total time by number of clients)

The same rule decides which visits count as evidence that someone was served (455 \*\* \*\*, 855 \*\* \*\*, 950 \*\* \*\*, 955 \*\* \*\*), so visit and individuals-served counts use the same visits.

**fc_455_version**

This can take one of three conditions for counting individuals under the "Individuals Served" statistics (455 \*\* \*\*, 855 \*\* \*\*, 950 \*\* \*\*, 955 \*\* \*\*):

* `verA` which requires that an individual has been enrolled or admitted to a program or service *and* has received at least one visit 
* `verB` which requires that an individual has received at least one visit *regardless* of their enrollment or admission status 
* `verC` which requires that an individual has been enrolled or admitted to a program or service *regardless* of whether they have received a visit (useful for programs where visits/contacts are not recorded against the client file)

**fc_506_version**

This can take one of two conditions for counting when the first service was received (506 \*\* \*\*):

* `verA` which counts the date of enrollment or admission as the first service date
* `verB` which counts the first visit after the enrollment or admission date

**ir_version**

This can take one of two conditions which assigns either CMHA or CSS statistical codes to the Information and Referral functional centre:

* `verCMHA` which sets the "Not Uniquely Identified SR Interactions" statistical code to 452 65 00
* `verCSS` which sets the "Not Uniquely Identified SR Interactions" statistical code to 452 60 00

**report_date**

The date the run is "as at". It defaults to today. Records dated after it are ignored: service statuses that start later and activities on later dates are dropped, and decision or first-service dates that fall later are treated as not yet known. Statuses that are still open are treated as running to the day after this date.

## Input files

`generate_tables()` reads four files from `data_folder/interim/rds/`. If a **required** column is missing, the run stops with an error that names it. If an **expected** column is missing, the run warns and carries on with that column blank, so the statistics shown will be undercounted. Exports should always include the expected columns.

The full list is `input_columns` in `R/input_columns.R`. Keep this section in step with it.

**participants.rds**, one row per participant

| Column | Level | Used for |
|---|---|---|
| `participant_id` | required | all |
| `participant_date_of_birth` | required | age groups in the daily census |
| `participant_preferred_language` | expected | 950 |
| `participant_ohrs_special_needs` | expected | 955 |

**services.rds**, one row per service status

| Column | Level | Used for |
|---|---|---|
| `participant_id`, `service_enrollment_id`, `service_id`, `funder_service_code`, `service_status`, `service_status_start_date` | required | all |
| `service_name` | required | counts by service |
| `service_decision_date` | required | 407, 489, 501 |
| `service_status_reason` | required | 511, 513 reason codes |
| `participant_funder_age_group_code` | required | age groups |
| `service_first_direct_activity` | expected | service start date |

**activities.rds**, one row per activity

| Column | Level | Used for |
|---|---|---|
| `participant_id`, `service_id`, `funder_service_code`, `activity_date`, `activity_type`, `activity_individual_group`, `activity_status` | required | all |
| `service_name` | required | counts by service |
| `activity_duration` | required | 450/451 (fc_450_version), hours of care |
| `activity_count` | required | visits, interactions |
| `activity_worker_count` | required | interactions, hours of care |
| `activity_group_id` | required | group sessions and attendance |
| `activity_id` | expected | detail tables: the visit behind each count. AlayaCare's visit ID; data prep generates a stable, unique ID for CRMS |
| `participant_funder_age_group_code` | required | age groups |
| `activity_assessment` | expected | CSS assessments |
| `service_sub_service_name` | expected | 248 meal types |

Language and special needs are recorded once per participant and read from participants.rds in every fc_455_version. Copies of these columns on services or activities are ignored.

**functional_centre_mapping.rds**, one row per functional centre

| Column | Level | Used for |
|---|---|---|
| `funder_service_code` | required | all |
| `service_name`, `service_department_name` | expected | service list for the Power BI report |

## Detail tables

The detail tables list the rows behind every statistic, so a count can be traced back to the clients and records it came from. There is one table per reporting level: `mis_detail_by_fc.csv` and `mis_detail_by_service.csv`. Filter on an account, a functional centre, a client or a date range to see who was counted. The census statistics (S403, S406, S455, S855, S950, S955) are reported only here, as one row per client per day. The visit and service statistics are also summed in `mis_visit_and_service_counts_by_fc.csv` and `mis_visit_and_service_counts_by_service.csv`, which are the sums of their detail rows (the tests check this).

They contain client identifiers, so restrict who can see them in the Power BI report.

| Column | Meaning |
|---|---|
| `date` | The date the row counts on, as in the summary tables |
| `funder_service_code`, `service_name` | Where it is reported (`service_name` in the by-service table only) |
| `funder_statistical_account_code` | The account (its first three digits are the statistic) |
| `aggregation` | How the statistic totals over a period (as in `service_list.csv`; see below) |
| `participant_id` | The client. Blank for group sessions (S266, S492); "Unregistered" for not uniquely identified contacts |
| `reference` | The record to look up (blank for census statistics): the enrollment for episodes, waits, admissions and discharges (all of them, e.g. `E1, E2`, when overlapping enrollments in the FC were merged into one episode); the activity for visit statistics; the group session for S266 and S492 |
| `value` | What the row adds: 1 for a count or a client day, or days, hours, meals or interactions |
| `funder_key`, `funder_service_key` | The key to the funder codes table, as in the summary tables (`funder_service_key` in the by-service table only) |

To total a statistic over a period, follow its `aggregation`: add up `value` for visit and service statistics (`sum_visits_and_services`) and S403 (`sum_census`); add up `value` on the last day of the period for S406 (`end_of_period`); count distinct `participant_id` for S455, S855, S950 and S955 (`distinct_cumulative`).

## Tests

The tests in `tests/testthat` run the full pipeline on a small set of made-up clients, each set up to check one counting rule. Run them with:

``` r
devtools::test()
```

### Checking that a change leaves the output unchanged

`dev/compare-outputs.R` builds a larger random dataset of made-up people across 11 functional centres, with two services each. It runs every combination of the version arguments, then compares the output tables with a saved copy. Use it when restructuring code, where the numbers shouldn't move. It takes a few minutes. Run it from the repo root:

``` sh
Rscript dev/compare-outputs.R save      # before the change
Rscript dev/compare-outputs.R compare   # after the change
```

`compare` names each statistic that differs. The baseline is saved to `dev/baseline.rds`, which isn't committed.
