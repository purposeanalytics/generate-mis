
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
  ir_version = "verCMHA"
)

```

## Arguments

**data_folder**

The location on your local drive of the `data` folder containing the following sub-folders: `raw`, `interim/csv`, `interim/rds`, `processed`, `external` 

**fc_450_version**
This can take one of two conditions for counting visits under the "Visits" and "Service Provider Interactions" statistics (450 \*\* \*\*, 451 \*\* \*\*, 265 \*\* \*\*):

* `verA` which excludes visits that were 5 or fewer minutes in duration (but includes entries with no reported time interval)
* `verB` which counts all visits regardless of duration (can be necessary for some bulk notes where client management system divides total time by number of clients)

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
