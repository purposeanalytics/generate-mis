all_functional_centres_and_statistical_codes <- readr::read_csv(r"(C:\Temp\westnh\mis_powerbi\outputs\statistical_codes.csv)")

usethis::use_data(all_functional_centres_and_statistical_codes, overwrite = TRUE)
