all_functional_centres_and_statistical_codes <- readr::read_csv(paste0(Sys.getenv("USERPROFILE"), r"(\Workspace\functional-centre-statistical-account-generation\output_files\statistical_codes.csv)"))

usethis::use_data(all_functional_centres_and_statistical_codes, overwrite = TRUE)
