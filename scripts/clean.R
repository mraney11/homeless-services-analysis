library(tidyverse)
library(lubridate)
library(janitor)
library(here)

file_path_in <- here::here("data", "homeless_services_raw.csv")
file_path_out <- here::here("data", "homeless_services_clean.csv")
if (!file.exists(file_path_in)) {
  stop("ERROR: File not found at: ", file_path_in)
}
df_raw <- readr::read_csv(file_path_in, show_col_types = FALSE)
df_clean <- df_raw
cat("--- SECTION 2: Column Standardization ---\n")
df_clean <- df_clean %>%
  janitor::clean_names()
cat("--- SECTION 3: Missing Value Handling ---\n")
df_clean <- df_clean %>%
  mutate(across(where(is.character),
                ~ifelse(trimws(.) %in% c("", "NA", "N/A", "Unknown", "null", "-", "."), NA_character_, .)
  ))
cat("--- SECTION 4: Data Type Correction ---\n")
date_cols <- names(df_clean)[str_detect(names(df_clean), "date")]
if(length(date_cols) > 0) {
  df_clean <- df_clean %>%
    mutate(across(all_of(date_cols), \(x) lubridate::ymd(x, quiet = TRUE)))
}
df_clean <- df_clean %>%
  mutate(across(where(is.character),
                ~ifelse(str_detect(., "^[0-9.]+$"), as.numeric(.), .)))
cat("--- SECTION 5: Categorical Cleaning ---\n")
df_clean <- df_clean %>%
  mutate(across(where(is.character), ~str_trim(.))) %>%
  # Fix double spaces to single spaces
  mutate(across(where(is.character), ~str_replace_all(., "\\s+", " "))) %>%
  mutate(across(where(is.character), ~str_to_title(.)))
cat("--- SECTION 6: Numeric Corrections ---\n")
df_clean <- df_clean %>%
  mutate(
    age = as.numeric(age),
    length_of_stay = as.numeric(length_of_stay),
    housing_placement_days = as.numeric(housing_placement_days)
  ) %>%
  mutate(
    age = ifelse(age < 0 | age > 100, NA, age),
    length_of_stay = ifelse(length_of_stay < 0, NA, length_of_stay),
    housing_placement_days = ifelse(housing_placement_days < 0, NA, housing_placement_days)
  )
cat("--- SECTION 7: Logical Consistency Fixes ---\n")
# Impute placement days for Permanent exits missing this value
df_clean <- df_clean %>%
  mutate(
    housing_placement_days = ifelse(
      exit_status == "Permanent" & is.na(housing_placement_days),
      length_of_stay,
      housing_placement_days
    )
  ) %>%
  mutate(
    housing_placement_days =
      ifelse(housing_placement_days > length_of_stay,
             length_of_stay,
             housing_placement_days)
  )
cat("--- SECTION 8: Duplicate Handling ---\n")
df_clean <- df_clean %>%
  distinct()
cat("--- SECTION 10: Final Validation ---\n")
print(summary(df_clean))
cat("Remaining missing values:\n")
print(colSums(is.na(df_clean)))
cat("\nProgram Type frequencies after cleaning:\n")
print(table(df_clean$program_type))
cat("--- SECTION 11: Output Clean Data ---\n")
write_csv(df_clean, file_path_out)
cat("Data written to:", file_path_out, "\n")
