library(tidyverse)
library(janitor)
library(here)

file_path_in <- here::here("data", "homeless_services_raw.csv")
file_path_out <- here::here("data", "homeless_services_clean.csv")

if (!file.exists(file_path_in)) {
  stop("ERROR: Raw file not found at: ", file_path_in)
}

df_raw <- readr::read_csv(file_path_in, show_col_types = FALSE)
df_clean <- df_raw

cat("--- SECTION 2: Column Standardization ---\n")
df_clean <- df_clean %>%
  janitor::clean_names()

cat("--- SECTION 3: Missing Value Handling ---\n")
df_clean <- df_clean %>%
  mutate(across(where(is.character),
                ~ if_else(trimws(.x) %in% c("", "NA", "N/A", "Unknown", "null", "-", "."), NA_character_, .x)
  ))

cat("--- SECTION 4: Data Type Correction ---\n")
date_cols <- names(df_clean)[str_detect(names(df_clean), "date")]
if (length(date_cols) > 0) {
  df_clean <- df_clean %>%
    mutate(across(all_of(date_cols), ~ lubridate::ymd(.x, quiet = TRUE)))
}

# Robust check to convert character columns to numeric if they are purely numeric
is_coercible_to_numeric <- function(x) {
  if (!is.character(x)) return(FALSE)
  x_clean <- x[!is.na(x)]
  if (length(x_clean) == 0) return(FALSE)
  parsed <- suppressWarnings(as.numeric(x_clean))
  !any(is.na(parsed))
}

df_clean <- df_clean %>%
  mutate(across(where(is_coercible_to_numeric), as.numeric))

cat("--- SECTION 5: Categorical Cleaning ---\n")
# Helper function to clean text, standardise spacing, title case, and correct 'to' preposition
clean_text_standard <- function(x) {
  if (!is.character(x)) return(x)
  x %>%
    str_trim() %>%
    str_replace_all("\\s+", " ") %>%
    str_to_title() %>%
    str_replace_all("\\bTo\\b", "to")
}

df_clean <- df_clean %>%
  mutate(across(where(is.character), clean_text_standard))

cat("--- SECTION 6: Numeric Corrections ---\n")
df_clean <- df_clean %>%
  mutate(
    age = as.numeric(age),
    length_of_stay = as.numeric(length_of_stay),
    housing_placement_days = as.numeric(housing_placement_days)
  ) %>%
  mutate(
    age = if_else(age < 0 | age > 100, NA_real_, age),
    length_of_stay = if_else(length_of_stay < 0, NA_real_, length_of_stay),
    housing_placement_days = if_else(housing_placement_days < 0, NA_real_, housing_placement_days)
  )

cat("--- SECTION 7: Logical Consistency Fixes ---\n")
# Impute placement days for Permanent exits missing this value, and cap placement days at stay length
df_clean <- df_clean %>%
  mutate(
    housing_placement_days = if_else(
      !is.na(exit_status) & exit_status == "Permanent" & is.na(housing_placement_days),
      length_of_stay,
      housing_placement_days
    ),
    housing_placement_days = pmin(housing_placement_days, length_of_stay)
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
readr::write_csv(df_clean, file_path_out)
cat("Data written to:", file_path_out, "\n")

