library(tidyverse)
library(skimr)
library(here)

# Load raw dataset
file_path <- here::here("data", "homeless_services_raw.csv")
if (!file.exists(file_path)) {
  stop("ERROR: File not found at: ", file_path)
}
df <- readr::read_csv(file_path, show_col_types = FALSE)

cat("\n# SECTION 1: Data Dimensions\n")
cat("Data successfully loaded.\n")
cat("Rows:", nrow(df), " Columns:", ncol(df), "\n")

cat("\n# SECTION 2: Structure & Schema\n")
glimpse(df)
dup_cols <- duplicated(names(df))
empty_cols <- names(df)[names(df) == ""]
cat("Duplicate column names:", sum(dup_cols), "\n")
cat("Empty column names:", length(empty_cols), "\n")

cat("\n# SECTION 3: Missing Value Analysis\n")
# Search for standard NA values and common placeholders
missing_summary <- df %>%
  summarise(across(everything(),
                   ~ sum(is.na(.x) | trimws(as.character(.x)) %in% 
                           c("", "NA", "N/A", "Unknown", "null", "-", "."))
  )) %>%
  pivot_longer(everything(),
               names_to = "Variable",
               values_to = "Missing_Count") %>%
  mutate(Percent_Missing = round((Missing_Count / nrow(df)) * 100, 2)) %>%
  arrange(desc(Missing_Count))
print(missing_summary, n = 100)

row_missing <- df %>%
  mutate(across(everything(), ~ is.na(.x) | trimws(as.character(.x)) %in%
                  c("", "NA", "N/A", "Unknown", "null", "-", "."))) %>%
  rowSums()
cat("Rows with >3 missing values:", sum(row_missing > 3), "\n")

cat("\n# SECTION 4: Duplicate & ID Checks\n")
cat("Exact duplicate rows:", sum(duplicated(df)), "\n")
if ("client_id" %in% names(df)) {
  cat("Duplicate client_ids:", sum(duplicated(df$client_id)), "\n")
  cat("Missing client_ids:", sum(is.na(df$client_id)), "\n")
  cat("Unique client_ids:", dplyr::n_distinct(df$client_id), "\n")
} else {
  cat("client_id column not found.\n")
}

cat("\n# SECTION 5: Descriptive Numeric Audit\n")
# Exclude identifier client_id from descriptive statistics
numeric_df <- df %>% 
  select(where(is.numeric)) %>% 
  select(-any_of("client_id"))

print(skimr::skim(numeric_df))

neg_vals <- numeric_df %>%
  summarise(across(everything(), ~ sum(.x < 0, na.rm = TRUE)))
zero_vals <- numeric_df %>%
  summarise(across(everything(), ~ sum(.x == 0, na.rm = TRUE)))

cat("Negative values count:\n")
print(neg_vals)
cat("Zero values count:\n")
print(zero_vals)

cat("\n# SECTION 6: Categorical Value Listings\n")
char_df <- df %>% select(where(is.character))
for (col in names(char_df)) {
  cat("\nVariable:", col, "\n")
  print(sort(unique(char_df[[col]])))
}

cat("\n# SECTION 7: Date Variable Range Check\n")
date_cols <- names(df)[str_detect(names(df), "date")]
for (col in date_cols) {
  cat("\nChecking:", col, "\n")
  print(summary(df[[col]]))
}

cat("\n# SECTION 8: Logic Consistency Audit\n")
if (all(c("housing_placement_days", "length_of_stay") %in% names(df))) {
  cat("Placement > Stay:",
      nrow(df %>% filter(housing_placement_days > length_of_stay)), "\n")
}
if (all(c("exit_status", "housing_placement_days") %in% names(df))) {
  cat("Permanent exit missing placement days:",
      nrow(df %>% filter(exit_status == "Permanent" & is.na(housing_placement_days))), "\n")
}

cat("\n# SECTION 9: Distribution & Variance Checks\n")
if ("exit_status" %in% names(df)) {
  cat("Exit Status distribution:\n")
  df %>%
    count(exit_status) %>%
    mutate(Percent = round((n / sum(n)) * 100, 2)) %>%
    print()
}

# Check for constant/zero-variance columns in numeric variables
constant_vars <- numeric_df %>%
  summarise(across(everything(), ~ var(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Variance") %>%
  filter(Variance == 0 | is.na(Variance))

if (nrow(constant_vars) > 0) {
  cat("\nConstant/Zero-variance numeric columns found:\n")
  print(constant_vars)
} else {
  cat("\nNo constant/zero-variance numeric columns found.\n")
}
