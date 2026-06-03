library(tidyverse)
library(lubridate)
library(skimr)
library(janitor)
library(here)

file_path <- here::here("data", "homeless_services_raw.csv")
if (!file.exists(file_path)) {
  stop("ERROR: File not found at: ", file_path)
}
df <- readr::read_csv(file_path, show_col_types = FALSE)
cat("\n# SECTION 1\n")
cat("Data successfully loaded.\n")
cat("Rows:", nrow(df), " Columns:", ncol(df), "\n")
cat("\n# SECTION 2\n")
glimpse(df)
dup_cols <- duplicated(names(df))
empty_cols <- names(df)[names(df) == ""]
cat("Duplicate column names:", sum(dup_cols), "\n")
cat("Empty column names:", length(empty_cols), "\n")
cat("\n# SECTION 3\n")
missing_summary <- df %>%
  summarise(across(everything(),
                   ~sum(is.na(.) | trimws(as.character(.)) %in% 
                          c("", "NA", "N/A", "Unknown", "null", "-", "."))
  )) %>%
  pivot_longer(everything(),
               names_to = "Variable",
               values_to = "Missing_Count") %>%
  mutate(Percent_Missing = round((Missing_Count / nrow(df)) * 100, 2)) %>%
  arrange(desc(Missing_Count))
print(missing_summary, n=100)
row_missing <- rowSums(df %>%
                         mutate(across(everything(), ~is.na(.) | trimws(as.character(.)) %in%
                                         c("", "NA", "N/A", "Unknown", "null", "-", "."))))
cat("Rows with >3 missing values:", sum(row_missing > 3), "\n")
cat("\n# SECTION 4\n")
cat("Exact duplicate rows:", sum(duplicated(df)), "\n")
if ("client_id" %in% names(df)) {
  cat("Duplicate client_ids:", sum(duplicated(df$client_id)), "\n")
  cat("Missing client_ids:", sum(is.na(df$client_id)), "\n")
  cat("Unique client_ids:", dplyr::n_distinct(df$client_id), "\n")
} else {
  cat("client_id column not found.\n")
}
cat("\n# SECTION 5\n")
numeric_df <- df %>% select(where(is.numeric))
print(skimr::skim(numeric_df))
neg_vals <- numeric_df %>%
  summarise(across(everything(), ~sum(. < 0, na.rm = TRUE)))
zero_vals <- numeric_df %>%
  summarise(across(everything(), ~sum(. == 0, na.rm = TRUE)))
print("Negative values:")
print(neg_vals)
print("Zero values:")
print(zero_vals)
cat("\n# SECTION 6\n")
char_df <- df %>% select(where(is.character))
for (col in names(char_df)) {
  cat("\nVariable:", col, "\n")
  print(sort(unique(char_df[[col]])))
}
cat("\n# SECTION 7\n")
date_cols <- names(df)[str_detect(names(df), "date")]
for (col in date_cols) {
  cat("\nChecking:", col, "\n")
  print(summary(df[[col]]))
}
cat("\n# SECTION 8\n")
if (all(c("housing_placement_days", "length_of_stay") %in% names(df))) {
  cat("Placement > Stay:",
      nrow(df %>% filter(housing_placement_days > length_of_stay)), "\n")
}
if (all(c("exit_status", "housing_placement_days") %in% names(df))) {
  cat("Permanent exit missing placement days:",
      nrow(df %>% filter(exit_status == "Permanent" &
                           is.na(housing_placement_days))), "\n")
}
cat("\n# SECTION 9\n")
if ("exit_status" %in% names(df)) {
  print(prop.table(table(df$exit_status, useNA = "ifany")) * 100)
}
print(numeric_df %>%
        summarise(across(everything(), ~var(., na.rm = TRUE))) %>%
        select(where(~. == 0 | is.na(.))))
