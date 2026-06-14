# ============================================================
# PROMPT 5 — MASTER MODELING SYSTEM
# File Name: 05_modeling.R
# Purpose: Build, evaluate, and interpret statistical models
# Input: Clean dataset + EDA insights
# Output: Models, diagnostics, and interpretation
# ============================================================

library(tidyverse)
library(broom)
library(here)

# Setup output directory
output_dir <- here::here("outputs")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

file_path <- here::here("data", "homeless_services_clean.csv")
if (!file.exists(file_path)) {
  stop("ERROR: Clean dataset not found at: ", file_path)
}

df_clean <- readr::read_csv(file_path, show_col_types = FALSE)

# ============================================================
# 🔷 SECTION 2 — VARIABLE SELECTION & PREPARATION
# ============================================================
# Filter key model variables, handle NAs, and pre-calculate binary outcome variable
model_df <- df_clean %>%
  select(
    exit_status,
    age,
    length_of_stay,
    program_type,
    services_received,
    case_management_intensity,
    prior_episodes
  ) %>%
  drop_na() %>%
  mutate(is_permanent = if_else(exit_status == "Permanent", 1, 0))

# ============================================================
# 🔷 SECTION 3 — LOGISTIC REGRESSION (PRIMARY MODEL)
# ============================================================
model_logit <- glm(
  is_permanent ~ age + length_of_stay + program_type + 
                 services_received + case_management_intensity + prior_episodes,
  data = model_df,
  family = binomial()
)

cat("\n--- Logistic Regression Summary ---\n")
print(summary(model_logit))

logit_results <- broom::tidy(model_logit, conf.int = TRUE)
readr::write_csv(logit_results, here::here("outputs", "logit_results.csv"))

# ============================================================
# 🔷 SECTION 4 — LINEAR REGRESSION (SECONDARY MODEL)
# ============================================================
model_linear <- lm(
  length_of_stay ~ age + program_type + services_received + 
                   case_management_intensity + prior_episodes,
  data = model_df
)

cat("\n--- Linear Regression Summary ---\n")
print(summary(model_linear))

linear_results <- broom::tidy(model_linear, conf.int = TRUE)
readr::write_csv(linear_results, here::here("outputs", "linear_results.csv"))

# ============================================================
# 🔷 SECTION 5 — MODEL DIAGNOSTICS
# ============================================================
png(here::here("outputs", "model_linear_diagnostics.png"), width = 800, height = 600)
par(mfrow = c(2, 2))
plot(model_linear)
dev.off()

cat("\nModel diagnostics saved to outputs/model_linear_diagnostics.png\n")

# ============================================================
# 🔷 SECTION 6 — MODEL PERFORMANCE
# ============================================================
model_df$pred_prob <- predict(model_logit, type = "response")
model_df$pred_class <- if_else(model_df$pred_prob > 0.5, "Permanent", "Other")

# 1. 2x3 Confusion Matrix against original multi-class exit_status
perf_table_detailed <- table(Predicted = model_df$pred_class, Actual = model_df$exit_status)
cat("\n--- Model Performance (Detailed 2x3 Confusion Matrix) ---\n")
print(perf_table_detailed)
readr::write_csv(as.data.frame(perf_table_detailed), here::here("outputs", "model_performance_table.csv"))

# 2. 2x2 Binary Confusion Matrix
actual_binary <- if_else(model_df$exit_status == "Permanent", "Permanent", "Other")
perf_table_binary <- table(Predicted = model_df$pred_class, Actual = actual_binary)
cat("\n--- Model Performance (Binary 2x2 Confusion Matrix) ---\n")
print(perf_table_binary)

# 3. Formal Classification Metrics
tp <- sum(model_df$pred_class == "Permanent" & actual_binary == "Permanent")
fp <- sum(model_df$pred_class == "Permanent" & actual_binary == "Other")
fn <- sum(model_df$pred_class == "Other" & actual_binary == "Permanent")
tn <- sum(model_df$pred_class == "Other" & actual_binary == "Other")

accuracy <- (tp + tn) / nrow(model_df)
sensitivity <- tp / (tp + fn)
specificity <- tn / (tn + fp)
precision <- tp / (tp + fp)

cat("\n--- Classification Performance Metrics (Threshold = 0.5) ---\n")
cat("Accuracy:    ", round(accuracy * 100, 2), "%\n", sep = "")
cat("Sensitivity: ", round(sensitivity * 100, 2), "%\n", sep = "")
cat("Specificity: ", round(specificity * 100, 2), "%\n", sep = "")
cat("Precision:   ", round(precision * 100, 2), "%\n", sep = "")

cat("\nOutputs saved to outputs/\n")

