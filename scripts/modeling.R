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

file_path <- here::here("data", "homeless_services_clean.csv")

if (!file.exists(file_path)) {
  stop("ERROR: Clean dataset not found.")
}

df_clean <- readr::read_csv(file_path, show_col_types = FALSE)


# ============================================================
# 🔷 SECTION 2 — VARIABLE SELECTION
# ============================================================
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
  drop_na()

# ============================================================
# 🔷 SECTION 3 — LOGISTIC REGRESSION (PRIMARY MODEL)
# ============================================================
model_logit <- glm(
  exit_status == "Permanent" ~ age + length_of_stay + program_type + 
                               services_received + case_management_intensity + prior_episodes,
  data = model_df,
  family = binomial()
)

cat("\n--- Logistic Regression Summary ---\n")
print(summary(model_logit))

logit_results <- broom::tidy(model_logit, conf.int = TRUE)
write_csv(logit_results, here::here("outputs", "logit_results.csv"))

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
write_csv(linear_results, here::here("outputs", "linear_results.csv"))

# ============================================================
# 🔷 SECTION 5 — MODEL DIAGNOSTICS
# ============================================================
png(here::here("outputs", "model_linear_diagnostics.png"), width=800, height=600)
par(mfrow=c(2,2))
plot(model_linear)
dev.off()

cat("\nModel diagnostics saved to outputs/model_linear_diagnostics.png\n")

# ============================================================
# 🔷 SECTION 6 — MODEL PERFORMANCE
# ============================================================
model_df$pred_prob <- predict(model_logit, type = "response")
model_df$pred_class <- ifelse(model_df$pred_prob > 0.5, "Permanent", "Other")

perf_table <- table(Predicted = model_df$pred_class, Actual = model_df$exit_status)
cat("\n--- Model Performance ---\n")
print(perf_table)
write_csv(as.data.frame(perf_table), here::here("outputs", "model_performance_table.csv"))

cat("\nOutputs saved to outputs/\n")
