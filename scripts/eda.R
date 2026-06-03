library(tidyverse)
library(skimr)
library(here)

# Paths
file_path <- here::here("data", "homeless_services_clean.csv")
plot_dir <- here::here("outputs", "figures")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

df_clean <- read_csv(file_path, show_col_types = FALSE)
# 1. Exit Status Plot
p1 <- df_clean %>%
  count(exit_status) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(x = exit_status, y = percent, fill = exit_status)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(title = "Distribution of Exit Status", x = "Exit Status", y = "Percentage")
ggsave(file.path(plot_dir, "01_exit_status.png"), p1, width = 6, height = 4)
# 2. Univariate Numeric Plot
p2 <- df_clean %>%
  select(age, prior_episodes, length_of_stay, services_received, housing_placement_days, monthly_income_at_exit) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  facet_wrap(~name, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of Numeric Variables")
ggsave(file.path(plot_dir, "02_numeric_vars.png"), p2, width = 8, height = 6)
# 3. Univariate Categorical Plot
p3 <- df_clean %>%
  select(gender, race_ethnicity, program_type, case_management_intensity, referral_source) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(value)) +
  geom_bar(fill = "coral", color = "black") +
  facet_wrap(~name, scales = "free", ncol=2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Categorical Variables")
ggsave(file.path(plot_dir, "03_categorical_vars.png"), p3, width = 10, height = 8)
# 4. Bivariate: Outcome vs Categorical
p4 <- df_clean %>%
  count(exit_status, program_type) %>%
  group_by(program_type) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(program_type, prop, fill = exit_status)) +
  geom_col(position = "fill") +
  coord_flip() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Exit Status by Program Type", x = "Program Type", y = "Proportion")
ggsave(file.path(plot_dir, "04_outcome_vs_program.png"), p4, width = 8, height = 5)
# 5. Bivariate: Outcome vs Numeric (Length of Stay)
p5 <- df_clean %>%
  ggplot(aes(exit_status, length_of_stay, fill = exit_status)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Length of Stay by Exit Status", x = "Exit Status", y = "Length of Stay (Days)")
ggsave(file.path(plot_dir, "05_outcome_vs_stay.png"), p5, width = 6, height = 5)
# 6. Correlation Analysis
numeric_df <- df_clean %>% select(where(is.numeric)) %>% select(-client_id)
cor_matrix <- cor(numeric_df, use = "complete.obs")
cor_df <- as.data.frame(as.table(cor_matrix))
p6 <- ggplot(cor_df, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Freq, 2)), size = 3) +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Matrix", x = "", y = "", fill = "Correlation")
ggsave(file.path(plot_dir, "06_correlation.png"), p6, width = 8, height = 8)
# Generate summaries for text interpretation
cat("--- SKIM SUMMARY ---\n")
skim(df_clean)
cat("\n--- SEGMENTATION ---\n")
df_clean %>%
  group_by(program_type) %>%
  summarise(
    n = n(),
    avg_stay = mean(length_of_stay, na.rm = TRUE),
    success_rate = mean(exit_status == "Permanent", na.rm = TRUE)
  ) %>%
  print()
