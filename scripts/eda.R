library(tidyverse)
library(skimr)
library(here)

# Paths
file_path <- here::here("data", "homeless_services_clean.csv")
plot_dir <- here::here("outputs", "figures")
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

df_clean <- readr::read_csv(file_path, show_col_types = FALSE)

# Define clean, cohesive color palette for program outcomes
palette_exit <- c(
  "Permanent" = "#0d9488",                # Soft teal for success
  "Temporary" = "#f59e0b",                # Amber for transition
  "Returned to Homelessness" = "#ef4444"  # Rose for return to homelessness
)

# Standard premium theme function for styling consistent visuals
theme_premium <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(face = "bold", size = 14, margin = margin(b = 6), hjust = 0.5),
      plot.subtitle = element_text(size = 10, color = "gray30", margin = margin(b = 10), hjust = 0.5),
      axis.title = element_text(face = "bold", size = 10, color = "gray20"),
      axis.text = element_text(size = 9, color = "gray30"),
      strip.text = element_text(face = "bold", size = 10, color = "gray20"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray95"),
      legend.title = element_text(face = "bold", size = 9),
      legend.text = element_text(size = 8),
      plot.margin = margin(12, 12, 12, 12)
    )
}

# 1. Exit Status Plot
p1 <- df_clean %>%
  count(exit_status) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(x = exit_status, y = percent, fill = exit_status)) +
  geom_col(alpha = 0.9, show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = palette_exit) +
  theme_premium() +
  labs(
    title = "Distribution of Exit Status",
    subtitle = "Percentage of clients by final program outcome",
    x = "Exit Status",
    y = "Percentage"
  )
ggsave(here::here("outputs", "figures", "01_exit_status.png"), p1, width = 6, height = 4)

# 2. Univariate Numeric Plot
p2 <- df_clean %>%
  select(
    `Age` = age,
    `Prior Episodes` = prior_episodes,
    `Length of Stay (Days)` = length_of_stay,
    `Services Received` = services_received,
    `Housing Placement Days` = housing_placement_days,
    `Monthly Income at Exit` = monthly_income_at_exit
  ) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 20, fill = "#3b82f6", color = "white", alpha = 0.85, na.rm = TRUE) +
  facet_wrap(~name, scales = "free") +
  theme_premium() +
  theme(panel.grid.major = element_line(color = "gray92")) +
  labs(
    title = "Distribution of Numeric Variables",
    subtitle = "Histograms showing univariate distributions of operational and client metrics",
    x = "Value",
    y = "Count"
  )
ggsave(here::here("outputs", "figures", "02_numeric_vars.png"), p2, width = 8, height = 6)

# 3. Univariate Categorical Plot
p3 <- df_clean %>%
  select(
    `Gender` = gender,
    `Race/Ethnicity` = race_ethnicity,
    `Program Type` = program_type,
    `Case Management Intensity` = case_management_intensity,
    `Referral Source` = referral_source
  ) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(value)) +
  geom_bar(fill = "#f97316", color = "white", alpha = 0.85, na.rm = TRUE) +
  facet_wrap(~name, scales = "free", ncol = 2) +
  theme_premium() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title = "Distribution of Categorical Variables",
    subtitle = "Bar charts showing univariate distributions of client and program attributes",
    x = "Category",
    y = "Count"
  )
ggsave(here::here("outputs", "figures", "03_categorical_vars.png"), p3, width = 10, height = 8)

# 4. Bivariate: Outcome vs Categorical (Program Type)
p4 <- df_clean %>%
  count(exit_status, program_type) %>%
  group_by(program_type) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = program_type, y = prop, fill = exit_status)) +
  geom_col(position = "fill", alpha = 0.9) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = palette_exit, name = "Exit Status") +
  theme_premium() +
  theme(panel.grid.major.y = element_blank()) +
  labs(
    title = "Exit Status by Program Type",
    subtitle = "Relative placement success rate across program models",
    x = "Program Type",
    y = "Proportion"
  )
ggsave(here::here("outputs", "figures", "04_outcome_vs_program.png"), p4, width = 8, height = 5)

# 5. Bivariate: Outcome vs Numeric (Length of Stay)
p5 <- df_clean %>%
  ggplot(aes(x = exit_status, y = length_of_stay, fill = exit_status)) +
  geom_boxplot(alpha = 0.8, show.legend = FALSE, na.rm = TRUE) +
  scale_fill_manual(values = palette_exit) +
  theme_premium() +
  theme(panel.grid.major.x = element_blank()) +
  labs(
    title = "Length of Stay by Exit Status",
    subtitle = "Comparison of program durations (days) by placement success",
    x = "Exit Status",
    y = "Length of Stay (Days)"
  )
ggsave(here::here("outputs", "figures", "05_outcome_vs_stay.png"), p5, width = 6, height = 5)

# 6. Correlation Analysis
numeric_df <- df_clean %>%
  select(
    `Age` = age,
    `Veteran Status` = veteran_status,
    `Disability Status` = disability_status,
    `Prior Episodes` = prior_episodes,
    `Length of Stay` = length_of_stay,
    `Services Received` = services_received,
    `Mental Health Support` = mental_health_support,
    `Substance Use Support` = substance_use_support,
    `Employment Assistance` = employment_assistance,
    `Housing Placement Days` = housing_placement_days,
    `Monthly Income` = monthly_income_at_exit
  )

cor_matrix <- cor(numeric_df, use = "complete.obs")
cor_df <- as.data.frame(as.table(cor_matrix))

p6 <- ggplot(cor_df, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Freq, 2)), size = 3, fontface = "bold") +
  scale_fill_gradient2(
    low = "#e11d48", 
    high = "#2563eb", 
    mid = "#f8fafc", 
    midpoint = 0, 
    limit = c(-1, 1),
    name = "Correlation"
  ) +
  theme_premium() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank()
  ) +
  labs(
    title = "Correlation Heatmap",
    subtitle = "Pearson correlation coefficients among numeric metrics",
    x = "",
    y = ""
  )
ggsave(here::here("outputs", "figures", "06_correlation.png"), p6, width = 8, height = 8)

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

