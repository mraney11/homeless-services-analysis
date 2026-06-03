# Homeless Services Outcome Analysis

A data analytics portfolio project evaluating the effectiveness of homeless services programs and identifying the primary drivers of successful permanent housing placement.

---

# Overview

Nonprofit organizations providing homeless services operate with tight budgets and face the constant challenge of placing vulnerable individuals into permanent housing. Frontline agencies must make critical resource allocation decisions, yet they often lack quantitative evidence on which interventions most effectively drive housing stability.

This project bridges the gap between administrative data and strategic operations. By analyzing client pathways, program durations, and case management models, this analysis provides nonprofit leadership with evidence-based recommendations.

The analysis identifies the operational drivers of successful housing transitions, allowing leaders to:

* Optimize staffing levels
* Improve program efficiency
* Target capital investments
* Support grant applications with data-driven findings

---

# Dataset

The analysis uses a refined administrative dataset consisting of client intake, engagement, and exit records.

## Dataset Details

* **Source:** Simulated administrative records reflecting real-world homeless services operations
* **Size:** 150 client records, 20 columns

## Key Variables

### Outcome Variable

* `exit_status`

  * `Permanent` = successful permanent housing placement
  * `Other` = temporary housing, shelter, or return to homelessness

### Demographics

* `age`
* `gender`
* `veteran_status`
* `disability_status`

### Client History

* `prior_episodes`

  * Number of prior homeless episodes within the last 3 years

### Programmatic & Operational Variables

* `program_type`

  * Emergency Shelter
  * Rapid Rehousing
  * Transitional Housing
  * Permanent Supportive Housing

* `length_of_stay`

* `services_received`

* `case_management_intensity`

---

# Project Structure

```text
homeless_services_project/
│
├── data/
│   ├── homeless_services_raw.csv
│   └── homeless_services_clean.csv
│
├── scripts/
│   ├── audit.R
│   ├── clean.R
│   ├── eda.R
│   └── modeling.R
│
├── report/
│   ├── homeless_services_analysis.qmd
│   ├── homeless_services_analysis.html
│   ├── project_framing_and_analysis_plan.md
│   ├── data_intake_and_screening.md
│   ├── data_cleaning_report.md
│   ├── modeling_analysis.md
│   └── executive_summary_printable.pdf
│
├── outputs/
│   ├── logit_results.csv
│   ├── linear_results.csv
│   ├── model_performance_table.csv
│   ├── model_linear_diagnostics.png
│   └── figures/
│       ├── 02_numeric_vars.png
│       ├── 03_categorical_vars.png
│       ├── 04_outcome_vs_program.png
│       ├── 05_outcome_vs_stay.png
│       └── 06_correlation.png
│
└── README.md
```

---

# Methodology

## 1. Data Screening

Conducted structural checks to:

* Validate `client_id` integrity
* Check date ranges
* Identify negative or impossible values
* Evaluate missing data patterns
* Review missingness in:

  * `housing_placement_days`
  * `monthly_income_at_exit`

## 2. Data Cleaning

Key cleaning steps included:

* Standardizing inconsistent text formatting in `program_type`
* Correcting capitalization and spacing inconsistencies
* Applying logical imputations where appropriate
* Retaining NA values for sensitive income fields to reduce model bias

## 3. Exploratory Data Analysis (EDA)

EDA focused on:

* Housing outcome distributions
* Program-level success rates
* Length-of-stay comparisons
* Correlation analysis among numeric variables
* Identification of possible multicollinearity

## 4. Statistical Modeling

### Primary Model: Logistic Regression

Modeled the probability of a successful permanent housing placement using:

* Age
* Program type
* Length of stay
* Income at entry

### Secondary Model: Linear Regression

Modeled predictors of program stay duration (`length_of_stay`).

### Model Diagnostics

Diagnostic procedures included:

* Residual analysis
* Model fit evaluation
* Assessment of regression assumptions

---

# Key Insights

## Case Management is the Primary Actionable Lever

High-intensity case management was strongly associated with successful permanent housing placement outcomes.

## Permanent Supportive Housing (PSH) Performs Best

Permanent Supportive Housing achieved a placement success rate of approximately **96.4%**, though with longer average stays.

## Transitional Housing and Rapid Rehousing Are Highly Efficient

* Transitional Housing: **82.7%** success rate
* Rapid Rehousing: **77.8%** success rate

Both achieved strong outcomes with substantially shorter program durations.

## Time in Program Matters

Longer program participation was associated with improved housing stability outcomes.

## Attrition Risk Among Chronically Unhoused Clients

Clients with multiple prior homelessness episodes tended to have shorter stays and increased dropout risk.

---

# Tools Used

* Antigravity / Gemini
* ChatGPT
* R
* RStudio
* Quarto
* Tidyverse
* broom
* skimr
* janitor

---

# How to Run the Project

This project uses a structured 9-prompt workflow executed sequentially in Antigravity/Gemini.

## Prompt Workflow

```text
01_project_overview.md
02_data_screening_master.md
03_data_cleaning_master.md
04_eda_master.md
05_modeling_master.md
06_quarto_report_master.md
07_readme_master.md
08_code_review_master.md
09_final_qa_packaging.md
```

## Prerequisites

Install:

* R
* RStudio
* Quarto

## Required R Packages

```r
install.packages(c(
  "tidyverse",
  "broom",
  "skimr",
  "here",
  "janitor"
))
```

---

# Workflow Notes

* Prompts 02–05 generate executable R scripts
* Each script is debugged individually in RStudio
* Scripts are reviewed using ChatGPT QA prompts
* Final reporting is rendered through Quarto

Generated scripts include:

| Prompt | Script               |
| ------ | -------------------- |
| 02     | `scripts/audit.R`    |
| 03     | `scripts/clean.R`    |
| 04     | `scripts/eda.R`      |
| 05     | `scripts/modeling.R` |

---

# Final Report

Render the Quarto report:

```text
report/homeless_services_analysis.qmd
```

Final output:

```text
report/homeless_services_analysis.html
```

---

# Author

**Michael Raney**
Nonprofit Data Analyst
