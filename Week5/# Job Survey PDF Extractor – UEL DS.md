# Job Survey PDF Extractor – UEL DS-7006

This R script extracts **Table 2.1 (Job Survey data)** from pages 2 and 3 of the **UEL-DS-7006 Quantitative Data Analysis PDF**.

## 📦 Features

- Parses numerical responses from structured table text.
- Applies fallback logic to handle line inconsistencies.
- Validates expected structure (34 rows × 24 variables).
- Outputs:
  - `job_survey_data.csv`: Clean dataset
  - `job_survey_data_variables.csv`: Variable descriptions
- Provides:
  - Summary statistics
  - Structure preview
  - Missing value report
  - Basic quality checks

## 🛠 Requirements

- R 4.1+
- R Packages:
  - `pdftools`
  - `tidyverse`
  - `pacman` (for loading dependencies)

Install dependencies:

```r
if (!require(pacman)) install.packages("pacman")
pacman::p_load(pdftools, tidyverse)
