# ============================================================================
# Script: job_survey_pdf_extractor.R
# Purpose: Extract Table 2.1 data from pages 2–3 of the UEL-DS-7006 Job Survey PDF
# Author: Mubanga Nsofu
# Course: UEL-DS-7006 Quantitative Data Analysis
# Student ID: R2012D11689258
# License: CC BY-NC-SA 4.0
# Institution: University of East London
# Version: 1.0
# Date: 2025-06-15
# ============================================================================


# Install package manager (pacman) if not already installed----------------

if (!require(pacman)) {
  install.packages("pacman") # Check if package manager is installed 
}

# Manage the installation and loading of pdftools, tidyverse and the dependencies

pacman::p_load(pdftools,tidyverse)


# Function to extract Job Survey data from PDF----------------------
extract_job_survey_from_pdf <- function(pdf_path) {
  
  # Check if file exists
  if (!file.exists(pdf_path)) {
    stop("PDF file not found at specified path: ", pdf_path)
  }
  
  cat("Reading PDF file:", pdf_path, "\n")
  
  # Read the PDF
  pdf_text <- pdf_text(pdf_path)
  
  if(length(pdf_text) < 3) {
    stop("PDF does not have enough pages. Expected at least 3 pages.")
  }
  
  cat("PDF loaded successfully. Total pages:", length(pdf_text), "\n")
  
  # Extract pages 2 and 3 which contain Table 2.1
  page2 <- pdf_text[2]
  page3 <- pdf_text[3]
  
  cat("Extracting data from pages 2 and 3...\n")
  
  # Combine pages 2 and 3
  combined_text <- paste(page2, page3, sep = "\n")
  
  # Split into lines
  lines <- strsplit(combined_text, "\n")[[1]]
  
  # Find data lines (lines that start with case numbers followed by data)
  # Pattern matches lines starting with 1-2 digits, then whitespace, then more digits
  data_pattern <- "^\\s*\\d{1,2}\\s+\\d"
  data_lines <- lines[grepl(data_pattern, lines)]
  
  cat("Found", length(data_lines), "potential data lines\n")
  
  if(length(data_lines) == 0) {
    stop("No data lines found in the PDF. Please check the PDF format.")
  }
  
  # Parse each data line
  parsed_data <- list()
  
  for(i in seq_along(data_lines)) {
    line <- trimws(data_lines[i])
    
    # Extract all numbers from the line using regex
    numbers <- as.numeric(unlist(str_extract_all(line, "\\d+")))
    
    # Filter out NA values
    numbers <- numbers[!is.na(numbers)]
    
    # We expect 24 variables, but accept lines with at least 20 to handle variations
    if(length(numbers) >= 20) {
      # Take exactly 24 numbers (pad with NA if fewer, truncate if more)
      if(length(numbers) >= 24) {
        parsed_data[[i]] <- numbers[1:24]
      } else {
        # Pad with NA to reach 24 variables
        parsed_data[[i]] <- c(numbers, rep(NA, 24 - length(numbers)))
      }
    } else {
      cat("Skipping line", i, "- insufficient data points:", length(numbers), "\n")
    }
  }
  
  # Remove NULL entries
  parsed_data <- parsed_data[!sapply(parsed_data, is.null)]
  
  if(length(parsed_data) == 0) {
    stop("No valid data rows extracted. Please check the PDF format and data structure.")
  }
  
  cat("Successfully parsed", length(parsed_data), "data rows\n")
  
  # Convert to matrix
  data_matrix <- do.call(rbind, parsed_data)
  
  # Create data frame
  job_data <- as.data.frame(data_matrix)
  
  # Add proper variable names from Table 2.2
  var_names <- c("id", "ethnicgp", "gender", "income", "age", "years", "commit",
                 "satis1", "satis2", "satis3", "satis4",
                 "autonom1", "autonom2", "autonom3", "autonom4", 
                 "routine1", "routine2", "routine3", "routine4",
                 "attend", "skill", "prody", "qual", "absence")
  
  names(job_data) <- var_names
  
  cat("Dataset created with", nrow(job_data), "cases and", ncol(job_data), "variables\n")
  
  return(job_data)
}

# Enhanced extraction function with better parsing-----------------------
extract_job_survey_advanced <- function(pdf_path) {
  
  if (!file.exists(pdf_path)) {
    stop("PDF file not found at specified path: ", pdf_path)
  }
  
  cat("Advanced extraction from PDF:", pdf_path, "\n")
  
  # Read the PDF
  pdf_text <- pdf_text(pdf_path)
  
  # Focus on pages 2 and 3
  if(length(pdf_text) < 3) {
    stop("PDF must have at least 3 pages")
  }
  
  # Process each page separately for better control
  all_data_rows <- list()
  
  for(page_num in 2:3) {
    cat("Processing page", page_num, "...\n")
    
    page_text <- pdf_text[page_num]
    lines <- strsplit(page_text, "\n")[[1]]
    
    # More specific pattern for data rows
    # Look for lines that start with case numbers (01-34)
    for(line in lines) {
      line <- trimws(line)
      
      # Skip empty lines and headers
      if(nchar(line) < 10) next
      
      # Check if line starts with a case number
      if(grepl("^\\d{1,2}\\s+", line)) {
        
        # Extract all numeric values
        # Use a more comprehensive regex that handles different spacing
        numeric_parts <- str_extract_all(line, "\\b\\d+\\b")[[1]]
        numbers <- as.numeric(numeric_parts)
        
        # Remove any NA values
        numbers <- numbers[!is.na(numbers)]
        
        # Validate we have enough data points
        if(length(numbers) >= 20) {
          # Ensure exactly 24 variables
          if(length(numbers) >= 24) {
            all_data_rows[[length(all_data_rows) + 1]] <- numbers[1:24]
          } else {
            # Pad with NA
            padded <- c(numbers, rep(NA, 24 - length(numbers)))
            all_data_rows[[length(all_data_rows) + 1]] <- padded
          }
        }
      }
    }
  }
  
  if(length(all_data_rows) == 0) {
    stop("No data rows could be extracted from the PDF")
  }
  
  # Convert to data frame
  data_matrix <- do.call(rbind, all_data_rows)
  job_data <- as.data.frame(data_matrix)
  
  # Add variable names
  var_names <- c("id", "ethnicgp", "gender", "income", "age", "years", "commit",
                 "satis1", "satis2", "satis3", "satis4",
                 "autonom1", "autonom2", "autonom3", "autonom4", 
                 "routine1", "routine2", "routine3", "routine4",
                 "attend", "skill", "prody", "qual", "absence")
  
  names(job_data) <- var_names
  
  cat("Advanced extraction completed:", nrow(job_data), "cases extracted\n")
  
  return(job_data)
}

# Main function to extract data and export to CSV-------------------
extract_and_export_job_survey <- function(pdf_path, output_file = "job_survey_data.csv") {
  
  cat("=== Automated PDF Data Extraction ===\n\n")
  
  # Try primary extraction method
  job_survey_df <- tryCatch({
    extract_job_survey_from_pdf(pdf_path)
  }, error = function(e) {
    cat("Primary extraction failed:", e$message, "\n")
    cat("Trying advanced extraction method...\n")
    
    # Try advanced method
    tryCatch({
      extract_job_survey_advanced(pdf_path)
    }, error = function(e2) {
      stop("Both extraction methods failed. Error: ", e2$message)
    })
  })
  
  # Validate the extracted data
  cat("\n=== Data Validation ===\n")
  
  # Check dimensions
  expected_cases <- 34
  expected_vars <- 24
  
  cat("Expected: 34 cases, 24 variables\n")
  cat("Extracted:", nrow(job_survey_df), "cases,", ncol(job_survey_df), "variables\n")
  
  if(nrow(job_survey_df) != expected_cases) {
    warning("Number of cases (", nrow(job_survey_df), ") does not match expected (", expected_cases, ")")
  }
  
  if(ncol(job_survey_df) != expected_vars) {
    warning("Number of variables (", ncol(job_survey_df), ") does not match expected (", expected_vars, ")")
  }
  
  # Display dataset information
  cat("\n=== Dataset Summary ===\n")
  cat("Dimensions:", nrow(job_survey_df), "rows ×", ncol(job_survey_df), "columns\n")
  cat("Variables:", paste(names(job_survey_df), collapse = ", "), "\n\n")
  
  # Show first few rows
  cat("=== First 6 rows ===\n")
  print(head(job_survey_df))
  
  # Show structure
  cat("\n=== Dataset Structure ===\n")
  str(job_survey_df)
  
  # Show summary statistics
  cat("\n=== Summary Statistics ===\n")
  print(summary(job_survey_df))
  
  # Save to CSV
  write.csv(job_survey_df, file = output_file, row.names = FALSE)
  cat("\n=== Export Complete ===\n")
  cat("Dataset saved as:", output_file, "\n")
  
  if(file.exists(output_file)) {
    cat("File size:", round(file.size(output_file) / 1024, 2), "KB\n")
  }
  
  # Create variable descriptions file
  variable_descriptions <- data.frame(
    Variable = names(job_survey_df),
    Description = c("Identification number", "Ethnic group", "Gender", 
                    "Gross annual income", "Age", "Years worked", 
                    "Organizational commitment",
                    "Job-satisfaction scale - Item 1", "Job-satisfaction scale - Item 2",
                    "Job-satisfaction scale - Item 3", "Job-satisfaction scale - Item 4",
                    "Job-autonomy scale - Item 1", "Job-autonomy scale - Item 2", 
                    "Job-autonomy scale - Item 3", "Job-autonomy scale - Item 4",
                    "Job-routine scale - Item 1", "Job-routine scale - Item 2",
                    "Job-routine scale - Item 3", "Job-routine scale - Item 4",
                    "Attendance at meeting", "Rated skill", "Rated productivity",
                    "Rated quality", "Absenteeism"),
    stringsAsFactors = FALSE
  )
  
  # Save variable descriptions
  desc_file <- gsub("\\.csv$", "_variables.csv", output_file)
  write.csv(variable_descriptions, file = desc_file, row.names = FALSE)
  cat("Variable descriptions saved as:", desc_file, "\n")
  
  # Data quality checks
  cat("\n=== Data Quality Checks ===\n")
  
  # Check for missing values
  missing_counts <- colSums(is.na(job_survey_df))
  if(any(missing_counts > 0)) {
    cat("Variables with missing values:\n")
    print(missing_counts[missing_counts > 0])
  } else {
    cat("No missing values detected\n")
  }
  
  # Check for potential coding issues
  cat("\nPotential data issues to review:\n")
  cat("- Income values of 0:", sum(job_survey_df$income == 0, na.rm = TRUE), "cases\n")
  cat("- Absence values > 50:", sum(job_survey_df$absence > 50, na.rm = TRUE), "cases\n")
  
  cat("\n=== Ready for Analysis ===\n")
  cat("Files created:\n")
  cat("- ", output_file, " (main dataset)\n")
  cat("- ", desc_file, " (variable descriptions)\n")
  cat("\nDataset is ready for statistical analysis!\n")
  
  # Return the dataframe
  return(job_survey_df)
}

# Usage instructions-----------------------
cat("=== Usage Instructions ===\n")
cat("To extract data from your PDF file:\n\n")
cat("# Basic usage:\n")
cat("job_data <- extract_and_export_job_survey('path/to/your/pdf_file.pdf')\n\n")
cat("# With custom output filename:\n")
cat("job_data <- extract_and_export_job_survey('path/to/your/pdf_file.pdf', 'my_data.csv')\n\n")
cat("# Example:\n")
cat("job_data <- extract_and_export_job_survey('UEL-DS-7006_document.pdf')\n\n")

cat("Replace 'path/to/your/pdf_file.pdf' with the actual path to your PDF file.\n")
cat("The function will automatically extract Table 2.1 from pages 2-3 and export to CSV.\n")

