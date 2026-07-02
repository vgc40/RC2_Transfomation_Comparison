# ==== Loading libraries =========

rm(list = ls(all = TRUE))

library(stringr)
library(readr)
library(tidyverse)
library(readxl)
library(dplyr)
library(janitor)

# ==== Defining paths and working directories ======

files <- list.files(
  path = "Archive 3",
  pattern = ".csv",
  full.names = TRUE
)

samples <- files

# ===== Data cleaning settings ======

options(digits = 12)

mass_min <- 200
mass_max <- 900

# ===== CM column header format ======

cm_cols <- c(
  "Index",
  "Mass",
  "Calibrated_Mass",
  "Calculated_Mass",
  "Peak_Height",
  "Peak_Area",
  "Resolving_Power",
  "S_N",
  "Ion_Charge",
  "Error_ppm",
  "Error_Score",
  "Isotopologue_Similarity",
  "Confidence_Score",
  "DBE",
  "HtoC_ratio",
  "OtoC_ratio",
  "Heteroatom_Class",
  "Ion_Type",
  "Is_Isotopologue",
  "Mono_Isotopic_Index",
  "Molecular_Formula",
  "C",
  "H",
  "O",
  "N",
  "P",
  "S",
  "13C",
  "17O",
  "18O",
  "33S",
  "34S"
)

# ===== Function to standardize RC2 headers to CM headers ======

standardize_to_cm_headers <- function(df) {
  
  df <- df %>%
    dplyr::rename(
      Mass = any_of("m/z"),
      Calibrated_Mass = any_of("Calibrated m/z"),
      Calculated_Mass = any_of("Calculated m/z"),
      Peak_Height = any_of("Peak Height"),
      Peak_Area = any_of("Peak Area"),
      Resolving_Power = any_of("Resolving Power"),
      S_N = any_of("S/N"),
      Ion_Charge = any_of("Ion Charge"),
      Error_ppm = any_of("m/z Error (ppm)"),
      Error_Score = any_of("m/z Error Score"),
      Isotopologue_Similarity = any_of("Isotopologue Similarity"),
      Confidence_Score = any_of("Confidence Score"),
      HtoC_ratio = any_of("H/C"),
      OtoC_ratio = any_of("O/C"),
      Heteroatom_Class = any_of("Heteroatom Class"),
      Ion_Type = any_of("Ion Type"),
      Is_Isotopologue = any_of("Is Isotopologue"),
      Mono_Isotopic_Index = any_of("Mono Isotopic Index"),
      Molecular_Formula = any_of("Molecular Formula")
    )
  
  # Add CM columns that are absent in RC2 files
  missing_cols <- setdiff(cm_cols, names(df))
  
  for (col in missing_cols) {
    df[[col]] <- NA
  }
  
  # Put columns in CM order, then keep any extra columns at the end
  df <- df %>%
    dplyr::select(
      all_of(cm_cols),
      everything()
    )
  
  return(df)
}

# ===== Clean sample files =====
# Standardizes headers, filters by mass range, removes isotopologues

clean_samples <- list()

cleaning_summary <- data.frame(
  file = character(),
  n_before = integer(),
  n_after_cleaning = integer(),
  n_removed_mass_or_isotopologue = integer(),
  stringsAsFactors = FALSE
)

for (i in seq_along(samples)) {
  
  sample_raw <- read.csv(samples[i], header = TRUE, check.names = FALSE)
  
  sample_standardized <- standardize_to_cm_headers(sample_raw)
  
  sample_i <- sample_standardized %>%
    dplyr::filter(
      Calibrated_Mass >= mass_min,
      Calibrated_Mass <= mass_max
    ) %>%
    dplyr::distinct(Calibrated_Mass, .keep_all = TRUE) %>%
    dplyr::filter(Is_Isotopologue != 1)
  
  clean_samples[[i]] <- sample_i
  
  cleaning_summary[i, ] <- data.frame(
    file = basename(samples[i]),
    n_before = nrow(sample_raw),
    n_after_cleaning = nrow(sample_i),
    n_removed_mass_or_isotopologue = nrow(sample_raw) - nrow(sample_i),
    stringsAsFactors = FALSE
  )
}

names(clean_samples) <- basename(samples)

# ===== Write cleaned samples to CSV =====

dir.create("CoreMS_input", showWarnings = FALSE)

for (i in seq_along(clean_samples)) {
  
  write.csv(
    clean_samples[[i]],
    file = file.path(
      "CoreMS_input",
      basename(samples[i])
    ),
    row.names = FALSE
  )
}

# ===== Print and save cleaning summary =====

cat("\nOverall cleaning summary:\n")
print(cleaning_summary)

write.csv(
  cleaning_summary,
  file = file.path("CoreMS_input", "mass_isotopologue_cleaning_summary.csv"),
  row.names = FALSE
)