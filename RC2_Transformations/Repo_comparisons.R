rm(list = ls())

library(tidyverse)
library(janitor)
library(stringr)

dir.create("Data/Yakima_repo", recursive = TRUE, showWarnings = FALSE)
dir.create("Results/Repo_Comparison", recursive = TRUE, showWarnings = FALSE)

# ============================================================
# Download and unzip repo data
# ============================================================

zip_url <- "https://github.com/danczakre/Yakima-River-Basin-Functional-Diversity/raw/main/Data%20for%20Manuscript.zip"
zip_file <- "Data/Yakima_repo/Data_for_Manuscript.zip"

if (!file.exists(zip_file)) {
  download.file(zip_url, zip_file, mode = "wb")
}

unzip(zip_file, exdir = "Data/Yakima_repo", overwrite = TRUE)

repo_files <- list.files(
  "Data/Yakima_repo",
  recursive = TRUE,
  full.names = TRUE
)

cat("\nRepo files found:\n")
print(basename(repo_files))

repo_data_file <- repo_files[str_detect(basename(repo_files), "^Processed_RC2_Hawkes_Data\\.csv$")]
repo_trans_file <- repo_files[str_detect(basename(repo_files), "^RC2_Hawkes_Trans_Profiles\\.csv$")]
repo_catch_file <- repo_files[str_detect(basename(repo_files), "^Rc2_allsites_0717\\.csv$")]
repo_meta_file <- repo_files[str_detect(basename(repo_files), "^RC2 Temporal Study Responses")]

if (length(repo_data_file) == 0) stop("Could not find Processed_RC2_Hawkes_Data.csv")
if (length(repo_trans_file) == 0) stop("Could not find RC2_Hawkes_Trans_Profiles.csv")
if (length(repo_catch_file) == 0) stop("Could not find Rc2_allsites_0717.csv")
if (length(repo_meta_file) == 0) stop("Could not find RC2 Temporal Study Responses metadata file")

repo_data_file <- repo_data_file[1]
repo_trans_file <- repo_trans_file[1]
repo_catch_file <- repo_catch_file[1]
repo_meta_file <- repo_meta_file[1]

# ============================================================
# Repo richness and normalized transformations
# Repo method:
# SR = colSums(data)
# TR = colSums(trans)
# Normalized_TR = TR / SR
# ============================================================

repo_data <- read.csv(
  repo_data_file,
  row.names = 1,
  check.names = FALSE
)

repo_trans <- read.csv(
  repo_trans_file,
  row.names = 1,
  check.names = FALSE
)

repo_trans <- repo_trans[, -1]
colnames(repo_trans) <- gsub("Sample_", "", colnames(repo_trans))

repo_data[repo_data > 0] <- 1

common_repo_samples <- intersect(colnames(repo_data), colnames(repo_trans))

repo_data <- repo_data[, common_repo_samples, drop = FALSE]
repo_trans <- repo_trans[, common_repo_samples, drop = FALSE]

repo_div <- tibble(
  Sample_ID_repo = common_repo_samples,
  repo_richness = colSums(repo_data, na.rm = TRUE),
  repo_total_transformations = colSums(repo_trans, na.rm = TRUE)
) %>%
  mutate(
    repo_normalized_transformations = repo_total_transformations / repo_richness,
    repo_site_4digit = str_extract(Sample_ID_repo, "00[0-9][0-9]"),
    repo_icr_rep = str_extract(Sample_ID_repo, "ICR\\.[0-9]"),
    repo_rep = str_extract(repo_icr_rep, "[0-9]$")
  ) %>%
  filter(!is.na(repo_site_4digit), !is.na(repo_rep))

# ============================================================
# Repo basin area
# ============================================================

repo_meta <- read.csv(
  repo_meta_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
) %>%
  clean_names()

repo_catch <- read.csv(
  repo_catch_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
) %>%
  clean_names()

cat("\nRepo catchment columns:\n")
print(names(repo_catch))

repo_meta <- repo_meta %>%
  mutate(
    site_vial_id_4_digit_numeric_code = str_pad(
      as.character(site_vial_id_4_digit_numeric_code),
      width = 4,
      side = "left",
      pad = "0"
    )
  )

repo_catch_temporal <- repo_catch %>%
  filter(study == "Temporal") %>%
  select(
    study,
    name,
    repo_basin_area_km2 = tot_da_sq_km,
    d50_m
  ) %>%
  mutate(
    repo_basin_area_km2 = parse_number(as.character(repo_basin_area_km2)),
    name = case_when(
      str_detect(name, "American") ~ "American River",
      str_detect(name, "Union Gap") ~ "Union Gap",
      str_detect(name, "Little Naches") ~ "Little Naches",
      str_detect(name, "Mabton") ~ "Mabton",
      str_detect(name, "Kiona") ~ "Kiona",
      str_detect(name, "Craig Road 1") ~ "Naches- Craig Road 1",
      str_detect(name, "Craig Road 2") ~ "Naches- Craig Road 2",
      TRUE ~ name
    )
  )

repo_site_basin <- repo_meta %>%
  left_join(repo_catch_temporal, by = c("location" = "name")) %>%
  transmute(
    repo_site_4digit = site_vial_id_4_digit_numeric_code,
    repo_location = location,
    repo_basin_area_km2 = repo_basin_area_km2
  ) %>%
  distinct()

repo_values <- repo_div %>%
  left_join(repo_site_basin, by = "repo_site_4digit")

# ============================================================
# Your current water-only values
# ============================================================

your_geospatial <- read.csv(
  "https://raw.githubusercontent.com/river-corridors-sfa/Geospatial_variables/refs/heads/main/Archived_versions/v4_RCSFA_Extracted_Geospatial_Data_2025-01-31.csv",
  stringsAsFactors = FALSE
) %>%
  transmute(
    Site_ID = site,
    your_basin_area_km2 = parse_number(as.character(totdasqkm))
  )

your_metadata <- read.csv(
  "v3_RC2_TemporalStudy_2021_2022_SampleData/v3_RC2_Sample_Field_Metadata.csv",
  stringsAsFactors = FALSE
) %>%
  transmute(
    Parent_ID,
    Site_ID
  ) %>%
  left_join(your_geospatial, by = "Site_ID")

your_values <- read_csv(
  "output/Total_Transformations_normalized_7_2_26.csv",
  show_col_types = FALSE
) %>%
  clean_names() %>%
  rename(
    Sample_ID = any_of(c("sample_id", "sampleid", "sample")),
    your_richness = any_of(c("num_peak", "number_of_peaks")),
    your_total_transformations = any_of(c("n_transformations", "total_transformations")),
    your_normalized_transformations = any_of(c(
      "normalized_transformations",
      "norm_transformations",
      "normalized_total_transformations"
    ))
  ) %>%
  filter(str_detect(Sample_ID, "ICR")) %>%
  mutate(
    your_rep = str_extract(Sample_ID, "(?<=-)[0-9]+$"),
    Parent_ID = Sample_ID %>%
      str_remove("-(\\d+)$") %>%
      str_remove("_ICR$"),
    your_site_4digit = str_extract(Parent_ID, "00[0-9][0-9]")
  ) %>%
  left_join(your_metadata, by = "Parent_ID")

# ============================================================
# Sample-level comparison
# ============================================================

sample_comparison <- your_values %>%
  left_join(
    repo_values,
    by = c(
      "your_site_4digit" = "repo_site_4digit",
      "your_rep" = "repo_rep"
    )
  ) %>%
  mutate(
    richness_difference = your_richness - repo_richness,
    total_transformations_difference =
      your_total_transformations - repo_total_transformations,
    normalized_transformations_difference =
      your_normalized_transformations - repo_normalized_transformations,
    basin_area_difference_km2 =
      your_basin_area_km2 - repo_basin_area_km2
  ) %>%
  select(
    Sample_ID,
    Parent_ID,
    Site_ID,
    your_site_4digit,
    your_rep,
    Sample_ID_repo,
    repo_location,
    
    your_richness,
    repo_richness,
    richness_difference,
    
    your_total_transformations,
    repo_total_transformations,
    total_transformations_difference,
    
    your_normalized_transformations,
    repo_normalized_transformations,
    normalized_transformations_difference,
    
    your_basin_area_km2,
    repo_basin_area_km2,
    basin_area_difference_km2
  )

write_csv(
  sample_comparison,
  "Results/Repo_Comparison/water_sample_level_comparison.csv"
)

# ============================================================
# Site-level comparison
# ============================================================

site_comparison <- sample_comparison %>%
  group_by(
    Site_ID,
    your_site_4digit,
    repo_location,
    your_basin_area_km2,
    repo_basin_area_km2
  ) %>%
  summarise(
    n_your_samples = sum(!is.na(your_richness)),
    n_repo_samples = sum(!is.na(repo_richness)),
    
    your_mean_richness = mean(your_richness, na.rm = TRUE),
    repo_mean_richness = mean(repo_richness, na.rm = TRUE),
    richness_difference = your_mean_richness - repo_mean_richness,
    
    your_mean_total_transformations =
      mean(your_total_transformations, na.rm = TRUE),
    repo_mean_total_transformations =
      mean(repo_total_transformations, na.rm = TRUE),
    total_transformations_difference =
      your_mean_total_transformations - repo_mean_total_transformations,
    
    your_mean_normalized_transformations =
      mean(your_normalized_transformations, na.rm = TRUE),
    repo_mean_normalized_transformations =
      mean(repo_normalized_transformations, na.rm = TRUE),
    normalized_transformations_difference =
      your_mean_normalized_transformations - repo_mean_normalized_transformations,
    
    basin_area_difference_km2 =
      mean(your_basin_area_km2, na.rm = TRUE) -
      mean(repo_basin_area_km2, na.rm = TRUE),
    
    .groups = "drop"
  )

write_csv(
  site_comparison,
  "Results/Repo_Comparison/water_site_level_comparison.csv"
)

# ============================================================
# Summary checks
# ============================================================

cat("\nSample-level comparison preview:\n")
print(sample_comparison)

cat("\nSite-level comparison preview:\n")
print(site_comparison)

cat("\nUnmatched sample rows:\n")
unmatched_samples <- sample_comparison %>%
  filter(
    is.na(repo_richness) |
      is.na(repo_total_transformations) |
      is.na(repo_normalized_transformations)
  ) %>%
  select(
    Sample_ID,
    Parent_ID,
    Site_ID,
    your_site_4digit,
    your_rep
  )

print(unmatched_samples)

cat("\nRows with basin-area differences:\n")
basin_differences <- site_comparison %>%
  filter(!is.na(basin_area_difference_km2)) %>%
  filter(abs(basin_area_difference_km2) > 0.001) %>%
  select(
    Site_ID,
    your_site_4digit,
    repo_location,
    your_basin_area_km2,
    repo_basin_area_km2,
    basin_area_difference_km2
  )

print(basin_differences)

cat("\nDone.\n")
cat("Wrote sample-level comparison to:\n")
cat("Results/Repo_Comparison/water_sample_level_comparison.csv\n")
cat("Wrote site-level comparison to:\n")
cat("Results/Repo_Comparison/water_site_level_comparison.csv\n")

# ============================================================
# Cleaner 1:1 scatterplots to compare your values against repo values
# ============================================================

dir.create("Figures/Repo_Comparison", recursive = TRUE, showWarnings = FALSE)

theme_compare <- theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(face = "bold", size = 11),
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    aspect.ratio = 1
  )

get_lims <- function(x, y) {
  vals <- c(x, y)
  vals <- vals[is.finite(vals)]
  rng <- range(vals, na.rm = TRUE)
  pad <- diff(rng) * 0.08
  
  if (pad == 0) {
    pad <- max(abs(rng), na.rm = TRUE) * 0.08
  }
  
  c(rng[1] - pad, rng[2] + pad)
}

plot_1to1 <- function(dat, xvar, yvar, colorvar, title, xlab, ylab) {
  plot_dat <- dat %>%
    filter(
      is.finite({{ xvar }}),
      is.finite({{ yvar }})
    )
  
  lims <- get_lims(
    pull(plot_dat, {{ xvar }}),
    pull(plot_dat, {{ yvar }})
  )
  
  ggplot(
    plot_dat,
    aes(x = {{ xvar }}, y = {{ yvar }}, color = {{ colorvar }})
  ) +
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      color = "grey35",
      linewidth = 0.7
    ) +
    geom_point(size = 2.6, alpha = 0.85) +
    geom_smooth(
      aes(group = 1),
      method = "lm",
      formula = y ~ x,
      se = TRUE,
      color = "black",
      fill = "grey75",
      linewidth = 0.7,
      alpha = 0.35
    ) +
    scale_x_continuous(limits = lims) +
    scale_y_continuous(limits = lims) +
    labs(
      title = title,
      x = xlab,
      y = ylab,
      color = "Site"
    ) +
    theme_compare
}

# ============================================================
# Sample-level plots
# ============================================================

p_sample_richness <- plot_1to1(
  dat = sample_comparison,
  xvar = repo_richness,
  yvar = your_richness,
  colorvar = Site_ID,
  title = "Sample richness",
  xlab = "Repo richness",
  ylab = "Your richness"
)

p_sample_total_trans <- plot_1to1(
  dat = sample_comparison,
  xvar = repo_total_transformations,
  yvar = your_total_transformations,
  colorvar = Site_ID,
  title = "Sample total transformations",
  xlab = "Repo total transformations",
  ylab = "Your total transformations"
)

p_sample_norm_trans <- plot_1to1(
  dat = sample_comparison,
  xvar = repo_normalized_transformations,
  yvar = your_normalized_transformations,
  colorvar = Site_ID,
  title = "Sample normalized transformations",
  xlab = "Repo normalized transformations",
  ylab = "Your normalized transformations"
)

p_sample_basin <- plot_1to1(
  dat = sample_comparison,
  xvar = repo_basin_area_km2,
  yvar = your_basin_area_km2,
  colorvar = Site_ID,
  title = "Sample basin area",
  xlab = expression(Repo~basin~area~(km^2)),
  ylab = expression(Your~basin~area~(km^2))
)

fig_sample_compare <- (
  p_sample_richness + p_sample_total_trans +
    p_sample_norm_trans + p_sample_basin
) +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")

print(fig_sample_compare)

ggsave(
  "Figures/Repo_Comparison/sample_level_1to1_comparison_clean.png",
  fig_sample_compare,
  width = 8.5,
  height = 8.5,
  dpi = 300
)

ggsave(
  "Figures/Repo_Comparison/sample_level_1to1_comparison_clean.pdf",
  fig_sample_compare,
  width = 8.5,
  height = 8.5
)

# ============================================================
# Site-level plots
# ============================================================

p_site_richness <- plot_1to1(
  dat = site_comparison,
  xvar = repo_mean_richness,
  yvar = your_mean_richness,
  colorvar = Site_ID,
  title = "Site mean richness",
  xlab = "Repo mean richness",
  ylab = "Your mean richness"
)

p_site_total_trans <- plot_1to1(
  dat = site_comparison,
  xvar = repo_mean_total_transformations,
  yvar = your_mean_total_transformations,
  colorvar = Site_ID,
  title = "Site mean total transformations",
  xlab = "Repo mean total transformations",
  ylab = "Your mean total transformations"
)

p_site_norm_trans <- plot_1to1(
  dat = site_comparison,
  xvar = repo_mean_normalized_transformations,
  yvar = your_mean_normalized_transformations,
  colorvar = Site_ID,
  title = "Site mean normalized transformations",
  xlab = "Repo mean normalized transformations",
  ylab = "Your mean normalized transformations"
)

p_site_basin <- plot_1to1(
  dat = site_comparison,
  xvar = repo_basin_area_km2,
  yvar = your_basin_area_km2,
  colorvar = Site_ID,
  title = "Site basin area",
  xlab = expression(Repo~basin~area~(km^2)),
  ylab = expression(Your~basin~area~(km^2))
)

fig_site_compare <- (
  p_site_richness + p_site_total_trans +
    p_site_norm_trans + p_site_basin
) +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")

print(fig_site_compare)

ggsave(
  "Figures/Repo_Comparison/site_level_1to1_comparison_clean.png",
  fig_site_compare,
  width = 8.5,
  height = 8.5,
  dpi = 300
)

ggsave(
  "Figures/Repo_Comparison/site_level_1to1_comparison_clean.pdf",
  fig_site_compare,
  width = 8.5,
  height = 8.5
)

# ============================================================
# Also save each plot separately for easier inspection
# ============================================================

ggsave("Figures/Repo_Comparison/sample_richness_1to1_clean.png", p_sample_richness, width = 4.5, height = 4.5, dpi = 300)
ggsave("Figures/Repo_Comparison/sample_total_transformations_1to1_clean.png", p_sample_total_trans, width = 4.5, height = 4.5, dpi = 300)
ggsave("Figures/Repo_Comparison/sample_normalized_transformations_1to1_clean.png", p_sample_norm_trans, width = 4.5, height = 4.5, dpi = 300)
ggsave("Figures/Repo_Comparison/sample_basin_area_1to1_clean.png", p_sample_basin, width = 4.5, height = 4.5, dpi = 300)

ggsave("Figures/Repo_Comparison/site_richness_1to1_clean.png", p_site_richness, width = 4.5, height = 4.5, dpi = 300)
ggsave("Figures/Repo_Comparison/site_total_transformations_1to1_clean.png", p_site_total_trans, width = 4.5, height = 4.5, dpi = 300)
ggsave("Figures/Repo_Comparison/site_normalized_transformations_1to1_clean.png", p_site_norm_trans, width = 4.5, height = 4.5, dpi = 300)
ggsave("Figures/Repo_Comparison/site_basin_area_1to1_clean.png", p_site_basin, width = 4.5, height = 4.5, dpi = 300)

cat("\nClean 1:1 comparison figures written to Figures/Repo_Comparison.\n")