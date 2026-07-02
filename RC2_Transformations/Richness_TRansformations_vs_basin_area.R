rm(list = ls())

library(tidyverse)
library(janitor)
library(stringr)
library(patchwork)
library(ggpmisc)

dir.create("Figures/Water_BasinArea", recursive = TRUE, showWarnings = FALSE)
dir.create("Results/Water_BasinArea", recursive = TRUE, showWarnings = FALSE)
dir.create("Data/Yakima_repo", recursive = TRUE, showWarnings = FALSE)

theme_pub <- theme_bw(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

# ============================================================
# Get repo sample list so plots only use samples present in repo
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

repo_data_file <- repo_files[str_detect(basename(repo_files), "^Processed_RC2_Hawkes_Data\\.csv$")][1]
repo_trans_file <- repo_files[str_detect(basename(repo_files), "^RC2_Hawkes_Trans_Profiles\\.csv$")][1]

if (is.na(repo_data_file)) stop("Could not find Processed_RC2_Hawkes_Data.csv in repo zip.")
if (is.na(repo_trans_file)) stop("Could not find RC2_Hawkes_Trans_Profiles.csv in repo zip.")

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

repo_samples <- intersect(colnames(repo_data), colnames(repo_trans))

repo_sample_lookup <- tibble(
  repo_sample_id = repo_samples,
  repo_site_4digit = str_extract(repo_sample_id, "00[0-9][0-9]"),
  repo_replicate = str_extract(repo_sample_id, "(?<=ICR\\.)[0-9]+")
) %>%
  filter(!is.na(repo_site_4digit), !is.na(repo_replicate)) %>%
  distinct()

cat("\nNumber of repo ICR samples available for filtering:", nrow(repo_sample_lookup), "\n")

write_csv(
  repo_sample_lookup,
  "Results/Water_BasinArea/repo_sample_lookup_used_for_filtering.csv"
)

# ============================================================
# Load basin area data
# ============================================================

geospatial <- read.csv(
  "https://raw.githubusercontent.com/river-corridors-sfa/Geospatial_variables/refs/heads/main/Archived_versions/v4_RCSFA_Extracted_Geospatial_Data_2025-01-31.csv",
  stringsAsFactors = FALSE
) %>%
  transmute(
    Site_ID = site,
    stream_order = as.integer(streamorde),
    basin_area_km2 = parse_number(as.character(totdasqkm))
  )

# ============================================================
# Load metadata
# ============================================================

metadata <- read.csv(
  "v3_RC2_TemporalStudy_2021_2022_SampleData/v3_RC2_Sample_Field_Metadata.csv",
  stringsAsFactors = FALSE
) %>%
  transmute(
    Parent_ID,
    Site_ID
  ) %>%
  left_join(geospatial, by = "Site_ID")

# ============================================================
# Load water-only transformation data
# Filter to samples also present in repo
# ============================================================

water_reps_all <- read_csv(
  "output/Total_Transformations_normalized_7_2_26.csv",
  show_col_types = FALSE
) %>%
  clean_names() %>%
  rename(
    Sample_ID = any_of(c("sample_id", "sampleid", "sample")),
    number_of_peaks = any_of(c("num_peak", "number_of_peaks")),
    total_transformations = any_of(c("n_transformations", "total_transformations")),
    normalized_transformations = any_of(c(
      "normalized_transformations",
      "norm_transformations",
      "normalized_total_transformations"
    ))
  ) %>%
  filter(str_detect(Sample_ID, "ICR")) %>%
  mutate(
    replicate = str_extract(Sample_ID, "(?<=-)[0-9]+$"),
    Parent_ID = Sample_ID %>%
      str_remove("-(\\d+)$") %>%
      str_remove("_ICR$"),
    your_site_4digit = str_extract(Parent_ID, "00[0-9][0-9]")
  )

water_reps <- water_reps_all %>%
  semi_join(
    repo_sample_lookup,
    by = c(
      "your_site_4digit" = "repo_site_4digit",
      "replicate" = "repo_replicate"
    )
  ) %>%
  left_join(metadata, by = "Parent_ID") %>%
  mutate(
    basin_area_log10 = if_else(basin_area_km2 > 0, log10(basin_area_km2), NA_real_)
  ) %>%
  drop_na(
    Parent_ID,
    Site_ID,
    basin_area_km2,
    basin_area_log10,
    number_of_peaks,
    normalized_transformations
  )

missing_from_repo <- water_reps_all %>%
  anti_join(
    repo_sample_lookup,
    by = c(
      "your_site_4digit" = "repo_site_4digit",
      "replicate" = "repo_replicate"
    )
  ) %>%
  select(
    Sample_ID,
    Parent_ID,
    your_site_4digit,
    replicate
  )

write_csv(
  missing_from_repo,
  "Results/Water_BasinArea/water_samples_not_in_repo.csv"
)

cat("\nNumber of water replicate rows before repo filter:", nrow(water_reps_all), "\n")
cat("Number of water replicate rows after repo filter:", nrow(water_reps), "\n")
cat("Samples excluded because they were not in repo:", nrow(missing_from_repo), "\n")

if (nrow(water_reps) == 0) {
  stop("No samples remained after filtering to repo samples. Check whether Parent_ID contains the same 4-digit site code used by the repo.")
}

# ============================================================
# Average replicates by site/sample
# ============================================================

water_site_summary <- water_reps %>%
  group_by(
    Parent_ID,
    Site_ID,
    stream_order,
    basin_area_km2,
    basin_area_log10
  ) %>%
  summarise(
    n_reps = n(),
    mean_richness = mean(number_of_peaks, na.rm = TRUE),
    se_richness = sd(number_of_peaks, na.rm = TRUE) / sqrt(n_reps),
    mean_normalized_transformations = mean(normalized_transformations, na.rm = TRUE),
    se_normalized_transformations = sd(normalized_transformations, na.rm = TRUE) / sqrt(n_reps),
    .groups = "drop"
  ) %>%
  mutate(
    se_richness = replace_na(se_richness, 0),
    se_normalized_transformations = replace_na(se_normalized_transformations, 0)
  )

write_csv(
  water_reps,
  "Results/Water_BasinArea/water_replicates_filtered_to_repo_samples.csv"
)

write_csv(
  water_site_summary,
  "Results/Water_BasinArea/water_site_summary_basin_area_repo_samples_only.csv"
)

cat("\nNumber of water site summaries:", nrow(water_site_summary), "\n")

# ============================================================
# Plot 1: richness vs log10 basin area
# ============================================================

p_richness_logx <- ggplot(
  water_site_summary,
  aes(x = basin_area_log10, y = mean_richness)
) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    color = "blue",
    fill = "grey75",
    linewidth = 0.9,
    alpha = 0.45
  ) +
  geom_errorbar(
    aes(
      ymin = mean_richness - se_richness,
      ymax = mean_richness + se_richness
    ),
    width = 0,
    color = "grey35",
    alpha = 0.7
  ) +
  geom_point(
    shape = 21,
    size = 2.5,
    fill = "grey35",
    color = "black",
    stroke = 0.25
  ) +
  stat_poly_eq(
    aes(
      label = paste(
        after_stat(rr.label),
        after_stat(p.value.label),
        sep = "*\", \"*"
      )
    ),
    formula = y ~ x,
    parse = TRUE,
    small.p = TRUE,
    label.x = 0.08,
    label.y = 0.12,
    size = 3.5
  ) +
  labs(
    title = "A",
    x = expression(log[10]~basin~area~(km^2)),
    y = "Water richness"
  ) +
  theme_pub

# ============================================================
# Plot 2: normalized transformations vs log10 basin area
# ============================================================

p_trans_logx <- ggplot(
  water_site_summary,
  aes(x = basin_area_log10, y = mean_normalized_transformations)
) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    color = "blue",
    fill = "grey75",
    linewidth = 0.9,
    alpha = 0.45
  ) +
  geom_errorbar(
    aes(
      ymin = mean_normalized_transformations - se_normalized_transformations,
      ymax = mean_normalized_transformations + se_normalized_transformations
    ),
    width = 0,
    color = "grey35",
    alpha = 0.7
  ) +
  geom_point(
    shape = 21,
    size = 2.5,
    fill = "grey35",
    color = "black",
    stroke = 0.25
  ) +
  stat_poly_eq(
    aes(
      label = paste(
        after_stat(rr.label),
        after_stat(p.value.label),
        sep = "*\", \"*"
      )
    ),
    formula = y ~ x,
    parse = TRUE,
    small.p = TRUE,
    label.x = 0.08,
    label.y = 0.12,
    size = 3.5
  ) +
  labs(
    title = "B",
    x = expression(log[10]~basin~area~(km^2)),
    y = "Water normalized transformations"
  ) +
  theme_pub

# ============================================================
# Plot 3: richness vs raw basin area
# ============================================================

p_richness_rawx <- ggplot(
  water_site_summary,
  aes(x = basin_area_km2, y = mean_richness)
) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    color = "blue",
    fill = "grey75",
    linewidth = 0.9,
    alpha = 0.45
  ) +
  geom_errorbar(
    aes(
      ymin = mean_richness - se_richness,
      ymax = mean_richness + se_richness
    ),
    width = 0,
    color = "grey35",
    alpha = 0.7
  ) +
  geom_point(
    shape = 21,
    size = 2.5,
    fill = "grey35",
    color = "black",
    stroke = 0.25
  ) +
  stat_poly_eq(
    aes(
      label = paste(
        after_stat(rr.label),
        after_stat(p.value.label),
        sep = "*\", \"*"
      )
    ),
    formula = y ~ x,
    parse = TRUE,
    small.p = TRUE,
    label.x = 0.08,
    label.y = 0.12,
    size = 3.5
  ) +
  labs(
    title = "A",
    x = expression(basin~area~(km^2)),
    y = "Water richness"
  ) +
  theme_pub

# ============================================================
# Plot 4: normalized transformations vs raw basin area
# ============================================================

p_trans_rawx <- ggplot(
  water_site_summary,
  aes(x = basin_area_km2, y = mean_normalized_transformations)
) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    color = "blue",
    fill = "grey75",
    linewidth = 0.9,
    alpha = 0.45
  ) +
  geom_errorbar(
    aes(
      ymin = mean_normalized_transformations - se_normalized_transformations,
      ymax = mean_normalized_transformations + se_normalized_transformations
    ),
    width = 0,
    color = "grey35",
    alpha = 0.7
  ) +
  geom_point(
    shape = 21,
    size = 2.5,
    fill = "grey35",
    color = "black",
    stroke = 0.25
  ) +
  stat_poly_eq(
    aes(
      label = paste(
        after_stat(rr.label),
        after_stat(p.value.label),
        sep = "*\", \"*"
      )
    ),
    formula = y ~ x,
    parse = TRUE,
    small.p = TRUE,
    label.x = 0.08,
    label.y = 0.12,
    size = 3.5
  ) +
  labs(
    title = "B",
    x = expression(basin~area~(km^2)),
    y = "Water normalized transformations"
  ) +
  theme_pub

# ============================================================
# Combine and save figures
# ============================================================

fig_logx <- p_richness_logx / p_trans_logx
fig_rawx <- p_richness_rawx / p_trans_rawx

print(fig_logx)
print(fig_rawx)

ggsave(
  "Figures/Water_BasinArea/water_richness_transformations_log10_basin_area_repo_samples_only.png",
  fig_logx,
  width = 4.2,
  height = 6.5,
  dpi = 300
)

ggsave(
  "Figures/Water_BasinArea/water_richness_transformations_log10_basin_area_repo_samples_only.pdf",
  fig_logx,
  width = 4.2,
  height = 6.5
)

ggsave(
  "Figures/Water_BasinArea/water_richness_transformations_raw_basin_area_repo_samples_only.png",
  fig_rawx,
  width = 4.2,
  height = 6.5,
  dpi = 300
)

ggsave(
  "Figures/Water_BasinArea/water_richness_transformations_raw_basin_area_repo_samples_only.pdf",
  fig_rawx,
  width = 4.2,
  height = 6.5
)

cat("\nDone. Repo-filtered figures written to Figures/Water_BasinArea.\n")
cat("Filtered replicate data written to Results/Water_BasinArea/water_replicates_filtered_to_repo_samples.csv\n")
cat("Site summary written to Results/Water_BasinArea/water_site_summary_basin_area_repo_samples_only.csv\n")
cat("Excluded samples written to Results/Water_BasinArea/water_samples_not_in_repo.csv\n")