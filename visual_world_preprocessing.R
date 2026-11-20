# install.packages("tidyverse")
library(readr)
library(dplyr)

# 1. Read the file

file <- "/documents/xxxxx.csv"
out_dir <- dirname(file)                                 # folder the file sits in
base_name <- tools::file_path_sans_ext(basename(file))    # file name without the .csv

df <- read_csv(file, show_col_types = FALSE)

# 2. Drop the first 8 columns
df <- df[, -(1:8)]

# 3. Add participant, group and L1 at the front
n <- nrow(df)  # number of rows to repeat the labels across

df <- cbind(
  participant = rep("E1_P001", n),
  group = rep("monolingual", n),
  L1 = rep("EN", n),
  df
)


# For each free-sort trial, record whether the Stroop trial before it was congruent or incongruent. The Stroop condition sits two rows above the
# free-sort row, so lag(values, 2) reaches it. Rows that are not free-sort are left empty.
df$previous_stroop <- ifelse(
  df$task == "free-sort",
  ifelse(
    grepl('"condition":"incongruent"', dplyr::lag(df$values, 2)), "incongruent",
    ifelse(
      grepl('"condition":"congruent"', dplyr::lag(df$values, 2)), "congruent", ""
    )
  ),
  ""
)

# Validation error is measured during the fixation cross, which is logged one row above its trial, so it is shifted down onto the trial it belongs to.
df$trial_validation_error <- dplyr::lag(df$trial_validation_error)

# 4. Save as a new file, with _pre_processed added to the name
write_csv(df, file.path(out_dir, paste0(base_name, "_pre_processed.csv")))
