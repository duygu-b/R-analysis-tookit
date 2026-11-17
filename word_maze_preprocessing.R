library(readr)
library(dplyr)

# Reading one participant's raw data file

file <- "file.csv"
out_dir <- dirname(file) # folder the file sits in (used when saving)
base_name <- tools::file_path_sans_ext(basename(file)) # file name without the .csv extension (used when saving)

df <- read_csv(file, show_col_types = FALSE)

# Dropping the first 8 columns
df <- df[, -(1:8)]

# Adding participant, group and L1 columns at the front
n <- nrow(df)  # number of rows to repeat the labels across

df <- cbind(
  participant = rep("E1_P067", n),
  group = rep("monolingual", n),
  L1 = rep("EN", n),
  df
)

# For each free-sort trial, look two rows back and record whether the preceding Stroop trial was congruent or incongruent
df$previous_stroop <- ifelse(
  df$task == "free-sort",
  ifelse(grepl('"condition":"incongruent"', dplyr::lag(df$values, 2)), "incongruent",
    ifelse(grepl('"condition":"congruent"', dplyr::lag(df$values, 2)), "congruent", "" )
  ),
  ""
)

#Shift validation errors down one row so each lands on its own trial because validation error is measured during the fixation cross, which is logged one row above its trial — shift it down so it sits with the right trial
df$trial_validation_error <- dplyr::lag(df$trial_validation_error) 


write_csv(df, file.path(out_dir, paste0(base_name, "_pre_processed.csv")))

