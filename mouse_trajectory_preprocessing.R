install.packages(c("dplyr", "tidyr", "jsonlite", "readxl", "writexl"))
# These packages are used for data manipulation, JSON parsing, and file handling

library(dplyr) # data manipulation
library(jsonlite) # parsing JSON formatted ROI transition data
library(readxl) # reading Excel files
library(tidyr) # reshaping nested data into long format
library(writexl) # optional export to Excel format

# Step1: Read Excel dataset
# This file contains trial-level mouse tracking data (including nested JSON trajectories)
df <- read_excel("documents/All.xlsx", sheet="ambiguous")

# Step2: Parse JSON mouse trajectory column
# Each row contains a JSON string with x, y, t coordinates
# We convert each JSON string into a data.frame (list of coordinates per trial)
df$events <- lapply(df$mouse_trajectory, function(x) {
  if (is.na(x)) return(data.frame()) # handle missing data safely
  fromJSON(x)                         # convert JSON string into structured data
})
# Step3: Unnest trajectory data
# Expands each trial's nested coordinates into long format (one row per timestamp)
df_long <- df %>% unnest(events)
# This creates a flat file that can be used for mouse-tracking analyses (AUC, MD, etc.)
write.csv(df_long, "df_mouse_amb.csv", row.names = FALSE)
