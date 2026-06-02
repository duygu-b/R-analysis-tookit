install.packages(c("dplyr", "tidyr", "jsonlite", "readxl", "writexl"))
# These packages are used for data manipulation, JSON parsing, and reading/writing files

library(dplyr) # data manipulation
library(jsonlite) # parsing JSON formatted ROI transition data
library(readxl) # reading Excel files
library(tidyr) # reshaping nested data into long format
library(writexl) # optional export to Excel format

# Step1: Load dataset from Excel
# This dataset contains unambiguous sentence trials with ROI transition data stored as JSON strings
df <- read_excel("documents/All.xlsx", sheet="unambiguous")

# Step2: Parse ROI transition JSON data
# Each row contains a JSON string describing eye/mouse transitions over time
# We convert each JSON string into a structured data.frame per trial
df$events <- lapply(df$roi_transitions, function(x) {
  if (is.na(x)) return(NULL) # handle missing entries safely
  jsonlite::fromJSON(x)       # convert JSON string into tabular format
})

# Step3: Expand nested list into long format
# Each timestamped event becomes a separate row
df_long <- df %>% unnest(events)

# Step4: Keep only post-preparation phase data
# This filters the data to include only timepoints after sentence onset preparation phase
df_post <- df_long %>% filter(t >= prep_onset)

# Step5: Export cleaned dataset
# Final dataset used for analysis (post-preparation ROI transitions only)
write.csv(df_post, "df_post.csv", row.names = FALSE)
