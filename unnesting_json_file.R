install.packages(c("dplyr", "tidyr", "jsonlite", "readxl", "writexl"))
# These packages are used for data manipulation, JSON parsing, and reading/writing files

library(dplyr) # data manipulation
library(jsonlite) # parsing JSON formatted ROI transition data
library(readxl) # reading Excel files
library(tidyr) # reshaping nested data into long format
library(writexl) # optional export to Excel format

# Step1: Load dataset from Excel
# This dataset contains JSON-formatted data stored as text strings
df <- read_excel("documents/data.xlsx")

# Step2: Parse JSON data
# Each row contains a JSON string describing events or observations over time
# We convert each JSON string into a structured data.frame per trial/observation
df$events <- lapply(df$json_column, function(x) {
  if (is.na(x)) return(NULL) # handle missing entries safely
  jsonlite::fromJSON(x)      # convert JSON string into tabular format
})

# Step3: Expand nested list into long format
# Each element stored within the JSON structure becomes a separate row
df_long <- df %>% unnest(events)

# Step4: Export cleaned dataset
# Final dataset used for further analysis
write.csv(df_long, "cleaned_data.csv", row.names = FALSE)
