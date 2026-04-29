install.packages("readxl")
library(readxl)
df <- read.csv("documents/all.csv") # Loading dataset (pilot monolingual data)

df$logRT <- log(df$CriticalRegionRT) # Log-transform reaction times to reduce skewness in RT distribution
df$sentence_type <- factor(df$sentence_type) # Converting categorical predictors into factors for modeling

# Fitting linear model (no random effects; pilot analysis only)
# Testing main effects and interaction of sentence type and Stroop condition
df$previous_stroop <- factor(df$previous_stroop)

model <- lm(logRT ~ sentence_type * previous_stroop, data = df)

summary(model)
