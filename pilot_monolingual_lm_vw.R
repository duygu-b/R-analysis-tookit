install.packages("readxl")
library(readxl)
df <- read_excel("documents/df_post.xlsx", sheet="Sheet1")

df$target_look_latency <- as.numeric(df$target_look_latency) 
df$sentence_type <- factor(df$condition) # Converting categorical predictors into factors for modeling

# Fitting linear model (no random effects; pilot analysis only)
# Testing main effects and interaction of sentence type and Stroop condition
df$previous_stroop <- factor(df$previous_stroop)

model <- lm(target_look_latency ~ condition * previous_stroop, data = df)

summary(model)
