install.packages("lme4")
library(lme4)

df <- read.csv("documents/all.csv")

df$logRT <- log(df$CriticalRegionRT)

df$sentence_type <- factor(df$sentence_type)
df$previous_stroop <- factor(df$previous_stroop)
df$group <- factor(df$group)
df$participant <- factor(df$participant)
df$sentence_id <- factor(df$sentence_id)

model <- lmer(
  logRT ~ previous_stroop * sentence_type * group + # fixed effects: main effects and all interactions between Stroop, sentence type, and group
    (1 | participant) +   # random intercept for participant differences in overall RT
    (1 | sentence_id),    # random intercept for item (sentence) variability
  data = df
)

summary(model)
