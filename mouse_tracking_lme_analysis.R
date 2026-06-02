install.packages("lme4") # This package is used to fit linear mixed-effects models
library(dplyr)  # data manipulation
library(lme4) # for mixed-effects modeling
library(tidyr) # for data reshaping (if needed in later steps)
# Step1: Normalize time within each participant and sentence
# This removes differences in trial duration by scaling time to 0–1 range
df_long <- df_long %>%
  group_by(participant, sentence_id) %>%
  arrange(t) %>%
  mutate(t_norm = (t - min(t)) / (max(t) - min(t)))
# Step2: Compute Maximum Deviation (MD)
# MD captures the largest spatial deviation from the mean trajectory per trial
md <- df_long %>%
    group_by(participant, sentence_id, `previous-stroop`) %>%
    mutate(deviation = abs(x - mean(x))) %>%
    summarise(MD = max(deviation, na.rm = TRUE))
# Step3: Compute Area Under the Curve (AUC)
# AUC captures total accumulated deviation over normalized time
auc <- df_long %>%
  group_by(participant, sentence_id, `previous-stroop`) %>%
  arrange(t) %>%
  mutate(
    deviation = abs(x - mean(x)),
    dt = t - lag(t)) %>%
  summarise(AUC = sum(deviation * dt, na.rm = TRUE))

# Step4: Add trial-level condition information to AUC dataset
# This merges previous Stroop condition for later statistical analysis
auc <- auc %>% left_join(df %>% select(participant, sentence_id), by = c("participant", "sentence_id"))

# Step5: Add trial-level condition information to MD dataset
# Ensures MD results can be linked to experimental conditions
md <- md %>% left_join(df %>% select(participant, sentence_id), by = c("participant", "sentence_id"))

# Step6: Fit a linear mixed-effects model
# This model tests whether previous Stroop condition predicts AUC
# Random intercept for participant accounts for individual differences
model <- lmer(AUC ~ `previous-stroop` + (1|participant), data = auc) # The group variable (bilingual vs monolingual) will be added as a fixed effect,
# and its interaction with previous Stroop condition (previous_stroop * group) will be tested to examine whether conflict adaptation effects differ between language groups.

# Step7: Display model summary
# Shows fixed effects (condition effect), random effects, and model fit statistics
summary(model)
