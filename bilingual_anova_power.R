install.packages("Superpower")
library(Superpower)

design_result <- ANOVA_design(
  design = "3b*2w*2w",              # 3 grup (between) x 2 Stroop type (within) x 2 Sentence type (within)
  n = 45,                     
  mu = c( #Cong vs Incong
          # Turkish-English
          3409, 3765, #amb
          3132, 2861, #unamb
          # Spanish-English
          4336, 4474, 
          3586, 3373,
          # Monolingual
          4234, 4124, 
          3164, 3157),
  sd = 300,                      # reasonable standard deviation for RT
  r = 0.5,                       # correlation between congruent and incongruent conditions
  labelnames = c(
    "Group", "TurkEng", "SpanEng", "MonoEng",
    "Sentence", "Amb", "Unamb", 
    "Stroop", "Cong", "Incong")
)

power_result <- ANOVA_power(
  design_result,
  alpha_level = 0.05,
  nsims = 1000
)

print(power_result)
