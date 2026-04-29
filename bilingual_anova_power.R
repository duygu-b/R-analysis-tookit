install.packages("Superpower")
library(Superpower)

design_result <- ANOVA_design(
  design = "3b*4w",              # 3 groups (between) x 4 Stroop types (within)
  n = 45,                        # number of participants per group
  mu = c(2800, 2500, 3000, 2700,   # Turkish-English bilingual: Congruent, Incongruent
         2850, 2550, 2950, 2750,   # Spanish-English bilingual
         3000, 2900, 3100, 2950),  # Monolingual
  sd = 300,                      # reasonable standard deviation for RT
  r = 0.5,                       # correlation between congruent and incongruent conditions
  labelnames = c("Group", "TurkEng", "SpanEng", "MonoEng",
                 "Condition", "Cong-amb", "Incong-amb", "Cong-unamb", "Incong-unamb")
)

power_result <- ANOVA_power( design_result, alpha_level = 0.05, nsims = 1000 )

print(power_result)
