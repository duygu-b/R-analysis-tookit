# Mouse analysis runs on correct trials only, so pull those out first and save them as a separate file

# All participants' data, merged
df <- read.csv(".../all_groups.csv")

# Mouse measures only make sense when the participant moved the right image
new_df <- df %>% filter(accuracy == 1)

# Input file for the mouse measures script
write.csv(new_df, file = "documents/all_groups_mouse.csv", row.names = FALSE)
