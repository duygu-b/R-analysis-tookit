library("ggplot2"); theme_set(theme_bw())
library(readxl)
library(ggplot2)

#Preliminaries
data <- read_excel("/documents/df_post.xlsx", sheet="clean_gaze")
data$condition <- factor(data$condition)
data$target_look_latency <- as.numeric(data$target_look_latency)

print(data)
print(ggplot(data,aes(condition,target_look_latency))
      + geom_boxplot(fill="lightgray")
      + stat_sum(alpha=0.7)
      
      + scale_size(breaks=1:2, range=c(3,6))
)

#Permutation: step1

set.seed(101) # for reproducibility
nsim <- 9999 # number of simulations
res <- numeric(nsim) # set aside space for results

#step2 latency difference (subtraction) based on conditions in incongruent stroops
obs <- with(data,
            mean(target_look_latency[condition == "ambiguous" & previous_stroop == "incongruent"], na.rm = TRUE) -
              mean(target_look_latency[condition == "unambiguous" & previous_stroop == "incongruent"], na.rm = TRUE)
)

#step3
for (i in 1:nsim) {
  
  data$shuffled_condition <- unlist(
    tapply(data$condition, data$previous_stroop, sample)
  )
  
  res[i] <- mean(data$target_look_latency[
    data$shuffled_condition == "ambiguous" &
      data$previous_stroop == "incongruent"
  ], na.rm = TRUE) -
    
    mean(data$target_look_latency[
      data$shuffled_condition == "unambiguous" &
        data$previous_stroop == "incongruent"
    ], na.rm = TRUE)
}

#step4
res <- c(res, obs)

p_value <- mean(abs(res) >= abs(obs))
p_value

#step5 - visualisation
hist(res, col="gray", main="")
