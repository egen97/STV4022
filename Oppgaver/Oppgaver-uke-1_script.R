#devtools::install_github("avehtari/ROS-Examples",subdir = "rpackage")
library(rosdata)
library(tidyverse)
earnings <- rosdata::earnings

head(earnings)

str(earnings)

nrow(earnings)

ggplot(earnings, aes(x = earn)) + 
  geom_density(alpha=0.5)

earnings$log_earn <- log(earnings$earn) # Lag en logaritmisk versjon av variabelen
earnings$log_earn[!is.finite(earnings$log_earn)] <- 0 # Kod om uendelige verdier til 0
ggplot(earnings, aes(x = log_earn)) + geom_density(alpha = 0.5) # Lag et nytt plott


earnings$male <- factor(earnings$male, levels = c(0, 1), labels = c("Women", "Men"))

ggplot(earnings, aes(x = log_earn, color = male, fill = male)) + 
  geom_density(alpha=0.5) + 
  geom_vline(xintercept = median(earnings$log_earn[earnings$male=="Men"]), color = "#00BFC4") +
  geom_vline(xintercept = median(earnings$log_earn[earnings$male=="Women"]), color = "#F8766D") +
  theme(legend.title = element_blank())
