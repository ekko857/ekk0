library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(broom)
MyDF <- read.csv("../Data/EcolArchives-E089-51-D1.csv")

MyDF$Prey.mass[Prey.mass.unit == "mg"] == MyDF$Prey.mass[Prey.mass.unit == "mg"]

dat <- MyDF %>%  group_split(Location,Predator.lifestage,Type.of.feeding.interaction) %>% 
  lapply(., function(x) list(Location = x$Location[[1]],
                             feeding = x$Type.of.feeding.interaction[[1]], 
                             lifestage = x$Predator.lifestage[[1]],
                             mod = lm(Predator.mass ~ Prey.mass, data = x)))
dat

datafm <- setNames(data.frame(matrix(ncol = 8, nrow = 18)), c("Location","Predator_life","Feeding_type","regression_slope", "regression_intercept", "R", "F_statistic_value","p_value"))


for (i in 1:length(dat)) {
  val <- summary(dat[[i]]$mod)
  datafm[i,]$Location <- dat[[i]]$Location
  datafm[i,]$Predator_life <- dat[[i]]$lifestage
  datafm[i,]$Feeding_type <- dat[[i]]$feeding
  datafm[i,]$regression_intercept <- dat[[i]]$mod$coef[[1]]
  datafm[i,]$regression_slope <- dat[[i]]$mod$coef[[2]]
  datafm[i,]$R <- val$adj.r.squared
  datafm[i,]$F_statistic_value <- val$fstatistic[1]
  datafm[i,]$p_value <- ifelse(nrow(val$coefficients) > 1, val$coefficients[2,4], NA )
}
datafm
write_csv(datafm, "../Results/PP_Regress_Results_loc.csv")