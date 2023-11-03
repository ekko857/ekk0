rm(list = ls())
data <- read.csv("../data/EcolArchives-E089-51-D1.csv")
#plot
p <- ggplot(data, aes(x = log(Prey.mass), y = log(Predator.mass), color = Predator.lifestage)) +
  geom_point(shape = 3, size = 1) +
  facet_wrap(. ~ Type.of.feeding.interaction, scales = 'free', ncol = 1, strip.position = "right")  +
  geom_smooth(method = "lm", linewidth = 0.5, fullrange = T) +
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(x = "Prey Mass in grams",
       y = "Predator Mass in grams")+
  guides(color = guide_legend(nrow = 1)) 
#save plot
ggsave("../results/PP_Regress_plot.pdf", p, device = "pdf",width = 8, height = 11, units = "in")

# create a empty dataframe to save our data
regress_results <- data.frame(
  FeedingType = character(),
  LifeStage = character(),
  Slope = numeric(),
  Intercept = numeric(),
  Rsquare = numeric(),
  Fstatistic = numeric(),
  Pvalue = numeric(),
  stringsAsFactors = FALSE
)
# use for loop to calculate each lm
for (feeding_type in unique(data$Type.of.feeding.interaction)) {
  for (life_stage in unique(data$Predator.lifestage)) {
    subset_data <- filter(data, Type.of.feeding.interaction == feeding_type & Predator.lifestage == life_stage)
#make sure we have enough data   
    if (nrow(subset_data) > 1) {  
      model <- lm(log(Predator.mass) ~ log(Prey.mass), data = subset_data)
      summary_model <- summary(model)
#make sure our cof exist      
      if ("log(Prey.mass)" %in% rownames(summary_model$coefficients)) {
        regress_results <- rbind(regress_results, data.frame(
          FeedingType = feeding_type,
          LifeStage = life_stage,
          Slope = summary_model$coefficients["log(Prey.mass)", "Estimate"],
          Intercept = summary_model$coefficients["(Intercept)", "Estimate"],
          Rsquare = summary_model$r.squared,
          Fstatistic = summary_model$fstatistic[1],
          Pvalue = summary_model$coefficients["log(Prey.mass)", "Pr(>|t|)"]
        ))
      }
    }
  }
}
regress_results <- unique(regress_results)
write.csv(regress_results, "../results/PP_Regress_Results.csv", row.names = FALSE)

