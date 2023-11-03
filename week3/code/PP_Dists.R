rm(list = ls())
data <- read.csv("../data/EcolArchives-E089-51-D1.csv")

# calculate log
data$log_predator_mass <- log(data$Predator.mass)
data$log_prey_mass <- log(data$Prey.mass)
data$log_size_ratio <- log(data$Prey.mass / data$Predator.mass)
summary(data$Predator)

#calculate mean and median
group <- split(data, data$Type.of.feeding.interaction)
getwd()
list_1 <- list()

for (i in names(group)) {
  subdata <- group[[i]]
  list_1[[i]] <- data.frame(
    FeedingInteraction = i,
    MeanPredatorMass = mean(log(subdata$Predator.mass), na.rm = TRUE),
    MedianPredatorMass = median(log(subdata$Predator.mass), na.rm = TRUE),
    MeanPreyMass = mean(log(subdata$Prey.mass), na.rm = TRUE),
    MedianPreyMass = median(log(subdata$Prey.mass), na.rm = TRUE),
    MeanSizeRatio = mean(log(subdata$Prey.mass / subdata$Predator.mass), na.rm = TRUE),
    MedianSizeRatio = median(log(subdata$Prey.mass / subdata$Predator.mass), na.rm = TRUE)
  )
}
final_data <- do.call(rbind, list_1)

#save as csv
write.csv(final_data, "../results/PP_Results.csv", row.names = F)

# create a function to plot
plot_1 <- function(data, variable, interaction_type, filename){
  p <- ggplot(data, aes_string(x = variable)) + 
    geom_histogram() +
    facet_wrap(as.formula(paste('~',interaction_type)), scales = "free") +
    xlab(paste('Log of', variable)) +
    ylab("Frequency") +
    ggtitle(paste("Distribution of Log", variable, 'by Feeding Interaction Type')) +
    theme_minimal()
  
  ggsave(filename, plot = p)
}

#save our plots
plot_1(data, 'log_predator_mass', 'Type.of.feeding.interaction', '../results/Pred_Subplots.pdf')
plot_1(data, "log_prey_mass", 'Type.of.feeding.interaction', "../results/Prey_Subplots.pdf")
plot_1(data, "log_size_ratio", 'Type.of.feeding.interaction', "../results/SizeRatio_Subplots.pdf")
