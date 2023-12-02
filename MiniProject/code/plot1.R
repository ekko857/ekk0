# Clear all existing objects from the workspace
rm(list = ls())
# Load ggplot2 for data visualization
library(ggplot2)
library(dplyr)
# Load the CSV files containing data and model parameters
data <- read.csv("../data/modified_data_1.csv")
params_logistic_df <- read.csv("../results/params_logistic.csv")
params_gompertz_df <- read.csv("../results/params_gompertz.csv")
model_metrics <- read.csv("../results/model_metrics.csv")

# Define the logistic growth model function
logistic_model <- function(t, r_max, K, N_0){
  return(N_0 * K * exp(r_max * t)/(K + N_0 * (exp(r_max * t) - 1)))
}

# Define the modified Gompertz growth model function (as per Zwietering 1990)
gompertz_model <- function(t, r_max, K, N_0, t_lag){
  return(N_0 + (K - N_0) * exp(-exp(r_max * exp(1) * (t_lag - t)/((K - N_0) * log(10)) + 1)))
}

# Extract unique IDs from the dataset
unique_ids <- unique(data$ID)

# Initialize an empty list to store plots
plots_list <- list()

# Loop through each unique ID to create plots
for (id in unique_ids) {
  subset <- subset(data, ID == id)
  
  # Create a basic ggplot object with data points
  p <- ggplot(subset, aes(x = Time, y = logP)) + geom_point() +
    theme_minimal() + 
    labs(title = paste("Growth Model for", id), 
         x = "Time", 
         y = "Log(Population Bio)")
  
  # Define timepoints for model predictions
  timepoints <- seq(0, max(subset$Time), 1)
  
  # Add quadratic linear model as a smooth line
  p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = F, 
                       aes(color = "Quadratic Linear"), linewidth = 1)
  
  # Add cubic linear model as a smooth line
  p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = F, 
                       aes(color = "Cubic Linear"), linewidth = 1)
  
  # Add logistic model predictions if available
  if (id %in% params_logistic_df$ID) {
    logistic_params <- params_logistic_df[params_logistic_df$ID == id, ]
    predicted_logistic <- logistic_model(timepoints, logistic_params$r_max, logistic_params$K, logistic_params$N_0)
    df_1 <- data.frame(Time = timepoints, PopBio = predicted_logistic)
    p <- p + geom_line(data = df_1, aes(x = Time, y = log(PopBio), color = "Logistic"), 
                       linewidth = 1)
  }
  
  # Add Gompertz model predictions if available
  if (id %in% params_gompertz_df$ID) {
    gompertz_params <- params_gompertz_df[params_gompertz_df$ID == id, ]
    predicted_gompertz <- gompertz_model(timepoints, gompertz_params$r_max, gompertz_params$K, 
                                         gompertz_params$N_0, gompertz_params$t_lag)
    df_2 <- data.frame(Time = timepoints, PopBio = predicted_gompertz)
    p <- p + geom_line(data = df_2, aes(x = Time, y = PopBio, color = "Gompertz"), 
                       linewidth = 1)
  }
  
  # Customize plot colors and legend
  p <- p + scale_color_manual(values = c("Quadratic Linear" = "red", 
                                         "Cubic Linear" = "yellow", 
                                         "Logistic" = "blue", 
                                         "Gompertz" = "green"),
                              name = "Model") +
    theme(legend.position = "bottom") 
  
  # Store the plot in the list
  plots_list[[id]] <- p
}

# Remove rows with infinite values in model metrics
model_metrics <- model_metrics %>%
  filter(!is.infinite(AIC), !is.infinite(BIC), !is.infinite(R_Squared), !is.infinite(RSS))

# Calculate and summarize model performance metrics
model_performance <- model_metrics %>%
  group_by(Model) %>%
  summarize(
    Mean_AIC = mean(AIC, na.rm = TRUE),
    Mean_BIC = mean(BIC, na.rm = TRUE),
    Mean_R2 = mean(R_Squared, na.rm = TRUE),
    Mean_RSS = mean(RSS, na.rm = TRUE)
  )

# Visualization of AIC and BIC for different models
P_AIC <- ggplot(model_metrics, aes(x = Model, y = AIC, fill = Model)) +
  geom_boxplot() +
  labs(title = "AIC across Different Models", x = "Model", y = "AIC")

P_BIC <- ggplot(model_metrics, aes(x = Model, y = BIC, fill = Model)) +
  geom_boxplot() +
  labs(title = "BIC across Different Models", x = "Model", y = "BIC")

# Visualization of R2 and RSS for different models
P_R2 <- ggplot(model_metrics, aes(x = Model, y = R_Squared, fill = Model)) +
  geom_boxplot() +
  labs(title = "R2 across Different Models", x = "Model", y = "R2")

P_RSS <- ggplot(model_metrics, aes(x = Model, y = RSS, fill = Model)) +
  geom_boxplot() +
  labs(title = "RSS across Different Models", x = "Model", y = "RSS")

# Print the summarized model performance
#print(model_performance)

# Bar plot to show average AIC for each model
average_aic <- ggplot(model_metrics, aes(x = Model, y = AIC)) + 
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Average AIC by Model", x = "Model", y = "Average AIC")

# Group data by ID and find best model based on minimum AIC
best_models <- model_metrics %>%
  group_by(ID) %>%
  slice(which.min(AIC))

# Count how often each model is selected as the best
model_counts <- table(best_models$Model)
model_counts <- as.data.frame(model_counts)
names(model_counts) <- c("Model", "Count")

# Print the counts of best-selected models
#print(model_counts)

# Plot to show frequency of each model being selected as the best
P_model_count <- ggplot(model_counts, aes(x = Model, y = Count, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.3) +
  labs(title = "Frequency of Best Model by AIC", x = "Model", y = "Frequency")

# Print the plot
#print(P_model_count)

# Plot model frequency for each popunit in a facet grid
popunits <- unique(best_models$popunit)
P_unit <- ggplot(best_models, aes(x = Model, fill = Model)) +
  geom_bar() +
  facet_wrap(~ popunit) +  
  labs(title = "Model Frequency for each Popunit", x = "Model", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Count models and calculate success rates
model_counts_1 <- model_metrics %>%
  count(Model)
total_ids <- length(unique(model_metrics$ID))
model_success_rates <- model_counts_1 %>%
  mutate(Success_Rate = n / total_ids)

# Save plots to disk
ggsave("../results/average_aic.png", average_aic, width = 10, height = 8, dpi = 300)
ggsave("../results/p_aic.png", P_AIC, width = 10, height = 8, dpi = 300)
ggsave("../results/p_bic.png", P_BIC, width = 10, height = 8, dpi = 300)
ggsave("../results/p_r2.png", P_R2, width = 10, height = 8, dpi = 300)
ggsave("../results/p_rss.png", P_RSS, width = 10, height = 8, dpi = 300)
ggsave("../results/p_model_count.png", P_model_count, width = 10, height = 8, dpi = 300)
ggsave("../results/p_unit.png", P_unit, width = 10, height = 8, dpi = 300)
ggsave("../results/my_plot_246.png", plots_list[[246]], width = 10, height = 8, dpi = 300)
ggsave("../results/my_plot_229.png", plots_list[[229]], width = 10, height = 8, dpi = 300)
ggsave("../results/my_plot_162.png", plots_list[[162]], width = 10, height = 8, dpi = 300)
ggsave("../results/my_plot_149.png", plots_list[[149]], width = 10, height = 8, dpi = 300)
ggsave("../results/my_plot_24.png", plots_list[[24]], width = 10, height = 8, dpi = 300)
ggsave("../results/my_plot_2.png", plots_list[[2]], width = 10, height = 8, dpi = 300)

# Write model performance and success rates to CSV files
write.csv(model_performance, "../results/model_performance.csv", row.names = F)
write.csv(model_success_rates, "../results/model_success_rates.csv", row.names = F)

# Create a PDF to save all plots
pdf("../results/all_plots.pdf", width = 10, height = 8)

# Save each graph to the PDF
for (id in unique_ids) {
  print(plots_list[[id]])
}

# Close the PDF device
dev.off()
