
rm(list = ls())     # Clear all objects in the environment
load("../data/KeyWestAnnualMeanTemperature.RData")  # Load the temperature data file
ats$Year <- as.numeric(ats$Year)  # Convert the year to a numeric type

# Plot the scatter plot of temperatures between successive years
plot(ats$Temp[-length(ats$Temp)], ats$Temp[-1])

### Calculate the correlation between temperatures of successive years
cor_1 <- cor(ats$Temp[-length(ats$Temp)], ats$Temp[-1], method = "kendall")  # Calculate the Kendall correlation coefficient
cat('cor is:', cor_1)  # Print the correlation coefficient

### Permutation analysis
set.seed(999)                          # Set a random seed for reproducibility
permutations_times <- 10000            # Set the number of permutations
cor_2 <- rep(NA, permutations_times)   # Initialize a vector to store permutation correlation coefficients

# Repeat the correlation calculation for each permutation
for(i in 1:permutations_times) {
  perm_data <- sample(ats$Temp)                   # Randomly permute the temperature data
  cor_2[i] <- cor(perm_data[-length(perm_data)], perm_data[-1], method = "kendall")
}

### Calculate the p-value
p_value <- sum(abs(cor_2) >= cor_1) / permutations_times  # Calculate the proportion of permutation correlation coefficients greater than the observed one (p-value)
cat("p_value is", p_value, "\n")  # Print the p-value

### Generate a histogram of the correlation coefficients
pdf(file = "../results/correlation_histogram.pdf")  # Specify the output path for the PDF file

# Draw the histogram and mark the observed correlation coefficient
hist(cor_2, breaks = 50, main = "Permutation Correlation Coefficients",
     xlab = "Correlation Coefficient", ylab = "Frequency", xlim = c(-0.3, 0.3))
abline(v = cor_1, col = "red", lwd = 2)  # Draw a line at the position of the actual correlation coefficient
legend("topright", legend = paste("Observed Corr:", round(cor_1, 3)), col = "red", lwd = 1, cex = 0.5)

dev.off()  # Close the PDF device
