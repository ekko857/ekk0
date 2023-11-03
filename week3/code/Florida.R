rm(list = ls())
getwd()
setwd("/Users/3kko/Documents/CMEECourseWork/week3/code")
load("../data/KeyWestAnnualMeanTemperature.RData")
ls()
class(ats)
head(ats)
plot(ats)
str(ats)
ats$Year <- as.numeric(ats$Year)

# Calculate the observed cor
cor_1 <- cor(ats$Year,ats$Temp, method = "kendall")
cat('cor is ',cor_1)

#Permutation analysis
set.seed(999)
permutations_times <- 10000
cors <- rep(NA, permutations_times)

for (i in 1:permutations_times) {
  data <- ats
  data$Temp <- sample(data$Temp)
  cors[i] <- cor(data$Year,data$Temp, method = "kendall")
}

#Calculate the p-value
p_value <- sum(abs(cors) >= cor_1) / permutations_times
cat("pvalue is",p_value,"\n")

#plot a hist and save it
pdf(file = "../results/correlation_histogram.pdf")

hist(cors, breaks = 50, main = "Permutation Correlation Coefficients",
     xlab = "Correlation Coefficient", ylab = "Frequency", xlim = c(-0.5, 0.5))

abline(v = cor_1, col = "red", lwd = 2)
legend("topright", legend = paste("Observed Corr:", round(cor_1, 3)), col = "red", lwd = 1, cex = 0.5)

dev.off()
