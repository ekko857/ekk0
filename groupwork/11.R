###load the data
rm(list = ls())
load("../data/KeyWestAnnualMeanTemperature.RData")
ls(ats)
str(ats)
ats$Year <- as.numeric(ats$Year)

plot(ats$Temp[-length(ats$Temp)], ats$Temp[-1])

###calculate the cor between n and n-1 years
cor_1 <- cor(ats$Temp[-length(ats$Temp)], ats$Temp[-1], method = "kendall")
cat('cor is:',cor_1 )

###Permutation analysis
set.seed(999)
permutations_times <- 10000
cor_2 <- rep(NA, permutations_times)
for(i in 1:permutations_times) {
  perm_data <- sample(ats$Temp)
  cor_2[i] <- cor(perm_data[-length(perm_data)], perm_data[-1], method = "kendall")
}

###calculate p_value
p_value <- sum(abs(cor_2) >= cor_1) / permutations_times
cat("p_value is",p_value,"\n")

hist(cor_2, breaks = 50, main = "Permutation Correlation Coefficients",
     xlab = "Correlation Coefficient", ylab = "Frequency", xlim = c(-0.3, 0.3))

abline(v = cor_1, col = "red", lwd = 2)
legend("topright", legend = paste("Observed Corr:", round(cor_1, 3)), col = "red", lwd = 1, cex = 0.5
       
       