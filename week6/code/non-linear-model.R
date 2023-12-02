getwd()
setwd("/Users/3kko/Documents/CMEECourseWork/week6/code")
rm(list = ls())
graphics.off()

S_data <- seq(1,50,5)
S_data

V_data <- ((12.5 * S_data)/(7.1 + S_data))
plot(S_data, V_data)
set.seed(1456)
V_data <- V_data + rnorm(10, 0 , 1)
plot(S_data, V_data)

MM_model <- nls(V_data ~ V_max * S_data / (K_M + S_data))

plot(S_data,V_data,xlab = "substrate concentration", ylab = 
       "reaction rate")
lines(S_data,predict(MM_model),lty = 1,col = "red",lwd = 2)

coef(MM_model)
 
Predict2Plot <- coef(MM_model)["V_max"] * Substrate2Plot / (coef(MM_model)["K_M"] + Substrate2Plot) 
Substrate2Plot <- seq(min(S_data), max(S_data),len=200) 
plot(S_data,V_data, xlab = "Substrate Concentration", ylab = "Reaction Rate")
lines(Substrate2Plot, Predict2Plot, lty=1,col="blue",lwd=2)

summary(MM_model)

install.packages("minpack.lm") 
library(minpack.lm)
MM_model5 <- nlsLM(V_data ~ V_max * S_data / (K_M + S_data), start = list(V_max = 12, K_M = 7))
coef(MM_model5)
coef(MM_model)







MyData <- read.csv("../data/GenomeSize.csv") # using relative path assuming that your working directory is "code"

head(MyData)

Data2Fit <- subset(MyData, Suborder == "Anisoptera")
Data2Fit <- Data2Fit[!is.na(Data2Fit$TotalLength),]
plot(Data2Fit$TotalLength, Data2Fit$BodyWeight)

library("ggplot2")

ggplot(Data2Fit, aes(x = TotalLength, y = BodyWeight)) + 
  geom_point(size = (3),color="red") + theme_bw() + 
  labs(y="Body mass (mg)", x = "Wing length (mm)")

nrow(Data2Fit)
PowFit <- nlsLM(BodyWeight ~ a * TotalLength^b, data = Data2Fit, start = list(a = .1, b = .1))

Data2Fit$a <- log(Data2Fit$TotalLength)
Data2Fit$b <- log(Data2Fit$BodyWeight)
plot(Data2Fit$a,Data2Fit$b)
model <- lm(Data2Fit$b ~ Data2Fit$a)
summary(model)
lines(Data2Fit$a,predict(model))

a1 <- 10^-11.8178 
a1
Lengths <- seq(min(Data2Fit$TotalLength),max(Data2Fit$TotalLength),len=200)

QuaFit <- lm(BodyWeight ~ poly(TotalLength,2), data = Data2Fit)
Predic2PlotQua <- predict.lm(QuaFit, data.frame(TotalLength = Lengths))
