rm(list = ls())
data <- read.csv("../data/modified_data_1.csv")
library(dplyr)
require("minpack.lm")
#define the model we want to fit
#1.
#Quadratic linear model

#2.
#Cubic linear model

#3.
logistic_model <- function(t, r_max, K, N_0){ # The classic logistic equation
  return(N_0 * K * exp(r_max * t)/(K + N_0 * (exp(r_max * t) - 1)))
}

#4.
gompertz_model <- function(t, r_max, K, N_0, t_lag){ # Modified gompertz growth model (Zwietering 1990)
  return(N_0 + (K - N_0) * exp(-exp(r_max * exp(1) * (t_lag - t)/((K - N_0) * log(10)) + 1)))
}   

#unique(data$ID)
#we need to analysis data for every unique ID
unique_ids <- unique(data$ID)

#create a dataframe to save r^2 AIC BIC and RSS
model_metrics <- data.frame(ID = character(), 
                            Popunit = character(), 
                            Model = character(), 
                            R_Squared = numeric(), 
                            AIC = numeric(),
                            BIC = numeric(),
                            RSS = numeric(), 
                            stringsAsFactors = FALSE)


#create a empty df to save our params of logistic and gompertz model
params_logistic_df <- data.frame(ID = character(), r_max = numeric(),
                                 K = numeric(), N_0 = numeric(), stringsAsFactors = FALSE)
params_gompertz_df <- data.frame(ID = character(), r_max = numeric(), K = numeric(), N_0 = numeric(), 
                                 t_lag = numeric(), stringsAsFactors = FALSE)
for (id in unique_ids) {
  subset <- subset(data, ID == id)#create a subset with unique ID
  popunit <- unique(subset$PopBio_units)
  #Quadratic linear fitting of subset
  fit_quad <- lm(PopBio ~ Time + I(Time^2), data = subset)
  # cubic linear fitting of subset
  fit_cubic <- lm(PopBio ~ Time + I(Time^2) + I(Time^3), data = subset)
    
  if (exists("fit_quad")) {
    rss_quad <- sum(residuals(fit_quad)^2)#calculate rss
    tss_quad <- sum((subset$PopBio - mean(subset$PopBio))^2)#calculate tss
    r_squared_quad <- 1 - (rss_quad/tss_quad)# use equation to get R^2
    n_quad <- nrow(subset)
    p_quad <- length(coef(fit_quad))
    aic_quad <- n_quad + 2 + n_quad * log((2 * pi) / n_quad) + n_quad * log(rss_quad) + 2 * p_quad# get AIC
    bic_quad <- BIC(fit_quad)
    model_metrics <- rbind(model_metrics, #add them to data_frame
                           data.frame(ID = id, popunit = popunit, Model = "Quadratic Linear", 
                                      R_Squared = r_squared_quad, AIC = aic_quad, BIC = bic_quad, RSS = rss_quad))
  }
  if (exists("fit_cubic")) {
    rss_cubic <- sum(resid(fit_cubic)^2)#calculate rss
    tss_cubic <- sum((subset$PopBio - mean(subset$PopBio))^2)# get Rtss
    r_squared_cubic <- 1 - (rss_cubic/tss_cubic)# get R^2
    n_cubic <- nrow(subset)
    p_cubic <- length(coef(fit_cubic))
    aic_cubic <- n_cubic + 2 + n_cubic * log((2 * pi) / n_cubic) + n_cubic * log(rss_cubic) + 2 * p_cubic #aic 
    bic_cubic <- BIC(fit_cubic)# bic
    model_metrics <- rbind(model_metrics, c(id, popunit,  "Cubic Linear", r_squared_cubic, aic_cubic, bic_cubic, rss_cubic))
  }# add them to data frame
  
}


for (id in unique_ids) {
  subset <- subset(data, ID == id)#create a subset with unique ID
  popunit <- unique(subset$PopBio_units)
  #define the starting value
  N_0_start <- min(subset$PopBio) # lowest population size
  K_start <- max(subset$PopBio) # highest population size
  r_max_start <- 0.004
  fit_logistic <- try(nlsLM(PopBio ~ logistic_model(Time, r_max, K, N_0), data = subset, 
                            start = list(r_max = r_max_start, N_0 = N_0_start, K = K_start),control = list(maxiter = 200)),
                      silent = T)#model fitting
  
  if (class(fit_logistic) != "try-error") {
    params_logistic <- coef(fit_logistic)#save the coef we get
    params_logistic_df <- rbind(params_logistic_df, data.frame(ID = id, N_0 = params_logistic["N_0"], 
                                                               K = params_logistic["K"], r_max = params_logistic["r_max"]))
  }
  if (class(fit_logistic) != "try-error") {
    rss_logistic <- sum(residuals(fit_logistic)^2)#rss 
    tss_logistic <- sum((subset$PopBio - mean(subset$PopBio))^2)#tss
    r_squared_logistic <- 1 - (rss_logistic/tss_logistic)#r^2
    aic_logistic <- AIC(fit_logistic)#aic
    bic_logistic <- BIC(fit_logistic)#bic
    model_metrics <- rbind(model_metrics, c(id, popunit, "Logistic", r_squared_logistic, aic_logistic, bic_logistic, rss_logistic))
  }#save
}

for (id in unique_ids) {
  subset <- subset(data, ID == id)
  popunit <- unique(subset$PopBio_units)
  N_0_start <- min(subset$logP) # lowest population size
  K_start <- max(subset$logP) # highest population size
  r_max_start <- 0.004
  t_lag_start <- subset$Time[which.max(diff(diff(subset$logP)))]# find last timepoint of lag phase
  fit_gompertz <- try(nlsLM(logP ~ gompertz_model(Time, r_max, K, N_0, t_lag), data = subset, 
                            start = list(t_lag = t_lag_start, r_max = r_max_start, N_0 = N_0_start, K = K_start), control = list(maxiter = 200)),
                      silent = T)#model fitting
  
  if (class(fit_gompertz) != "try-error") {
    params_gompertz <- coef(fit_gompertz)#save the coef we get 
    params_gompertz_df <- rbind(params_gompertz_df, data.frame(ID = id, r_max = params_gompertz["r_max"], K = params_gompertz["K"], 
                                                               N_0 = params_gompertz["N_0"], t_lag = params_gompertz["t_lag"]))
  }
  if (class(fit_gompertz) != "try-error") {
    rss_gompertz <- sum(residuals(fit_gompertz)^2)#same as above calculate rss
    tss_gompertz <- sum((subset$logP - mean(subset$logP))^2)#tss
    r_squared_gompertz <- 1 - (rss_gompertz/tss_gompertz)#r^2
    aic_gompertz <- AIC(fit_gompertz)#aic
    bic_gompertz <- BIC(fit_gompertz)#bic
    model_metrics <- rbind(model_metrics, c(id, popunit, "Gompertz", r_squared_gompertz, aic_gompertz, bic_gompertz, rss_gompertz))
  }#save
}

# arrange the dataframe as ID order
model_metrics <- model_metrics %>% arrange(ID)

#remove the infinite value
model_metrics <- model_metrics %>%
  filter(!is.infinite(AIC), !is.infinite(BIC), !is.infinite(R_Squared), !is.infinite(RSS))

#save our dataframe as csv
write.csv(model_metrics, "../results/model_metrics.csv", row.names = FALSE)
write.csv(params_logistic_df, "../results/params_logistic.csv", row.names = FALSE)
write.csv(params_gompertz_df, "../results/params_gompertz.csv", row.names = FALSE)

