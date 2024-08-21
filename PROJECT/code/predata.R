##
library(tibble)
library(vegan)
library(phyloseq)
library(dplyr)
library(ggplot2)
library(mgcv)
library(forecast)
library(broom) 
library(gridExtra)
library(cowplot)
library(randomForest)
getwd()
setwd("/Users/3kko/Documents/CMEECourseWork/PROJECT")
data <- readRDS("data/16S/ps_16S_cESVs_clean_Ransome.RDS")
se_score <- readRDS("Data/Stress_datasets/RESET_SE_score_Ransome.RDS")
reset_score <- readRDS("Data/Stress_datasets/RESET_cumul_score_Ransome.RDS")
human_data <- read.csv('data/Stress_datasets/FR_AnthroStress_Biogeo_data_Ransome_15042024.csv')
hill_numbers <- read.csv("data/hill_numbers.csv", row.names = 1)
sample_data <- sample_data(data)
sample_data <- as.matrix(sample_data)
sample_data <- as.data.frame(sample_data)
sample_data$sample_name <- rownames(sample_data)
hill_numbers_with_sample <- left_join(hill_numbers, sample_data, by = 'sample_name')

otu <- otu_table(data)
otu <- as.matrix(otu)
otu <- as.data.frame(otu)

# 计算每个 eventID 和 variable 下的 SE_score 的均值
# 清洗数据，去除缺失值
se_score_clean <- se_score %>% filter(!is.na(SE_score))

mean_se_score <- se_score_clean %>%
  group_by(eventID, variable) %>%
  summarise(mean_SE_score = mean(SE_score, na.rm = TRUE), .groups = "drop")

# 进行双重指数平滑处理
data_smoothed <- reset_score %>%
  group_by(eventID) %>%
  arrange(eventID) %>%
  do({
    ts_data <- ts(.$RESET_score, start = 1, frequency = 1)
    fit <- HoltWinters(ts_data, beta = TRUE, gamma = FALSE)  # 双重指数平滑
    smoothed <- as.data.frame(fit$fitted)
    smoothed_RESET_score <- c(rep(NA, length(ts_data) - nrow(smoothed)), smoothed$xhat)
    data.frame(eventID = .$eventID, smoothed_RESET_score)
  })

# 计算每个 eventID 的 smoothed_RESET_score 均值
result <- data_smoothed %>%
  group_by(eventID) %>%
  summarise(mean_smoothed_RESET_score = mean(smoothed_RESET_score, na.rm = TRUE))



hill_numbers_with_stress <- left_join(hill_numbers_with_sample, result, by = "eventID")
hill_numbers_with_human <- left_join(hill_numbers_with_sample, human_data, by = 'eventID')
hill_numbers_with_se <- left_join(hill_numbers_with_sample, mean_se_score, by = 'eventID')


