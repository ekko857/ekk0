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
getwd()
setwd("Documents/PROJECT")
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

hill_numbers_with_stress <- left_join(hill_numbers_with_sample, result, by = "eventID")
hill_numbers_with_human <- left_join(hill_numbers_with_sample, human_data, by = 'eventID')
hill_numbers_with_se <- left_join(hill_numbers_with_sample, mean_se_score, by = 'eventID')


