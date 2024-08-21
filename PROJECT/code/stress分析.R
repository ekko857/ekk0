#####load packages
library(phyloseq)
library(ggplot2)
library(vegan)
library(dplyr)
#加载数据
se_score <- readRDS("Data/Stress_datasets/RESET_SE_score_Ransome.RDS")
cumul_score <- readRDS("Data/Stress_datasets/RESET_cumul_score_Ransome.RDS")

hill_numbers <- read.csv("data/hill_numbers.csv", row.names = 1)
sample_data_df <- read.csv("data/sample_data.csv", row.names = 1)
diversity_df <- read.csv("data/bibury.csv", row.names = 1)

##group_vars <- groups(reset_score)
#print(group_vars)

# 绘制时间序列图
ggplot(cumul_score, aes(x = date, y = RESET_score)) +
  geom_line() +
  geom_point() +
  labs(title = "RESET Score Over Time", x = "Date", y = "RESET Score") +
  theme_minimal()

# 绘制箱线图，展示不同事件的RESET_score分布
ggplot(cumul_score, aes(x = eventID, y = RESET_score)) +
  geom_boxplot() +
  labs(title = "RESET Score by Event ID", x = "Event ID", y = "RESET Score") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# 计算每个 eventID 的总体 RESET_score 均值
mean_reset_score_by_id <- cumul_score %>%
  group_by(eventID) %>%
  summarize(mean_reset_score = mean(RESET_score, na.rm = TRUE))

# 查看结果
#print(mean_reset_score_by_id)
# 合并两个数据集 hill number和 reset score mean

sample_data_df$sample_name <- rownames(sample_data_df)
hill_numbers$sample_name <- rownames(hill_numbers)
hill_numbers_with_sample <- left_join(hill_numbers, sample_data_df, by = "sample_name")
diversity_df$sample_name <- diversity_df$Sample
hill_numbers_with_sample <- left_join(hill_numbers_with_sample, diversity_df, by = 'sample_name')
hill_numbers_with_reset_score <- left_join(hill_numbers_with_sample, mean_reset_score_by_id, by = "eventID")

# 绘制 N0 与 mean_RESET_score 之间的关系图
ggplot(hill_numbers_with_reset_score, aes(x = mean_reset_score, y = N0)) +
  geom_point() +  # 添加散点
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # 添加回归线
  labs(title = "Relationship between Mean RESET Score and N0",
       x = "Mean RESET Score",
       y = "N0") +
  theme_minimal()
str(hill_numbers_with_reset_score)
summary(hill_numbers_with_reset_score)
# 计算相关性系数
correlation <- cor(hill_numbers_with_reset_score$mean_reset_score, hill_numbers_with_reset_score$N0, use = "complete.obs")
print(paste("Correlation coefficient between Mean RESET Score and N0: ", correlation))

# 绘制 N1 与 mean_RESET_score 之间的关系图
ggplot(hill_numbers_with_reset_score, aes(x = mean_reset_score, y = N1)) +
  geom_point() +  # 添加散点
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # 添加回归线
  labs(title = "Relationship between Mean RESET Score and N1",
       x = "Mean RESET Score",
       y = "N1") +
  theme_minimal()

# 计算 N1 的相关性系数
correlation_N1 <- cor(hill_numbers_with_reset_score$mean_reset_score, hill_numbers_with_reset_score$N1, use = "complete.obs")
print(paste("Correlation coefficient between Mean RESET Score and N1: ", correlation_N1))

# 绘制 N2 与 mean_RESET_score 之间的关系图
ggplot(hill_numbers_with_reset_score, aes(x = mean_reset_score, y = N2)) +
  geom_point() +  # 添加散点
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # 添加回归线
  labs(title = "Relationship between Mean RESET Score and N2",
       x = "Mean RESET Score",
       y = "N2") +
  theme_minimal()

# 计算 N2 的相关性系数
correlation_N2 <- cor(hill_numbers_with_reset_score$mean_reset_score, hill_numbers_with_reset_score$N2, use = "complete.obs")
print(paste("Correlation coefficient between Mean RESET Score and N2: ", correlation_N2))



# 添加一个小常数（例如1）来避免对数0的情况
hill_numbers_with_reset_score <- hill_numbers_with_reset_score %>%
  mutate(log_N0 = log(N0 + 1),
         log_N1 = log(N1 + 1),
         log_N2 = log(N2 + 1),
         log_mean_reset_score = log(mean_reset_score + 1))

# 绘制log(N0)与log(RESET分数)的关系图
ggplot(hill_numbers_with_reset_score, aes(x =log_mean_reset_score, y = log_N0)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Log-Mean RESET Score vs Log-Hill Number (N0)",
       x = "Log-Mean RESET Score",
       y = "Log-Hill Number (N0)") +
  theme_minimal()
# 绘制log(N1)与log(RESET分数)的关系图
ggplot(hill_numbers_with_reset_score, aes(x = log_mean_reset_score, y = log_N1)) +
  geom_point() +
  geom_smooth(method = "lm", color = "green") +
  labs(title = "Log-Mean RESET Score vs Log-Hill Number (N1)",
       x = "Log-Mean RESET Score",
       y = "Log-Hill Number (N1)") +
  theme_minimal()

# 绘制log(N2)与log(RESET分数)的关系图
ggplot(hill_numbers_with_reset_score, aes(x = log_mean_reset_score, y = log_N2)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Log-Mean RESET Score vs Log-Hill Number (N2)",
       x = "Log-Mean RESET Score",
       y = "Log-Hill Number (N2)") +
  theme_minimal()


# 计算Pearson相关系数
cor_pearson <- cor(hill_numbers_with_reset_score$log_N0, hill_numbers_with_reset_score$log_mean_reset_score, use = "complete.obs")
cor_spearman <- cor(hill_numbers_with_reset_score$N0, hill_numbers_with_reset_score$mean_reset_score, use = "complete.obs", method = "spearman")

print(cor_pearson)
print(cor_spearman)

# 多项式回归
poly_lm_model <- lm(N0 ~ poly(mean_reset_score, 2), data = hill_numbers_with_reset_score)
summary(poly_lm_model)

# 绘制多项式回归结果
ggplot(hill_numbers_with_reset_score, aes(x = mean_reset_score, y = N0)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "blue") +
  labs(title = "Polynomial Regression: Mean RESET Score vs Hill Number (N0)",
       x = "Mean RESET Score",
       y = "Hill Number (N0)") +
  theme_minimal()

#引入其他潜在影响因素
multi_lm_model <- lm(N0 ~ mean_reset_score + N1 + N2, data = hill_numbers_with_reset_score)
summary(multi_lm_model)
multi_log_lm_model <- lm(log_N0 ~ log_mean_reset_score + log_N1 + log_N2, data = hill_numbers_with_reset_score)
summary(multi_log_lm_model)





ggplot(hill_numbers_with_reset_score, aes(x = mean_reset_score, y = Bibury_Diversity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Relationship between Bibury Diversity and Reset Score",
       x = "Reset Score",
       y = "Bibury Diversity") +
  theme_minimal()














######################################
head(se_score)
# 查看数据的基本情况
str(se_score)
summary(se_score)

# 清洗数据，去除缺失值
se_score_clean <- se_score %>% filter(!is.na(SE_score))
# 绘制箱线图，查看不同变量的SE得分分布
ggplot(se_score_clean, aes(x = variable, y = SE_score)) +
  geom_boxplot() +
  labs(title = "SE Score Distribution by Variable",
       x = "Variable",
       y = "SE Score") +
  theme_minimal()

# 绘制时间序列图，针对某个变量
ggplot(se_score_clean %>% filter(variable == "wind"), aes(x = date, y = SE_score)) +
  geom_line() +
  geom_point() +
  labs(title = "SST SE Score over Time",
       x = "Date",
       y = "SE Score") +
  theme_minimal()

# 绘制时间序列图，使用facet_wrap创建小图
ggplot(se_score_clean %>% filter(eventID == "Ransome_BAA02_BAA02A_120312_150526"), aes(x = date, y = SE_score)) +
  geom_line() +
  geom_point() +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "SE Score over Time for a Specific Event",
       x = "Date",
       y = "SE Score") +
  theme_minimal()

# 计算每个 eventID 和 variable 下的 SE_score 的均值
mean_se_score <- se_score_clean %>%
  group_by(eventID, variable) %>%
  summarise(mean_SE_score = mean(SE_score, na.rm = TRUE), .groups = "drop")

#合并 mean_se_score和 hill number
hill_numbers_with_score <- left_join(hill_numbers_with_reset_score, mean_se_score, by = "eventID")
print(hill_numbers_with_score)

#####
ggplot(hill_numbers_with_score, aes(x = mean_SE_score, y = N0)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Mean SE Score vs Hill Number (N0) by Variable",
       x = "Mean SE Score",
       y = "Hill Number (N0)") +
  theme_minimal()


















####################
# 定义绘图函数
plot_reset <- function(data) {
  # 绘制N0与mean reset score的关系
  p1 <- ggplot(data, aes(x = mean_reset_score, y = N0)) +
    geom_point() +
    geom_smooth(method = "lm", col = "blue") +
    labs(title = "N0 vs Mean Reset Score",
         x = "Mean Reset Score",
         y = "N0")
  
  # 绘制N1与mean reset score的关系
  p2 <- ggplot(data, aes(x = mean_reset_score, y = N1)) +
    geom_point() +
    geom_smooth(method = "lm", col = "blue") +
    labs(title = "N1 vs Mean Reset Score",
         x = "Mean Reset Score",
         y = "N1")
  
  # 绘制N2与mean reset score的关系
  p3 <- ggplot(data, aes(x = mean_reset_score, y = N2)) +
    geom_point() +
    geom_smooth(method = "lm", col = "blue") +
    labs(title = "N2 vs Mean Reset Score",
         x = "Mean Reset Score",
         y = "N2")
  
  # 绘制Bibury_Diversity与mean reset score的关系
  p4 <- ggplot(data, aes(x = mean_reset_score, y = Bibury_Diversity)) +
    geom_point() +
    geom_smooth(method = "lm", col = "blue") +
    labs(title = "Bibury Diversity vs Mean Reset Score",
         x = "Mean Reset Score",
         y = "Bibury Diversity")
  
  # 使用gridExtra包将四个图表合并在一张图上
  library(gridExtra)
  grid.arrange(p1, p2, p3, p4, ncol = 2)
}

# 示例调用函数，假设数据框为data_100
plot_reset(sample_100)
plot_reset(sample_500)
plot_reset(sample_sessile)

plot_reset_log <- function(data) {
  # 绘制N0与mean reset score的关系
  p1 <- ggplot(data, aes(x = mean_reset_score, y = N0_log)) +
    geom_point() +
    geom_smooth(method = "lm", col = "blue") +
    labs(title = "N0_log vs Mean Reset Score",
         x = "Mean Reset Score",
         y = "N0_log")
  
  # 绘制N1与mean reset score的关系
  p2 <- ggplot(data, aes(x = mean_reset_score, y = N1_log)) +
    geom_point() +
    geom_smooth(method = "lm", col = "blue") +
    labs(title = "N1_log vs Mean Reset Score",
         x = "Mean Reset Score",
         y = "N1_log")
  
  # 绘制N2与mean reset score的关系
  p3 <- ggplot(data, aes(x = mean_reset_score, y = N2_log)) +
    geom_point() +
    geom_smooth(method = "lm", col = "blue") +
    labs(title = "N2_log vs Mean Reset Score",
         x = "Mean Reset Score",
         y = "N2_log")
  
  # 绘制Bibury_Diversity与mean reset score的关系
  p4 <- ggplot(data, aes(x = mean_reset_score, y = Bibury_Diversity)) +
    geom_point() +
    geom_smooth(method = "lm", col = "blue") +
    labs(title = "Bibury Diversity vs Mean Reset Score",
         x = "Mean Reset Score",
         y = "Bibury Diversity")
  
  # 使用gridExtra包将四个图表合并在一张图上
  library(gridExtra)
  grid.arrange(p1, p2, p3, p4, ncol = 2)
}
plot_reset_log(sample_100)
plot_reset_log(sample_500)
plot_reset_log(sample_sessile)


plot_reset_diff_log <- function(data) {
  # 绘制N0与mean reset score的关系
  p1 <- ggplot(data, aes(x = diff_reset_score, y = N0_log)) +
    geom_point() +
    geom_smooth(method = "lm", col = "blue") +
    labs(title = "N0_log vs Mean Reset Score",
         x = "Mean Reset Score",
         y = "N0_log")
  
  # 绘制N1与mean reset score的关系
  p2 <- ggplot(data, aes(x = diff_reset_score, y = N1_log)) +
    geom_point() +
    geom_smooth(method = "lm", col = "blue") +
    labs(title = "N1_log vs Mean Reset Score",
         x = "Mean Reset Score",
         y = "N1_log")
  
  # 绘制N2与mean reset score的关系
  p3 <- ggplot(data, aes(x = diff_reset_score, y = N2_log)) +
    geom_point() +
    geom_smooth(method = "lm", col = "blue") +
    labs(title = "N2_log vs Mean Reset Score",
         x = "Mean Reset Score",
         y = "N2_log")
  
  # 绘制Bibury_Diversity与mean reset score的关系
  p4 <- ggplot(data, aes(x = diff_reset_score, y = Bibury_Diversity)) +
    geom_point() +
    geom_smooth(method = "lm", col = "blue") +
    labs(title = "Bibury Diversity vs Mean Reset Score",
         x = "Mean Reset Score",
         y = "Bibury Diversity")
  # 使用gridExtra包将四个图表合并在一张图上
  library(gridExtra)
  grid.arrange(p1, p2, p3, p4, ncol = 2)
}

plot_reset_diff_log(sample_100)


# 绘制时间序列图
# 使用 facet_wrap 创建小图
ggplot(cumul_score, aes(x = date, y = RESET_score)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ eventID, scales = "free_y") +
  labs(title = "RESET Score over Time for Each Event",
       x = "Date",
       y = "RESET Score") +
  theme_minimal()
