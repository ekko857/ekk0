getwd()
se_score <- readRDS("Data/Stress_datasets/RESET_SE_score_Ransome.RDS")

cumul_score <- readRDS("Data/Stress_datasets/RESET_cumul_score_Ransome.RDS")
class(cumul_score)
library(dplyr)

average_reset_score <- cumul_score %>%
  summarise(mean_reset = mean(RESET_score, na.rm = TRUE))
print(average_reset_score)

# 查看 cumul_score 数据框的分组变量
group_vars <- groups(cumul_score)
print(group_vars)
cumul_score_df <- as.data.frame(cumul_score)
se_score_df <- as.data.frame(se_score)
library(dplyr)
library(tidyr)

sample_data_df<- as.data.frame(sample_data(data))
sample_data_df$sample_name <- rownames(sample_data_df)
hill_numbers$sample_name <- rownames(hill_numbers)

# 确保 `date` 列是日期类型
combined_data$date <- as.Date(combined_data$date)


ggplot(cumul_score_df, aes(x = date, y = RESET_score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "RESET_score over Time",
       x = "Date",
       y = "RESET_score") +
  theme_minimal()






str(se_score_df)
se_score_df$Year <- as.numeric(as.character(se_score_df$Year))
se_score_df$date <- as.Date(se_score_df$date)
se_score_df <- se_score_df %>% drop_na()

# 描述性统计分析
summary(se_score_df)

# 时间序列分析
ggplot(se_score_df, aes(x = date, y = SE_score, color = stress_category)) +
  geom_line() +
  labs(title = "Time Series of SE Score by Stress Category", x = "Date", y = "SE Score")



# 获取唯一的变量列表
variables <- unique(se_score_df$variable)

# 创建时间序列图，按变量分面
ggplot(se_score_df, aes(x = date, y = SE_score, color = variable)) +
  geom_point() +
  labs(title = "Time Series of SE Score by Variable",
       x = "Date",
       y = "SE Score") +
  theme_minimal() +
  facet_wrap(~ variable, scales = "free_y")

# 绘制箱线图
ggplot(se_score_df, aes(x = variable, y = SE_score, fill = variable)) +
  geom_boxplot() +
  labs(title = "Boxplot of SE Score by Variable",
       x = "Variable",
       y = "SE Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))








# 合并 hill_numbers 和 sample_data_df 数据框
hill_numbers_with_sample <- left_join(hill_numbers, sample_data_df, by = "sample_name")
combined_data <- left_join(hill_numbers_with_sample, cumul_score_df, by = "eventID")



# 将DMS格式转换为十进制度格式的函数
convert_dms_to_dd <- function(dms) {
  # 正则表达式匹配DMS格式
  dms_pattern <- "^(-?\\d+)° (\\d+)' (\\d+\\.?\\d*)\" ([NSEW])$"
  
  matches <- regmatches(dms, regexec(dms_pattern, dms))
  if (length(matches[[1]]) == 0) {
    stop("Invalid DMS format")
  }
  
  degrees <- as.numeric(matches[[1]][2])
  minutes <- as.numeric(matches[[1]][3])
  seconds <- as.numeric(matches[[1]][4])
  direction <- matches[[1]][5]
  
  # 转换为十进制度
  dd <- degrees + minutes / 60 + seconds / 3600
  if (direction %in% c("S", "W")) {
    dd <- -dd
  }
  
  return(dd)
}

combined_data <- combined_data %>%
  mutate(
    DecimalLongitude = sapply(DMSLongitude, convert_dms_to_dd),
    DecimalLatitude = sapply(DMSLatitude, convert_dms_to_dd)
  )

# 按 `eventID` 分组，并选择每组中最新的日期记录
latest_data <- combined_data %>%
  group_by(eventID) %>%
  filter(date == max(date)) %>%
  ungroup()


# 绘制散点图
ggplot(latest_data, aes(x = RESET_score, y = N0)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter plot of N0 vs RESET_score",
       x = "RESET Score",
       y = "N0 (Hill Number)")

ggplot(latest_data, aes(x = RESET_score, y = N1)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter plot of N1 vs RESET_score",
       x = "RESET Score",
       y = "N1 (Hill Number)")

ggplot(latest_data, aes(x = RESET_score, y = N2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter plot of N2 vs RESET_score",
       x = "RESET Score",
       y = "N2 (Hill Number)")

correlation_N0 <- cor(latest_data$RESET_score, latest_data$N0)
correlation_N1 <- cor(latest_data$RESET_score, latest_data$N1)
correlation_N2 <- cor(latest_data$RESET_score, latest_data$N2)

print(paste("Correlation between RESET score and N0: ", correlation_N0))
print(paste("Correlation between RESET score and N1: ", correlation_N1))
print(paste("Correlation between RESET score and N2: ", correlation_N2))

lm_N0 <- lm(N0 ~ RESET_score, data = latest_data)
lm_N1 <- lm(N1 ~ RESET_score, data = latest_data)
lm_N2 <- lm(N2 ~ RESET_score, data = latest_data)

summary(lm_N0)
summary(lm_N1)
summary(lm_N2)






# 分组进行线性回归分析
regression_results <- latest_data %>%
  group_by(locationID) %>%
  do(model = lm(N0 ~ RESET_score, data = .))

# 查看回归结果
summary(regression_results$model)

# 绘制每个位置的散点图和回归线
ggplot(latest_data, aes(x = RESET_score, y = N0)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~ locationID) +
  labs(title = "Scatter plot of N0 vs RESET_score by Location",
       x = "RESET Score",
       y = "N0 (Hill Number)")




anova_result <- aov(N0 ~ locationID, data = latest_data)
summary(anova_result)
# 绘制不同地区的箱线图
ggplot(latest_data, aes(x = locationID, y = N0)) +
  geom_boxplot() +
  labs(title = "Boxplot of N0 (Hill Number) by Location",
       x = "Location",
       y = "N0 (Hill Number)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




# 绘制图表
library(ggplot2)

# 绘制N0 vs DecimalLatitude的散点图
ggplot(latest_data, aes(x = DecimalLatitude, y = N0)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter plot of N0 vs Decimal Latitude",
       x = "Decimal Latitude",
       y = "Hill Number (N0)") +
  theme_minimal()

# 绘制N0 vs DecimalLongitude的散点图
ggplot(latest_data, aes(x = DecimalLongitude, y = N0)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter plot of N0 vs Decimal Longitude",
       x = "Decimal Longitude",
       y = "Hill Number (N0)") +
  theme_minimal()

# 对经度和Hill Number (N0)进行线性回归分析
lm_N0_lon <- lm(N0 ~ DecimalLongitude, data = latest_data)
summary(lm_N0_lon)

# 对纬度和Hill Number (N0)进行线性回归分析
lm_N0_lat <- lm(N0 ~ DecimalLatitude, data = latest_data)
summary(lm_N0_lat)


summary_stats <- cumul_score %>%
  summarise(
    mean_reset = mean(RESET_score, na.rm = TRUE),
    sd_reset = sd(RESET_score, na.rm = TRUE),
    min_reset = min(RESET_score, na.rm = TRUE),
    max_reset = max(RESET_score, na.rm = TRUE)
  )
print(summary_stats)
