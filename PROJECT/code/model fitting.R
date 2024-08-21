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


# 显示图形
print(combined_plot)
# 拟合模型

model_N0 <- lm(N0 ~ mean_smoothed_RESET_score * sampleSizeFractionation, data = hill_numbers_with_stress)
model_N1 <- lm(N1 ~ mean_smoothed_RESET_score * sampleSizeFractionation, data = hill_numbers_with_stress)
model_N2 <- lm(N2 ~ mean_smoothed_RESET_score * sampleSizeFractionation, data = hill_numbers_with_stress)

summary(model_N0)
summary(model_N1)
summary(model_N2)
# 获取模型摘要并转换为数据框
summary_N0 <- tidy(model_N0)
summary_N1 <- tidy(model_N1)
summary_N2 <- tidy(model_N2)

# 添加响应变量列以区分数据
summary_N0$Response <- "N0"
summary_N1$Response <- "N1"
summary_N2$Response <- "N2"

# 合并数据
summary_all <- rbind(summary_N0, summary_N1, summary_N2)

write.csv(summary_all, 'summary_reset.csv', row.names = FALSE)

# 输出数据框
print(summary_all)


# 预测 N0, N1, N2
hill_numbers_with_stress$predicted_N0 <- predict(lm(N0 ~ mean_smoothed_RESET_score * sampleSizeFractionation, data = hill_numbers_with_stress))
hill_numbers_with_stress$predicted_N1 <- predict(lm(N1 ~ mean_smoothed_RESET_score * sampleSizeFractionation, data = hill_numbers_with_stress))
hill_numbers_with_stress$predicted_N2 <- predict(lm(N2 ~ mean_smoothed_RESET_score * sampleSizeFractionation, data = hill_numbers_with_stress))

# 绘制 N0 的图
plot_N0 <- ggplot(hill_numbers_with_stress, aes(x = mean_smoothed_RESET_score, y = N0, color = sampleSizeFractionation)) +
  geom_point() +
  geom_line(aes(y = predicted_N0), linetype = "dashed") +
  labs( x = "Mean RESET Score", y = "N0") +
  scale_color_manual(name = "Sample size Fractionation",   # 更改图例标题
                     values = c("100" = "#ff5575", "500" = "#14bc94", "sessile" = "#6299ff"), # 示例颜色映射
                     labels = c("100" = "100 μm", "500" = "500 μm", "sessile" = "Sessile")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = "white"), 
        panel.background = element_rect(fill = "white", color = "white"))

# 绘制 N1 的图
plot_N1 <- ggplot(hill_numbers_with_stress, aes(x = mean_smoothed_RESET_score, y = N1, color = sampleSizeFractionation)) +
  geom_point() +
  geom_line(aes(y = predicted_N1), linetype = "dashed") +
  labs(x = "Mean RESET Score", y = "N1") +
  scale_color_manual(name = "Sample Size Fractionation",   # 更改图例标题
                     values = c("100" = "#ff5575", "500" = "#14bc94", "sessile" = "#6299ff"), # 示例颜色映射
                     labels = c("100" = "100 μm", "500" = "500 μm", "sessile" = "Sessile")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = "white"), 
        panel.background = element_rect(fill = "white", color = "white"))

# 绘制 N2 的图
plot_N2 <- ggplot(hill_numbers_with_stress, aes(x = mean_smoothed_RESET_score, y = N2, color = sampleSizeFractionation)) +
  geom_point() +
  geom_line(aes(y = predicted_N2), linetype = "dashed") +
  labs(x = "Mean RESET Score", y = "N2") +
  scale_color_manual(name = "Sample Size Fractionation",   # 更改图例标题
                     values = c("100" = "#ff5575", "500" = "#14bc94", "sessile" = "#6299ff"), # 示例颜色映射
                     labels = c("100" = "100 μm", "500" = "500 μm", "sessile" = "Sessile")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = "white"), 
        panel.background = element_rect(fill = "white", color = "white"))

# 将三个图合并在一个图中
p1 <- grid.arrange(plot_N0, plot_N1, plot_N2, ncol = 1)
ggsave("plot1.png", plot = p1, width = 10, height = 8, units = "in", dpi = 300)

# 假设您的数据框为 data，包含 columns: eventID, variable, mean_SE_score
data_wide <- hill_numbers_with_se %>%
  pivot_wider(
    names_from = variable,       # 将 variable 列的值用作新的列名
    values_from = mean_SE_score  # mean_SE_score 的值填充到这些新列中
  )


data_wide <- data_wide %>%
  mutate(
    current = ifelse(is.na(current), median(current, na.rm = TRUE), current),
    salinity = ifelse(is.na(salinity), median(salinity, na.rm = TRUE), salinity)
  )

data_wide <- data_wide %>%
  rename(
    SST_variability = `SST variability`,
    SST_anomaly = `SST anomaly`
  )


# 定义感兴趣的变量列表
vars_of_interest <- c("SST_variability", "DHW", "wind", "SST", "cloud", "depth", "SST_anomaly", "salinity", "current")

# 确保目标变量也在数据框中
vars_of_interest_N0 <- c("N0", vars_of_interest)
vars_of_interest_N1 <- c("N1", vars_of_interest)
vars_of_interest_N2 <- c("N2", vars_of_interest)

# 从data_wide中选择这些变量
data_selected_N0 <- data_wide[vars_of_interest_N0]
data_selected_N1 <- data_wide[vars_of_interest_N1]
data_selected_N2 <- data_wide[vars_of_interest_N2]


# 构建随机森林模型
RandomForest_model_N0 <- randomForest(N0 ~ ., data = data_selected_N0, ntree = 1000, mtry = 2, importance = TRUE)
RandomForest_model_N1 <- randomForest(N1 ~ ., data = data_selected_N1, ntree = 1000, mtry = 2, importance = TRUE)
RandomForest_model_N2 <- randomForest(N2 ~ ., data = data_selected_N2, ntree = 1000, mtry = 2, importance = TRUE)


# 显示变量重要性
importance(RandomForest_model_N0)
importance(RandomForest_model_N1)
importance(RandomForest_model_N2)

# 可视化变量重要性
varImpPlot(RandomForest_model_N0)
varImpPlot(RandomForest_model_N1)
varImpPlot(RandomForest_model_N2)


# 合并模型的重要性数据
importance_N0 <- data.frame(Variable = rownames(importance(RandomForest_model_N0)), Importance = importance(RandomForest_model_N0)[, "%IncMSE"], Hill_Number = "N0")
importance_N1 <- data.frame(Variable = rownames(importance(RandomForest_model_N1)), Importance = importance(RandomForest_model_N1)[, "%IncMSE"], Hill_Number = "N1")
importance_N2 <- data.frame(Variable = rownames(importance(RandomForest_model_N2)), Importance = importance(RandomForest_model_N2)[, "%IncMSE"], Hill_Number = "N2")

# 合并数据框
importance_combined <- rbind(importance_N0, importance_N1, importance_N2)


# 计算平均重要性并合并数据
importance_combined <- rbind(importance_N0, importance_N1, importance_N2) %>%
  mutate(Variable = factor(Variable, levels = unique(Variable))) %>%
  group_by(Variable) %>%
  mutate(Average_Importance = mean(Importance)) %>%
  ungroup() %>%
  mutate(Variable = reorder(Variable, Average_Importance))

p2 <- ggplot(importance_combined, aes(x = Variable, y = Importance, fill = Hill_Number)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  coord_flip() +
  labs(x = " ",
       y = "Increase in MSE (%)") +
  scale_x_discrete(labels = c("SST_anomaly" = "SST anomaly", "SST_variability" = "SST variability")) +
  scale_fill_brewer(palette = "Set4") +  
  theme_minimal() +
  theme(legend.title = element_text(face = "bold"), legend.position = "top")

p2

ggsave("p2.png", plot = p2, width = 10, height = 8, units = "in")





# 假设你的数据框名为 se_score_clean
sst_data <- se_score_clean %>%
  filter(variable == "SST")



# 确保日期是正确的Date格式
sst_data$date <- as.Date(sst_data$date)

# 创建一个空的数据框用来保存结果
sst_results <- data.frame(eventID = character(), Mean_SE_Score = numeric(), stringsAsFactors = FALSE)

# 对每个eventID进行处理
sst_data %>%
  group_by(eventID) %>%
  do({
    # 创建时间序列对象，这里假设数据是按月份收集的，根据实际情况调整频率
    ts_data <- ts(.$SE_score, frequency = 12)  # 假设每年有12个月的数据
    
    # 应用Holt-Winters三重指数平滑模型
    tryCatch({
      hw_model <- hw(ts_data, seasonal = "additive")  # 使用加法季节性，可以根据数据特性改为乘法
      
      # 计算所有平滑值的平均
      mean_smoothed <- mean(hw_model$fitted)
      
      # 将结果添加到结果数据框
      data.frame(eventID = unique(.$eventID), Mean_SE_Score = mean_smoothed)
    }, error = function(e) {
      # 错误处理，如果模型失败，则返回NA
      data.frame(eventID = unique(.$eventID), Mean_SE_Score = NA)
    })
  }) %>%
  bind_rows() -> sst_results

# 查看结果
print(sst_results)
sst_results <- sst_results %>% filter(!is.na(Mean_SE_Score))

hill_sst <- left_join(hill_numbers_with_sample, sst_results, by = 'eventID')
hill_sst <- hill_sst %>% filter(!is.na(Mean_SE_Score))



# 模型拟合
SST_model_N0 <- lm(N0 ~ Mean_SE_Score * sampleSizeFractionation, data = hill_sst)
SST_model_N1 <- lm(N1 ~ Mean_SE_Score * sampleSizeFractionation, data = hill_sst)
SST_model_N2 <- lm(N2 ~ Mean_SE_Score * sampleSizeFractionation, data = hill_sst)

summary(SST_model_N0)
summary(SST_model_N1)
summary(SST_model_N2)
# 使用broom包来整理模型的详细统计数据
tidy_N0 <- broom::tidy(SST_model_N0)
tidy_N1 <- broom::tidy(SST_model_N1)
tidy_N2 <- broom::tidy(SST_model_N2)

# 提取模型的R-squared和Adjusted R-squared
glance_N0 <- broom::glance(SST_model_N0)
glance_N1 <- broom::glance(SST_model_N1)
glance_N2 <- broom::glance(SST_model_N2)

# 创建统计数据框
sst_summary <- bind_rows(
  tidy_N0 %>% mutate(Model = "SST_model_N0", R.Squared = glance_N0$r.squared, Adjusted.R.Squared = glance_N0$adj.r.squared),
  tidy_N1 %>% mutate(Model = "SST_model_N1", R.Squared = glance_N1$r.squared, Adjusted.R.Squared = glance_N1$adj.r.squared),
  tidy_N2 %>% mutate(Model = "SST_model_N2", R.Squared = glance_N2$r.squared, Adjusted.R.Squared = glance_N2$adj.r.squared)
)

# 显示数据框
print(sst_summary)

write.csv(sst_summary, 'sst_summary.csv', row.names = FALSE)
# 预测并存储预测值
hill_sst$predicted_N0 <- predict(SST_model_N0, newdata = hill_sst)
hill_sst$predicted_N1 <- predict(SST_model_N1, newdata = hill_sst)
hill_sst$predicted_N2 <- predict(SST_model_N2, newdata = hill_sst)

# 绘制 N0 的图
plot_N0 <- ggplot(hill_sst, aes(x = Mean_SE_Score, y = N0, color = sampleSizeFractionation)) +
  geom_point() +
  geom_line(aes(y = predicted_N0), linetype = "dashed") +
  labs(x = "Mean SE Score of SST", y = "N0") +
  scale_color_manual(name = "Sample Size Fractionation",   # 更改图例标题
                     values = c("100" = "#ff5575", "500" = "#14bc94", "sessile" = "#6299ff"), # 示例颜色映射
                     labels = c("100" = "100 μm", "500" = "500 μm", "sessile" = "Sessile")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = "white"), 
        panel.background = element_rect(fill = "white", color = "white"))

# 绘制 N1 的图
plot_N1 <- ggplot(hill_sst, aes(x = Mean_SE_Score, y = N1, color = sampleSizeFractionation)) +
  geom_point() +
  geom_line(aes(y = predicted_N1), linetype = "dashed") +
  scale_color_manual(name = "Sample Size Fractionation",   # 更改图例标题
                     values = c("100" = "#ff5575", "500" = "#14bc94", "sessile" = "#6299ff"), # 示例颜色映射
                     labels = c("100" = "100 μm", "500" = "500 μm", "sessile" = "Sessile")) +
  labs( x = "Mean SE Score of SST", y = "N1") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = "white"), 
        panel.background = element_rect(fill = "white", color = "white"))

# 绘制 N2 的图
plot_N2 <- ggplot(hill_sst, aes(x = Mean_SE_Score, y = N2, color = sampleSizeFractionation)) +
  geom_point() +
  geom_line(aes(y = predicted_N2), linetype = "dashed") +
  labs( x = "Mean SE Score of SST", y = "N2") +
  scale_color_manual(name = "Sample Size Fractionation",   # 更改图例标题
                     values = c("100" = "#ff5575", "500" = "#14bc94", "sessile" = "#6299ff"), # 示例颜色映射
                     labels = c("100" = "100 μm", "500" = "500 μm", "sessile" = "Sessile")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = "white"), 
        panel.background = element_rect(fill = "white", color = "white"))

# 将三个图合并在一个图中
p3 <- combined_plot <- grid.arrange(plot_N0, plot_N1, plot_N2, ncol = 1)

ggsave("p3.png", plot = p3, width = 10, height = 8, units = "in")




# 假设你的数据框名为 se_score_clean
DHW_data <- se_score_clean %>%
  filter(variable == "DHW")


# 假设你的数据框名为 hill_sst
# 使用 GLM 拟合模型，这里假设使用正态分布
data_wide$sampleSizeFractionation <- as.factor(data_wide$sampleSizeFractionation)
dhw_N0 <- gam(N0 ~ s(DHW) + s(sampleSizeFractionation, bs="re"), data = data_wide)
dhw_N1 <- gam(N1 ~ s(DHW) + s(sampleSizeFractionation, bs="re"), data = data_wide)
dhw_N2 <- gam(N2 ~ s(DHW) + s(sampleSizeFractionation, bs="re"), data = data_wide)

summary(dhw_N0)
summary(dhw_N1)
summary(dhw_N2)

data_wide$p_N0 <- predict(dhw_N0, newdata = data_wide)
data_wide$p_N1 <- predict(dhw_N1, newdata = data_wide)
data_wide$p_N2 <- predict(dhw_N2, newdata = data_wide)


# 获取每个模型的详细统计数据
tidy_N0 <- tidy(dhw_N0)
tidy_N1 <- tidy(dhw_N1)
tidy_N2 <- tidy(dhw_N2)

# 获取每个模型的整体评估数据
glance_N0 <- glance(dhw_N0)
glance_N1 <- glance(dhw_N1)
glance_N2 <- glance(dhw_N2)
# 创建统计数据框，包括更多细节
dhw_summary <- bind_rows(
  mutate(tidy_N0, Model = "N0", AIC = glance_N0$AIC, BIC = glance_N0$BIC, Deviance = glance_N0$deviance, nobs = glance_N0$nobs),
  mutate(tidy_N1, Model = "N1", AIC = glance_N1$AIC, BIC = glance_N1$BIC, Deviance = glance_N1$deviance, nobs = glance_N1$nobs),
  mutate(tidy_N2, Model = "N2", AIC = glance_N2$AIC, BIC = glance_N2$BIC, Deviance = glance_N2$deviance, nobs = glance_N2$nobs)
)

# 打印数据框以查看结果
print(dhw_summary)
write.csv(dhw_summary, 'dhw_summary.csv', row.names = FALSE)
plot_N0 <- ggplot(data_wide, aes(x = DHW, y = N0, color = sampleSizeFractionation)) +
  geom_point() +
  geom_line(aes(y = p_N0), linetype = "dashed") +
  labs( x = "Mean SE Score of DHW", y = "N0") +
  scale_color_manual(name = "Sample Size Fractionation",   # 更改图例标题
                     values = c("100" = "#ff5575", "500" = "#14bc94", "sessile" = "#6299ff"), # 示例颜色映射
                     labels = c("100" = "100 μm", "500" = "500 μm", "sessile" = "Sessile")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = "white"), 
        panel.background = element_rect(fill = "white", color = "white"))

plot_N1 <- ggplot(data_wide, aes(x = DHW, y = N1, color = sampleSizeFractionation)) +
  geom_point() +
  geom_line(aes(y = p_N1), linetype = "dashed") +
  labs(x = "Mean SE Score of DHW", y = "N1") +
  scale_color_manual(name = "Sample Size Fractionation",   # 更改图例标题
                     values = c("100" = "#ff5575", "500" = "#14bc94", "sessile" = "#6299ff"), # 示例颜色映射
                     labels = c("100" = "100 μm", "500" = "500 μm", "sessile" = "Sessile")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = "white"), 
        panel.background = element_rect(fill = "white", color = "white"))

plot_N2 <- ggplot(data_wide, aes(x = DHW, y = N2, color = sampleSizeFractionation)) +
  geom_point() +
  geom_line(aes(y = p_N2), linetype = "dashed") +
  labs(x = "Mean SE Score of DHW", y = "N2") +
  scale_color_manual(name = "Sample Size Fractionation",   # 更改图例标题
                     values = c("100" = "#ff5575", "500" = "#14bc94", "sessile" = "#6299ff"), # 示例颜色映射
                     labels = c("100" = "100 μm", "500" = "500 μm", "sessile" = "Sessile")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = "white"), 
        panel.background = element_rect(fill = "white", color = "white"))

p4 <- grid.arrange(plot_N0, plot_N1, plot_N2, ncol = 1)

ggsave("p4.png", plot = p4, width = 10, height = 8, units = "in")


# 定义感兴趣的变量列表
vars_of_interest <- c( "grav_NC",                                    
                       "sediment",                                   
                       "nutrient",                                   
                       "pop_count",
                       "num_ports",
                       "reef_value" )

# 确保目标变量也在数据框中
vars_of_interest_N0 <- c("N0", vars_of_interest)
vars_of_interest_N1 <- c("N1", vars_of_interest)
vars_of_interest_N2 <- c("N2", vars_of_interest)

# 从data_wide中选择这些变量
data_selected_N0 <- hill_numbers_with_human[vars_of_interest_N0]
data_selected_N1 <- hill_numbers_with_human[vars_of_interest_N1]
data_selected_N2 <- hill_numbers_with_human[vars_of_interest_N2]


data_selected_N0 <- data_selected_N0 %>% filter(!is.na(grav_NC))
data_selected_N1 <- data_selected_N1 %>% filter(!is.na(grav_NC))
data_selected_N2 <- data_selected_N2 %>% filter(!is.na(grav_NC))


# 构建随机森林模型
RandomForest_model_N0 <- randomForest(N0 ~ ., data = data_selected_N0, ntree = 1000, mtry = 2, importance = TRUE)
RandomForest_model_N1 <- randomForest(N1 ~ ., data = data_selected_N1, ntree = 1000, mtry = 2, importance = TRUE)
RandomForest_model_N2 <- randomForest(N2 ~ ., data = data_selected_N2, ntree = 1000, mtry = 2, importance = TRUE)


# 显示变量重要性
importance(RandomForest_model_N0)
importance(RandomForest_model_N1)
importance(RandomForest_model_N2)

# 可视化变量重要性
varImpPlot(RandomForest_model_N0)
varImpPlot(RandomForest_model_N1)
varImpPlot(RandomForest_model_N2)


# 合并模型的重要性数据
importance_N0 <- data.frame(Variable = rownames(importance(RandomForest_model_N0)), Importance = importance(RandomForest_model_N0)[, "%IncMSE"], Hill_Number = "N0")
importance_N1 <- data.frame(Variable = rownames(importance(RandomForest_model_N1)), Importance = importance(RandomForest_model_N1)[, "%IncMSE"], Hill_Number = "N1")
importance_N2 <- data.frame(Variable = rownames(importance(RandomForest_model_N2)), Importance = importance(RandomForest_model_N2)[, "%IncMSE"], Hill_Number = "N2")

# 合并数据框
importance_combined <- rbind(importance_N0, importance_N1, importance_N2)


# 计算平均重要性并合并数据
importance_combined <- rbind(importance_N0, importance_N1, importance_N2) %>%
  mutate(Variable = factor(Variable, levels = unique(Variable))) %>%
  group_by(Variable) %>%
  mutate(Average_Importance = mean(Importance)) %>%
  ungroup() %>%
  mutate(Variable = reorder(Variable, Average_Importance))

p5 <- ggplot(importance_combined, aes(x = Variable, y = Importance, fill = Hill_Number)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  coord_flip() +
  labs(x = ' ', 
       y = "Increase in MSE (%)") +
  scale_fill_brewer(palette = "Set4") +  
  scale_x_discrete(labels = c("reef_value" = "Tourism", "sediment" = "Sediment", "nutrient" = "Nutrient", "pop_count" = "Coastal Developemnt", "grav_NC" = "Fishing", "num_ports" = "Industrial Development")) +
  theme_minimal() +
  theme(legend.title = element_text(face = "bold"), legend.position = "top")

p5
ggsave("p5.png", plot = p5, width = 10, height = 8, units = "in")


