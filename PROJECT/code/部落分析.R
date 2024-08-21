
#
otu_table <- otu_table(data)
tax_table <- tax_table(data)

library(dplyr)
library(tidyr)
# 假设你的 OTU 表和 tax 表是 data.frames
# 将它们转换为 phyloseq 对象
otu_mat <- as.matrix(otu_table)
tax_mat <- as.matrix(tax_table)
OTU <- otu_table(otu_mat, taxa_are_rows = TRUE)
TAX <- tax_table(tax_mat)

# 创建 phyloseq 对象
physeq <- phyloseq(OTU, TAX)

# 提取 OTU 表
otu_df <- as.data.frame(otu_table(physeq))
otu_df$OTU <- rownames(otu_df)

# 提取 tax 表
tax_df <- as.data.frame(tax_table(physeq))
tax_df$OTU <- rownames(tax_df)

# 合并 OTU 表和 tax 表
merged_df <- merge(otu_df, tax_df, by = "OTU")

# 自动选择所有样本列
sample_columns <- merged_df %>% select(where(is.numeric)) %>% colnames()

# 转换为长格式，方便后续汇总
merged_long <- merged_df %>%
  pivot_longer(cols = all_of(sample_columns), names_to = "Sample", values_to = "Abundance")

# 过滤掉丰度为0的记录
merged_long <- merged_long %>%
  filter(Abundance > 0)

# 汇总每个样本中 phylum 的数量
phylum_counts_long <- merged_long %>%
  group_by(Sample, phylum) %>%
  summarize(Total_Abundance = sum(Abundance))

# 查看汇总结果
print(phylum_counts_long)

phylum_counts_long <- phylum_counts_long %>%
  rename(sample_name = Sample)





library(ggplot2)

# 条形图：展示每个样本中不同 phylum 的总丰度
ggplot(phylum_counts_long, aes(x = sample_name, y = Total_Abundance, fill = phylum)) +
  geom_bar(stat = "identity", position = "stack") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Phylum Abundance in Each Sample",
       x = "Sample",
       y = "Total Abundance",
       fill = "Phylum")

# 叠加面积图：展示每个样本中不同 phylum 的总丰度变化
ggplot(phylum_counts_long, aes(x = sample_name, y = Total_Abundance, fill = phylum)) +
  geom_area(position = "stack") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Phylum Abundance in Each Sample",
       x = "Sample",
       y = "Total Abundance",
       fill = "Phylum")



# 箱线图：展示不同 phylum 在所有样本中的丰度分布
ggplot(phylum_counts_long, aes(x = phylum, y = Total_Abundance, fill = phylum)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Distribution of Phylum Abundance Across Samples",
       x = "Phylum",
       y = "Total Abundance",
       fill = "Phylum")




library(tidyr)
library(dplyr)
library(pheatmap)
install.packages('pheatmap')
# 假设 phylum_counts_long 数据框已经存在
# 确保 phylum_counts_long 有 sample_name, phylum 和 Total_Abundance 列

# 转换为宽格式数据框
phylum_counts_wide <- phylum_counts_long %>%
  pivot_wider(names_from = phylum, values_from = Total_Abundance, values_fill = list(Total_Abundance = 0))

# 设置 sample_name 为行名
row.names(phylum_counts_wide) <- phylum_counts_wide$sample_name
phylum_counts_wide <- phylum_counts_wide[,-1]  # 去掉 sample_name 列

# 检查数据框
head(phylum_counts_wide)

# 检查数据框是否有 NA 值
sum(is.na(phylum_counts_wide))

# 如果有 NA 值，可以用 0 替换
phylum_counts_wide[is.na(phylum_counts_wide)] <- 0

phylum_counts_wide <- scale(phylum_counts_wide)

# 绘制热图
pheatmap(as.matrix(phylum_counts_wide), 
         scale = "row", 
         clustering_distance_rows = "euclidean", 
         clustering_distance_cols = "euclidean",
         main = "Heatmap of Phylum Abundance Across Samples")






# 加载必要的包
library(ggplot2)
library(dplyr)

# 转换 phylum_counts_long 为相对丰度
phylum_counts_long <- phylum_counts_long %>%
  group_by(sample_name) %>%
  mutate(Relative_Abundance = Total_Abundance / sum(Total_Abundance))

# 加载必要的包
library(ggplot2)
library(dplyr)


# 安装 RColorBrewer 包
install.packages("RColorBrewer")

# 加载 RColorBrewer 包
library(RColorBrewer)

# 创建颜色方案
color_palette <- colorRampPalette(brewer.pal(9, "Set1"))(length(unique(phylum_counts_long$phylum)))

# 画堆积条形图
ggplot(phylum_counts_long, aes(x = sample_name, y = Relative_Abundance, fill = phylum)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color_palette) +
  labs(title = "Relative Abundance of Phyla in Different Samples",
       x = "Sample Name",
       y = "Relative Abundance",
       fill = "Phylum") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6), # 调整X轴文字角度和大小
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14), # 标题居中和加粗
    axis.title = element_text(face = "bold", size = 12), # 坐标轴标题加粗
    legend.position = "bottom", # 将图例放在底部
    legend.text = element_text(size = 7) # 图例文字大小
  ) +
  guides(fill = guide_legend(ncol = 15)) # 调整图例列数


library(dplyr)
library(tidyr)

# 假设第一个数据集是 phylum_counts_long，第二个数据集是 hill_numbers_with_se

# 将 phylum_counts_long 转换为宽格式
phylum_counts_wide <- phylum_counts_long %>%
  pivot_wider(names_from = phylum, values_from = Relative_Abundance, values_fill = list(Relative_Abundance = 0))

# 查看转换后的宽格式数据
head(phylum_counts_wide)

# 合并两个数据集
combined_data <- left_join(phylum_counts_wide, hill_numbers_with_se, by = "sample_name")
p_data <- left_join(phylum_counts_long, hill_numbers_with_se, by = "sample_name")

library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)

# 假设你的数据框名为 `combined_data`
# 下面的代码将计算每个样本的门的平均相对丰度，并可视化不同 sampleSizeFraction 下的门的组成

# 汇总每个样本中门的数量
combined_long <- combined_data %>%
  pivot_longer(cols = -c(sample_name, Total_Abundance, sampleSizeFractionation), names_to = "phylum", values_to = "abundance") %>%
  filter(abundance > 0)  # 移除丰度为0的行

# 计算每个 sampleSizeFraction 下每个门的平均相对丰度
phylum_fraction_summary <- combined_long %>%
  group_by(sampleSizeFraction, phylum) %>%
  summarize(mean_abundance = mean(abundance, na.rm = TRUE))

# 可视化不同 sampleSizeFraction 下的门的组成
ggplot(phylum_fraction_summary, aes(x = sampleSizeFraction, y = mean_abundance, fill = phylum)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(title = "Phylum Composition Across Different Sample Size Fractions",
       x = "Sample Size Fraction",
       y = "Mean Relative Abundance",
       fill = "Phylum")





ggplot(p_data, aes(x = sampleSizeFractionation, y = Relative_Abundance, fill = phylum)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Relative Abundance of Phyla by Sample Size Fractionation",
       x = "Sample Size Fractionation",
       y = "Relative Abundance",
       fill = "Phylum") +
  theme_minimal()





library(dplyr)
library(tidyr)
library(ggplot2)

# 假设数据框名为 combined_data，包含 country, sampleSizeFractionation, phylum 和 abundance 列

# 计算相对丰度
combined_data_relative <- combined_data %>%
  group_by(country, sampleSizeFractionation, phylum) %>%
  summarize(mean_abundance = mean(abundance, na.rm = TRUE)) %>%
  mutate(relative_abundance = mean_abundance / sum(mean_abundance))

# 绘制堆积柱状图
ggplot(p_data, aes(x = sampleSizeFractionation, y = Relative_Abundance, fill = phylum)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Relative Abundance of Phyla by Sample Size Fractionation and Country",
       x = "Sample Size Fractionation",
       y = "Relative Abundance",
       fill = "Phylum") +
  theme_minimal() +
  facet_wrap(~ country, ncol = 2)  # 根据国家分面，ncol 参数可以调整每行的图数量
