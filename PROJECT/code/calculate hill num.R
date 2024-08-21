#加载数据包
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
library(foreach)
library(parallel)
library(doParallel)

#加载数据
setwd("/Users/3kko/Documents/CMEECourseWork/PROJECT")
data <- readRDS("data/16S/ps_16S_cESVs_clean_Ransome.RDS")

####计算hill number
otu <- as.matrix(otu_table(data))
otu <- as.data.frame(otu)
head(otu)
class(otu)

##calculate total abundance
total_counts <- colSums(otu)
#total_counts

##calculate Relative abundance
rel_abundance <- sweep(otu, 2, total_counts, "/")
#head(rel_abundance)

#define a function for calculation of Shannon entropy
shannon_entropy <- function(p) {
  p <- p[p > 0]      # only do calculation when p > 0
  -sum(p * log(p))   # formular
}

#define a function for calculation of Hill number
hill_number <- function(p, q) {
  if (q == 1) {
    return(exp(shannon_entropy(p)))
  } else if (q == 0) {
    return(sum(p > 0))  # ignore 0 (means no such species exists)
  } else {
    return((sum(p^q))^(1/(1-q)))
  }
}

# calculate hill number for each samples
hill_numbers <- apply(rel_abundance, 2, function(p) {
  c(N0 = hill_number(p, 0),  # species richness (q = 0)
    N1 = hill_number(p, 1),  # The exponential form of Shannon entropy (q = 1)
    N2 = hill_number(p, 2))  # The reciprocal of Simpson's diversity (q = 2)
})
#save data as csv
hill_numbers <- as.data.frame(t(hill_numbers))
hill_numbers$sample_name <- rownames(hill_numbers)
###################################################################################


# this function calculates the Shannon entropy of a given set of frequencies
shannon_entropy <- function(frequency) {
  proportions <- frequency/sum(frequency)
  entropy <- 0
  for (pi in proportions) {
    if (pi > 0) {
      entropy <- entropy - (pi * log(pi))
    }
  }
  return(entropy) 
}

# this function produces a discretized log normal distribution in arithmetic space
discretised_lnorm <- function(mean,sd) {
  # the max variable gives us the maximum value we'll consider
  max <- floor(exp(mean + sd*4))+1
  # the missed_zero approximates what we've missed as it lies on or below zero abundance
  missed_zero <- plnorm(0.5,mean,sd)
  # store the result
  lnorm_result <- dlnorm(1:max,mean,sd)
  # normalize
  lnorm_result <- lnorm_result/(sum(lnorm_result)+missed_zero)
  return(lnorm_result)
}

# this function sums to vectors of different lengths
sum_vect <- function(x,y) {
  if (length(x)==length(y)) {
    return(x+y)
  } else {
    if (length(x)<length(y)) {
      return(c(x,rep(0,length= (length(y)-length(x))))+y)
    } else {
      return(c(y,rep(0,length= (length(x)-length(y))))+x)
    }
  }
}

# this function computes Bibury diversity
bibury <- function(frequency,param) {
  abundance_kernel <- c()
  for (i in frequency) {
    abundance_kernel <- sum_vect(abundance_kernel,discretised_lnorm(log(i),param))
  }
  bibury_diversity <- shannon_entropy(frequency) + shannon_entropy(abundance_kernel)
  return(bibury_diversity)
}

# 5. 计算每个样本的 Bibury 多样性指数
# 设置标准差参数
param <- 0.5


# 6. 设置并行计算环境
numCores <- detectCores() - 1
cl <- makeCluster(numCores)
registerDoParallel(cl)

# 导出必要的函数和变量到集群
clusterExport(cl, c("shannon_entropy", "discretised_lnorm", "bibury", "sum_vect", "param"))

#并行计算 Bibury 多样性指数
# 9. 并行计算 Bibury 多样性指数
# 9. 并行计算 Bibury 多样性指数
bibury_diversities <- foreach(i = 1:ncol(otu), .combine = c, .packages = c("base", "parallel")) %dopar% {
  frequency <- as.numeric(otu[, i])
  frequency <- frequency[frequency > 0]  # 筛选掉零值
  if (length(frequency) > 0) {
    return(bibury(frequency, param))
  } else {
    return(NA)  # 如果所有值都为零，返回 NA
  }
}
# 停止并行计算集群
stopCluster(cl)

# 将结果添加到数据框中
diversity_df <- data.frame(
  Sample = colnames(otu),
  Bibury_Diversity = bibury_diversities
)

write.csv(diversity_df, 'data/bibury.csv')
# 打印结果
print(diversity_df)

hill_numbers <- left_join(hill_numbers, diversity_df, by = 'sample_name')
hill_numbers <- hill_numbers %>% select(-Sample)
write.csv(hill_numbers, 'data/hill_numbers.csv')
