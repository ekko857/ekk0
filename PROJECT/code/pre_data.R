###load packages
library(phyloseq)
library(ggplot2)
library(vegan)
###load data
setwd("/Users/3kko/Documents/PROJECT")
data <- readRDS("data/16S/ps_16S_cESVs_clean_Ransome.RDS")
#otu_table(data)
class(data)
#extract otu_table
otu <- as.matrix(otu_table(data))
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


hill_numbers <- as.data.frame(t(hill_numbers))
                              
