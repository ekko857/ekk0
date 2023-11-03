rm(list = ls())
#####loading data
MyMetaData <- read.csv("../data/PoundHillMetaData.csv",header = TRUE,  sep=";")
class(MyMetaData)

MyData <- as.matrix(read.csv("../data/PoundHillData.csv",header = FALSE))
class(MyData)

#####change null to zero
MyData[MyData == ""] <- 0

#####transposing the data
MyData <- t(MyData)

#####change to data.frame and rename the col_names and row_names
Tempdata <- as.data.frame(MyData[-1, ], stringsAsFactors = F)
colnames(Tempdata) <- MyData[1, ]
rownames(Tempdata) <- NULL

#####use gather to reshape the data to long format
MyWrangledData <- gather(Tempdata, key = "Species", value = "Count",  -Cultivation,
                         -Block, -Plot, -Quadrat)


######Converting Data Types
MyWrangledData <- mutate(MyWrangledData,
  Cultivation = as.factor(Cultivation),
  Block = as.factor(Block),
  Plot = as.factor(Plot),
  Quadrat = as.factor(Quadrat),
  Count = as.integer(Count))
