#########################
MyData <- read.csv("data/trees.csv")

MyData <- read.csv("data/trees.csv", header = F)

MyData <- read.table("data/trees.csv", sep = ',', header = TRUE)

MyData <- read.csv("data/trees.csv", skip = 5) 

write.csv(MyData,"results/mydata.csv")
write.table(MyData[1,], file = "results/MyData.csv",append=TRUE)
write.csv(MyData, "results/MyData.csv", row.names=TRUE)
write.table(MyData, "results/MyData.csv", col.names=FALSE)
