getwd()
setwd("/Users/3kko/Documents/CMEECourseWork/MyRCoursework/week4")
a <- read.table("data/SparrowSize.txt",header = T)

##### mean var s d.

mean(a$Bill,na.rm = T)
var(a$Bill,na.rm = T)
sd(a$Bill,na.rm = T)

mean(a$Mass,na.rm = T)
var(a$Mass,na.rm = T)
sd(a$Mass,na.rm = T)

mean(a$Wing,na.rm = T)
var(a$Wing,na.rm = T)
sd(a$Wing,na.rm = T)

par(mfrow= c(2,2))
hist(a$Mass, xlab = "body mass" ,  breaks = 300)
hist(a$Wing, xlab = "wing length", breaks = 30)
hist(a$Tarsus, xlab = "trasus", breaks = 300)
hist(a$Bill, xlab = "bill length", breaks = 300)

install.packages('dplyr')

require('dplyr')
a$Tarsus.rounded <- round(a$Tarsus, digits = 1)
head(a$Tarsus.rounded)

TarsusTally <- count(a,Tarsus.rounded, sort = T)
TarsusTally <- a %>% count(Tarsus.rounded, sort=TRUE)
TarsusTally

a2 <- subset(a, !is.na(a$Tarsus))
nrow(a) - nrow(a2)
mean(a$BirdID)
mean(a$BirdIDFact)
a$BirdIDFact <- as.factor(a$BirdID)
a$BirdIDFact
plot(a$Mass~a$Year, xlab="Year", ylab="House sparrow body mass(g)")
plot(a$Mass~as.factor(a$Year), xlab="Year", ylab="House sparrow body mass(g)" )


b<-read.table("data/BTLD.txt", header=T)
str(b)
plot(b$LD.in_AprilDays.~jitter(b$Year), ylab="Laying date (April days)", xlab ="Year", pch=19, cex=0.3)

require(ggplot2)
p <- ggplot(b, aes(x=b$Year, y=b$LD.in_AprilDays.)) + geom_violin()
p
boxplot(b$LD.in_AprilDays.~b$Year, ylab="Laying date (April days)", xlab="Year")

p <- ggplot(b, aes(x=as.factor(b$Year),y=b$LD.in_AprilDays.)) + geom_violin()
p + stat_summary(fun.data = "mean_sdl", geom = ("pointrange"))


