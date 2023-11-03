getwd()
require(WebPower)
install.packages('WebPower')

y <- rnorm(51, mean=1 , sd=1.5)
x <- seq(from=0,to=5,by=0.1)
hist(y, breaks=10)
mean(y)
segments(x0=(mean(y)), y0=(0), x1=(mean(y)), y1=40, lty=1, col="blue")


x<-c(1,2,3,4,8) 
y<-c(4,3,5,7,9)
plot(x,y)

model_1 <- lm(y~x)

summary(model_1)

a <- read.table('week4/data/SparrowSize.txt',header = T)
a
plot(a$Mass~a$Tarsus, ylab = "mass/g",xlab = "tarsus/mm",pch = 19, cex = 0.1)


a1<-subset(a, a$Mass!="NA") 
a2<-subset(a1, a1$Tarsus!="NA") 
length(a2$Tarsus)

model1<-lm(Mass~Tarsus, data=a2)
summary(model1)
























