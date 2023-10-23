a<-c(1,2,3,4)
a
var(a)
sum(a)
prod(a+1)
length(a)
ls()
getwd()
a_squared <- a * a
is.vector(a)
a_squared
b<-4
is.vector(b)
wing.width.cm <- 1.2 #Using dot notation
wing.length.cm <- c(4.7, 5.2, 4.8)
a[1]
a[[1]]
class(b)
class(a)
li = list(c(1,2,3))
class(li)
v <- "hello"
class(v)
c <- 2L
class(2L)
c
d <- NA
d
class(d)
is.na(d)
b <- 0/0
b
class(b)
is.nan(b)
as.logical(4)
mat <- matrix(1:25,5,5,byrow=F)
mat
arr1 <- array(1:50, c(5, 5, 2))
arr1[,,1]
arr1[1,,]
arr1[,,2]
TRUE && c(TRUE,FALSE,FALSE)
years <- 1990:2009
class(years)
v <- c(0, 1, 2, 3, 4)
class(v)
set.seed(1234567)
rnorm(1)
getwd()
setwd(Users/ekko/Doucuments/CMEECourseWork/MyRCoursework/week3)
dir.create("code")
dir.create("data")    
dir.create("results")
dir()

#########################
MyData <- read.csv("data/trees.csv")

class(MyData)
head(MyData)
str(MyData)
MyData <- read.csv("data/trees.csv", header = F)
head(MyData)
MyData <- read.table("data/trees.csv", sep = ',', header = TRUE)
head(MyData)
MyData <- read.csv("data/trees.csv", skip = 5) 
head(MyData)
write.csv(MyData,"results/mydata.csv")
dir()
dir("results")
write.table(MyData[1,], file = "results/MyData.csv",append=TRUE)
write.csv(MyData, "results/MyData.csv", row.names=TRUE)


a <- TRUE
if (a == TRUE){
    print("a is true")
 }  else {
       print("a is flase")
    }
 
i <- 0
while (i < 10){
    i <- i +1
    print(i^2)
}