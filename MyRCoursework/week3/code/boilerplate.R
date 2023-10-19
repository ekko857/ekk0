MyFunction <- function(Arg1,Arg2) {
    print(paste("argument", as.character(Arg1), "is a", class(Arg1)))
    print(paste("Argument", as.character(Arg2), "is a", class(Arg2)))

    return (c(Arg1,Arg2))
}

MyFunction(1,2) #test the function
MyFunction("Riki","Tiki") #A different test

getwd()
setwd( "/Users/3kko/Documents/CMEECourseWork/MyRCourseWork/week3/code")
