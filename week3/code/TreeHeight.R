# This function calculates heights of trees given distance of each tree 
# from its base and angle to its top, using  the trigonometric formula 
#
# height = distance * tan(radians)
#
# ARGUMENTS
# degrees:   The angle of elevation of tree
# distance:  The distance from base of tree (e.g., meters)
#
# OUTPUT
# The heights of the tree, same units as "distance"
rm(list=ls())
getwd()
setwd("/Users/3kko/Documents/CMEECourseWork")
####read tress.csv
a <- read.csv("week3/data/trees.csv")

#### set the function to calculate tree height
TreeHeight <- function(degrees, distance) {
    radians <- degrees * pi / 180
    height <- distance * tan(radians)
    cat(paste("Tree height is:", height))
  
    return (height)
}
####creating trees_height
a$Trees_Height.m <- TreeHeight(a$Angle.degrees,a$Distance.m)

####save as csv
write.csv(a, "week3/results/TreeHts.csv", row.names = F)

