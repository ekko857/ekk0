rm(list = ls())

#load the data
data <- read.csv("../data/LogisticGrowthData.csv")

#colnames(data)
#head(data)
#class(data)
#any(is.na(data))
#unique(data$PopBio_units)#there are 4 diff unit:"OD_595" "N" "CFU" "DryWeight"
#unique(data$Time_units)# only one unit : hour


#check minimum value in popbio
#min(data$PopBio)# negative popbio does not make sense here
data <- data[data$PopBio >= 0, ]#new data without negative value of popio
#min(data$PopBio)

#create a ID col based on unique combos of species, medium, temp and citation
data$ID <- paste(data$Species, data$Temp, data$Medium, data$Citation, sep = ' _ ')

#create a log(popbio) col for data analysis
data$logP <- log(data$PopBio)

#any(is.na(data$logPopBio))# no na value

write.csv(data, "../data/modified_data_1.csv", row.names = F)
