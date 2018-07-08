library(ggplot2) 
library(lubridate)      # easily work with dates and times
library(zoo)            # working with time series data
#___________________________________________________________________________________________________________________________

# main folder
setwd("/Users/Alf/Documents/GitHub/Bachelor/R/scripts/")
# load data

#___________________________________________________________________________________________________________________________
 datei1test <- test123("./4_July_Heisenbergstrasse.csv", 1) 
#___________________________________________________________________________________________________________________________


test123 <- function(CSVdata, vehicleFilter){

dataFromHeisenbergstrasse <- read.table(file = CSVdata, sep = ",")

#change name of the first column
colnames(dataFromHeisenbergstrasse) <- c("timeInMilliseconds", "signalStrength")

# convert in minutes
timeInMinutes <- format( as.POSIXct(Sys.Date())+dataFromHeisenbergstrasse$timeInMilliseconds/1000, "%M:%S")

#plot
print(ggplot(dataFromHeisenbergstrasse, aes(x=timeInMilliseconds, y=signalStrength)) + geom_line() +
  labs(title="Heisenbergstrasse 4/7/2018 WI-Fi Strengths:", x = "Time", y = "Strength")) 
#___________________________________________________________________________________________________________________________

#get data where strength is under -65
subsetDataFromHeisenbergstrasseGreaterThanMinus65 <- dataFromHeisenbergstrasse[which(dataFromHeisenbergstrasse[,2]< -65), ]

#plot
print(ggplot(subsetDataFromHeisenbergstrasseGreaterThanMinus65, aes(x=timeInMilliseconds, y=signalStrength)) + geom_line() +
  labs(title="Heisenbergstrasse 4/7/2018 WI-Fi Strengths:", x = "Time", y = "Strength") )

#___________________________________________________________________________________________________________________________

#new data table amplitudeDataFromHeisenbergstrasse
amplitudeDataFromHeisenbergstrasse <- dataFromHeisenbergstrasse

#length of amplitudeDataFromHeisenbergstrasse uneven or even?
if (length(amplitudeDataFromHeisenbergstrasse$signalStrength)%%2 != 0 ){
  amplitudeDataFromHeisenbergstrasseLength = length(amplitudeDataFromHeisenbergstrasse$signalStrength)-1
}else{
  amplitudeDataFromHeisenbergstrasseLength = length(amplitudeDataFromHeisenbergstrasse$signalStrength)
}

listV3 <- amplitudeDataFromHeisenbergstrasse$V3
listSignalStrength <- amplitudeDataFromHeisenbergstrasse$signalStrength

#first measurement - second measurement
for (i in 1:(amplitudeDataFromHeisenbergstrasseLength)){
  
  nextValue <-listSignalStrength[i+1]
  
  listV3[i]<- c(if (listSignalStrength[i] <= nextValue) {
    
    0
    
  }else{
    (listSignalStrength[i]-listSignalStrength[i+1]) / 2
    
  })
  
}

listV4 <- amplitudeDataFromHeisenbergstrasse$V4

for (i in 1: (amplitudeDataFromHeisenbergstrasseLength)){
  listV4[i]<- c(if (listV3[i] < vehicleFilter) {
    0
  }else{
    listV3[i]
  })
}
return(listV4)

print(ggplot(amplitudeDataFromHeisenbergstrasse ,aes(x=timeInMilliseconds, y=listV4 )) + geom_line() + 
  labs(title="amplitude:", x = "time", y = "Strength:")  )
} # function end

#___________________________________________________________________________________________________________________________



#the value must be greater than x otherwise 0
# amplitudeFuction <- function(vehicleFilter, amplitudeDataFromHeisenbergstrasseLength) {
#   listV4
#   for (i in 1: (amplitudeDataFromHeisenbergstrasseLength+1)){
#     listV4[i]<- c(if (listV3[i] < vehicleFilter) {
#       0
#     }else{
#       listV3[i]
#     })
#   }
#   return(listV4)
# }


#filteredSignalStrength <-   amplitudeFuction(1)



# ggplot(amplitudeDataFromHeisenbergstrasse ,aes(x=timeInMilliseconds, y=filteredSignalStrength )) + geom_line() + 
#   labs(title="amplitude:", x = "time", y = "Strength:")  


#___________________________________________________________________________________________________________________________




















