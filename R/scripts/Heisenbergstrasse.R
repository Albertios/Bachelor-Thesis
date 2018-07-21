library(ggplot2)        # plot
library(lubridate)      # easily work with dates and times
library(zoo)            # working with time series data
library(jsonlite)       # json
#___________________________________________________________________________________________________________________________

# main folder
setwd("/Users/Alf/Documents/GitHub/Bachelor/R/")
# load data

#___________________________________________________________________________________________________________________________
# datei1test <- test123("./4_July_Heisenbergstrasse.csv", 1) 
#___________________________________________________________________________________________________________________________


#test123 <- function(CSVdata, vehicleFilter){

#dataFromHeisenbergstrasse <- read.table(file = CSVdata, sep = ",")

dataFromHeisenbergstrasse <- read.table(file = "./csv/4_July_Heisenbergstrasse.csv", sep = ",")

#change head name of the first & second column
colnames(dataFromHeisenbergstrasse) <- c("timeInMilliseconds", "signalStrength")

jsonCar <- fromJSON("./json/carTime4th7.json")

# minus video time until adruino turns on; multiply 1000 to convert seconds to millisecond
jsonCar <- (jsonCar - 589.726292) * 1000

# Remove elements larger than max of timeInMilliseconds:
jsonCar <- jsonCar[ jsonCar < max(dataFromHeisenbergstrasse$timeInMilliseconds) ]


# convert in minutes
#timeInMinutes <- format( as.POSIXct(Sys.Date())+dataFromHeisenbergstrasse$timeInMilliseconds/1000, "%M:%S")

#plot
print(ggplot(dataFromHeisenbergstrasse, aes(x=timeInMilliseconds, y=signalStrength)) + geom_line() +
        geom_vline(xintercept = jsonCar, colour="red", linetype = 3) +
        labs(title="Heisenbergstrasse 4/7/2018 WI-Fi Strengths:", x = "Time", y = "Strength")) 
#___________________________________________________________________________________________________________________________

#get data where strength is under -65
subsetDataFromHeisenbergstrasseGreaterThanMinus65 <- dataFromHeisenbergstrasse[which(dataFromHeisenbergstrasse[,2]< -65), ]

#plot
print(ggplot(subsetDataFromHeisenbergstrasseGreaterThanMinus65, aes(x=timeInMilliseconds, y=signalStrength)) + geom_line() +
        geom_vline(xintercept = jsonCar, colour="red", linetype = 3) +
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

AmplitudeList <- amplitudeDataFromHeisenbergstrasse$V3
listSignalStrength <- amplitudeDataFromHeisenbergstrasse$signalStrength

#first measurement - second measurement
for (i in 1:(amplitudeDataFromHeisenbergstrasseLength)){
  
  nextValue <-listSignalStrength[i+1]
  
  AmplitudeList[i]<- c(if (listSignalStrength[i] <= nextValue) {
    
    0
    
  }else{
    (listSignalStrength[i]-listSignalStrength[i+1]) / 2
    
  })
  
}

filteredAmplitudeList <- NA

vehicleFilter = 5

for (i in 1: (amplitudeDataFromHeisenbergstrasseLength)){
  filteredAmplitudeList[i]<- c(if (AmplitudeList[i] <= vehicleFilter) {
    0
  }else{
    AmplitudeList[i]
  })
}
return(filteredAmplitudeList)

#plot
print(ggplot(data=data.frame(x=dataFromHeisenbergstrasse$timeInMilliseconds[-length(dataFromHeisenbergstrasse$timeInMilliseconds) ] , y=filteredAmplitudeList) ,aes(x=x, y=y )) + geom_line() + 
        geom_vline(xintercept = jsonCar, colour="red", linetype = 3) +
        labs(title="amplitude:", x = "time", y = "Strength:")  )
#} # function end

#___________________________________________________________________________________________________________________________



#the value must be greater than x otherwise 0
# amplitudeFuction <- function(vehicleFilter, amplitudeDataFromHeisenbergstrasseLength) {
#   filteredAmplitudeList
#   for (i in 1: (amplitudeDataFromHeisenbergstrasseLength+1)){
#     filteredAmplitudeList[i]<- c(if (AmplitudeList[i] < vehicleFilter) {
#       0
#     }else{
#       AmplitudeList[i]
#     })
#   }
#   return(filteredAmplitudeList)
# }


#filteredSignalStrength <-   amplitudeFuction(1)



# ggplot(amplitudeDataFromHeisenbergstrasse ,aes(x=timeInMilliseconds, y=filteredSignalStrength )) + geom_line() + 
#   labs(title="amplitude:", x = "time", y = "Strength:")  


#___________________________________________________________________________________________________________________________




sum(filteredAmplitudeList > 5 )









