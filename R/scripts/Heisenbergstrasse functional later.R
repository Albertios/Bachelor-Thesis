library(ggplot2)        # plot
library(lubridate)      # easily work with dates and times
library(zoo)            # working with time series data
library(jsonlite)       # json
#___________________________________________________________________________________________________________________________

# main folder
setwd("/Users/Alf/Documents/GitHub/Bachelor/R/")
# load data

#_________________________(CSVdata, jsonCar, jsonBicycle, jsonTruck, videoTimeWhenArduinoSwitchOn, vehicleFilter)_________________________________________________________________
# bachelor <- wifiAnalysis("./4_July_Heisenbergstrasse.csv","./json/carTime4thJuly.json", "./json/bicycleTime4thJuly.json","./json/truckTime4thJuly.json", 589.726292, 5) 
#___________________________________________________________________________________________________________________________________________________


#test123 <- function(CSVdata, jsonCar, jsonBicycle, jsonTruck, videoTimeWhenArduinoSwitchOn, vehicleFilter){

#csvDataFromArduino <- read.table(file = CSVdata, sep = ",")

csvDataFromArduino <- read.table(file = "./csv/4_July_Heisenbergstrasse.csv", sep = ",")

#change head name of the first & second column
colnames(csvDataFromArduino) <- c("timeInMilliseconds", "signalStrength")

jsonCar <- fromJSON("./json/carTime4thJuly.json")
jsonBicycle <-fromJSON("./json/bicycleTime4thJuly.json")
jsonTruck <- fromJSON("./json/truckTime4thJuly.json")

json <- c(jsonCar, jsonBicycle, jsonTruck)

# minus video time until adruino turns on; multiply 1000 to convert seconds to millisecond
# jsonCar <- (jsonCar - 589.726292) * 1000
# jsonBicycle <- (jsonBicycle - 589.726292) * 1000
# jsonTruck <- (jsonTruck - 589.726292) * 1000

for( i in length(json[i])){ 

json[[i]] <- (json[[i]] - 589.726292) * 1000

}

# Remove elements larger than max of timeInMilliseconds:
jsonCar <- jsonCar[ jsonCar < max(csvDataFromArduino$timeInMilliseconds) ]


# convert in minutes
#timeInMinutes <- format( as.POSIXct(Sys.Date())+csvDataFromArduino$timeInMilliseconds/1000, "%M:%S")

#plot
print(ggplot(csvDataFromArduino, aes(x=timeInMilliseconds, y=signalStrength)) + geom_line() +
       # geom_vline(xintercept = jsonCar, colour="red", linetype = 3) +
        labs(title="Heisenbergstrasse 4/7/2018 WI-Fi Strengths:", x = "Time", y = "Strength")) 
#___________________________________________________________________________________________________________________________

#get data where strength is under -65
subsetcsvDataFromArduinoGreaterThanMinus65 <- csvDataFromArduino[which(csvDataFromArduino[,2]< -65), ]

#plot
print(ggplot(subsetcsvDataFromArduinoGreaterThanMinus65, aes(x=timeInMilliseconds, y=signalStrength)) + geom_line() +
        geom_vline(xintercept = jsonCar, colour="red", linetype = 3) +
        labs(title="Heisenbergstrasse 4/7/2018 WI-Fi Strengths:", x = "Time", y = "Strength") )

#___________________________________________________________________________________________________________________________

#new data table amplitudecsvDataFromArduino
amplitudecsvDataFromArduino <- csvDataFromArduino

#length of amplitudecsvDataFromArduino uneven or even?
if (length(amplitudecsvDataFromArduino$signalStrength)%%2 != 0 ){
  amplitudecsvDataFromArduinoLength = length(amplitudecsvDataFromArduino$signalStrength)-1
}else{
  amplitudecsvDataFromArduinoLength = length(amplitudecsvDataFromArduino$signalStrength)
}

AmplitudeList <- amplitudecsvDataFromArduino$V3
listSignalStrength <- amplitudecsvDataFromArduino$signalStrength

#first measurement - second measurement
for (i in 1:(amplitudecsvDataFromArduinoLength)){
  
  nextValue <-listSignalStrength[i+1]
  
  AmplitudeList[i]<- c(if (listSignalStrength[i] <= nextValue) {
    
    0
    
  }else{
    (listSignalStrength[i]-listSignalStrength[i+1]) / 2
    
  })
  
}

filteredAmplitudeList <- NA

vehicleFilter <= 5

for (i in 1: (amplitudecsvDataFromArduinoLength)){
  filteredAmplitudeList[i]<- c(if (AmplitudeList[i] <= vehicleFilter) {
    0
  }else{
    AmplitudeList[i]
  })
}
return(filteredAmplitudeList)

#plot
print(ggplot(data=data.frame(x=csvDataFromArduino$timeInMilliseconds[-length(csvDataFromArduino$timeInMilliseconds) ] , y=filteredAmplitudeList) ,aes(x=x, y=y)) + 
        geom_line(aes(colour=variable)) + 
        geom_vline(xintercept = jsonCar, colour="red", linetype = 3) +
        labs(title="amplitude cars:", x = "time", y = "Strength:")  )

#} # function end

#___________________________________________________________________________________________________________________________



#the value must be greater than x otherwise 0
# amplitudeFuction <- function(vehicleFilter, amplitudecsvDataFromArduinoLength) {
#   filteredAmplitudeList
#   for (i in 1: (amplitudecsvDataFromArduinoLength+1)){
#     filteredAmplitudeList[i]<- c(if (AmplitudeList[i] < vehicleFilter) {
#       0
#     }else{
#       AmplitudeList[i]
#     })
#   }
#   return(filteredAmplitudeList)
# }


#filteredSignalStrength <-   amplitudeFuction(1)



# ggplot(amplitudecsvDataFromArduino ,aes(x=timeInMilliseconds, y=filteredSignalStrength )) + geom_line() + 
#   labs(title="amplitude:", x = "time", y = "Strength:")  


#___________________________________________________________________________________________________________________________




sum(filteredAmplitudeList > 5 )









