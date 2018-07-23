library(ggplot2)        # plot
library(lubridate)      # easily work with dates and times
library(zoo)            # working with time series data
library(jsonlite)       # json
#___________________________________________________________________________________________________________________________

# main folder
setwd("/Users/Alf/Documents/GitHub/Bachelor/R/")
# load data

#_________________________(CSVdata, jsonCar, jsonBicycle, jsonTruck, videoTimeWhenArduinoSwitchOn, vehicleFilter)_________________________________________________________________
# bachelor <- wifiAnalysis("./4_July_Heisenbergstrasse.csv","./json/carTime4th7July.json", "./json/bicycleTime4th7July.json","./json/truckTime4th7July.json", 589.726292, 5) 
#___________________________________________________________________________________________________________________________________________________


#test123 <- function(CSVdata, jsonCar, jsonBicycle, jsonTruck, videoTimeWhenArduinoSwitchOn, vehicleFilter){

#csvDataFromArduino <- read.table(file = CSVdata, sep = ",")

csvDataFromArduino <- read.table(file = "./csv/4_July_Heisenbergstrasse.csv", sep = ",")

#change head name of the first & second column
colnames(csvDataFromArduino) <- c("timeInMilliseconds", "signalStrength")

jsonCar <- fromJSON("./json/carTime4thJuly.json")
jsonBicycle <-fromJSON("./json/bicycleTime4thJuly.json")
jsonTruck <- fromJSON("./json/truckTime4thJuly.json")



# minus video time until adruino turns on; multiply 1000 to convert seconds to millisecond
jsonCar <- (jsonCar - 589.726292) * 1000
jsonBicycle <- (jsonBicycle - 589.726292) * 1000
jsonTruck <- (jsonTruck - 589.726292) * 1000



# Remove elements larger than max of timeInMilliseconds:
jsonCar <- jsonCar[ jsonCar < max(csvDataFromArduino$timeInMilliseconds) ]
jsonBicycle <- jsonBicycle[ jsonBicycle < max(csvDataFromArduino$timeInMilliseconds) ]
jsonTruck <- jsonTruck[ jsonTruck < max(csvDataFromArduino$timeInMilliseconds) ]


# convert in minutes
#timeInMinutes <- format( as.POSIXct(Sys.Date())+csvDataFromArduino$timeInMilliseconds/1000, "%M:%S")

#plot 
print(ggplot(csvDataFromArduino, aes(x=timeInMilliseconds, y=signalStrength)) + geom_line() + 
        labs(title="Heisenbergstrasse 4/7/2018 WI-Fi Strengths:", x = "Time", y = "Strength")) 

#plot Car
print(ggplot(csvDataFromArduino, aes(x=timeInMilliseconds, y=signalStrength)) + geom_line() +
        geom_vline(xintercept = jsonCar, colour="red", linetype = 3) +
        labs(title="Heisenbergstrasse 4/7/2018 WI-Fi Strengths:", x = "Time", y = "Strength"))

#plot Bicycle
print(ggplot(csvDataFromArduino, aes(x=timeInMilliseconds, y=signalStrength)) + geom_line() +
        geom_vline(xintercept = jsonBicycle, colour="red", linetype = 3) +
        labs(title="Heisenbergstrasse 4/7/2018 WI-Fi Strengths:", x = "Time", y = "Strength"))

#plot Truck
print(ggplot(csvDataFromArduino, aes(x=timeInMilliseconds, y=signalStrength)) + geom_line() +
        geom_vline(xintercept = jsonTruck, colour="red", linetype = 3) +
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

#----------------------------------------------------------------------

vehicleFilterCar = 5
filteredAmplitudeListCar <- NA

vehicleFilterBicycle = 2
filteredAmplitudeListBicycle <- NA

vehicleFilterTruck = 11
filteredAmplitudeListTruck <- NA

#**********************************************************************
# car

for (i in 1: (amplitudecsvDataFromArduinoLength)){
  filteredAmplitudeListCar[i]<- c(if (AmplitudeList[i] <= vehicleFilterCar || AmplitudeList[i] >= vehicleFilterTruck) {
    0
  }else{
    AmplitudeList[i]
  })
}
return(filteredAmplitudeListCar)

wifiCountingCars <- sum(filteredAmplitudeListCar > 1 )
videoCountingCars <- length(jsonCar[!is.na(jsonCar)])

accuracyCars <- 100 / videoCountingCars * wifiCountingCars 

#plot car
print(ggplot(data=data.frame(x=amplitudecsvDataFromArduino$timeInMilliseconds[-length(amplitudecsvDataFromArduino$timeInMilliseconds) ] , y=filteredAmplitudeListCar) ,aes(x=x, y=y)) +  geom_line() + 
        geom_vline(xintercept = jsonCar, colour="red", linetype = 3) +
        labs(title = ( main = paste("counted cars with wifi:", wifiCountingCars, " counted cars from the video:", videoCountingCars )),
             subtitle = ( main = paste("accuracy", (round(accuracyCars)),"%" )),
             x = "time", y = "Strength:")  )

#**********************************************************************
# bicycle

for (i in 1: (amplitudecsvDataFromArduinoLength)){
  filteredAmplitudeListBicycle[i]<- c(if (AmplitudeList[i] <= vehicleFilterBicycle || AmplitudeList[i] > vehicleFilterCar ) {
    0
  }else{
    AmplitudeList[i]
  })
}
return(filteredAmplitudeListBicycle)

wifiCountingBicycles <- sum(filteredAmplitudeListBicycle > 1 )
videoCountingBicycles <- length(jsonBicycle)

accuracyBicycles <- 100 / videoCountingBicycles * wifiCountingBicycles 

#plot bicycle
print(ggplot(data=data.frame(x=csvDataFromArduino$timeInMilliseconds[-length(csvDataFromArduino$timeInMilliseconds) ] , y=filteredAmplitudeListBicycle) ,aes(x=x, y=y)) +  geom_line() +
        geom_vline(xintercept = jsonCar, colour="red", linetype = 3) +
        labs(title = ( main = paste("counted bicycles with wifi:", wifiCountingBicycles, " counted bicycles from the video:", videoCountingBicycles )),
             subtitle = ( main = paste("accuracy", (round(accuracyBicycles)),"%" )),
             x = "time", y = "Strength:")  )


#**********************************************************************
# truck

for (i in 1: (amplitudecsvDataFromArduinoLength)){
  filteredAmplitudeListTruck[i]<- c(if (AmplitudeList[i] < vehicleFilterTruck) {
    0
  }else{
    AmplitudeList[i]
  })
}
return(filteredAmplitudeListTruck)

wifiCountingTrucks <- sum(filteredAmplitudeListTruck > 1 )
videoCountingTrucks <- length(jsonTruck[!is.na(jsonTruck)])

accuracyTrucks <- 100 / videoCountingTrucks * wifiCountingTrucks

#plot truck
print(ggplot(data=data.frame(x=csvDataFromArduino$timeInMilliseconds[-length(csvDataFromArduino$timeInMilliseconds) ] , y=filteredAmplitudeListTruck) ,aes(x=x, y=y)) +
        geom_line() + 
        geom_vline(xintercept = jsonTruck, colour="red", linetype = 3) +
        labs(title = ( main = paste("counted trucks/buses with wifi:", wifiCountingTrucks, " counted trucks/buses from the video:", videoCountingTrucks )),
             subtitle = ( main = paste("accuracy", (round(accuracyTrucks)),"%" )),
             x = "time", y = "Strength:")  )

#} # function end

############################################################################################################################
#ToDo

# time axis
# legend

############################################################################################################################



#sum(filteredAmplitudeList > 5 )









