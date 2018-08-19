library(ggplot2)        # plot
library(lubridate)      # easily work with dates and times
library(zoo)            # working with time series data
library(jsonlite)       # json
library(scales)
library(lubridate)
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
#jsonCar <- fromJSON("./json/carTime4thJuly.json")
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


# Remove elements larger than max of jsonCar:
maxTimeInMilliseconds <- csvDataFromArduino$timeInMilliseconds[ csvDataFromArduino$timeInMilliseconds < max(jsonCar) ]

# Remove elements larger than max of maxTimeInMilliseconds:
csvDataFromArduino <- csvDataFromArduino[1:length(maxTimeInMilliseconds),]



# convert in minutes !!PROBLEM: x axis can't handle it
#csvDataFromArduino$timeInMinutes <- format( as.POSIXct(Sys.Date())+csvDataFromArduino$timeInMilliseconds/1000, "%H:%M:%S")

# convert milliseconds in date
# csvDataFromArduino$timeInMinutes <- as_datetime(csvDataFromArduino$timeInMilliseconds/1000)
#change year, month and day
# year(csvDataFromArduino$timeInMinutes) <- 2018
# month(csvDataFromArduino$timeInMinutes) <- 07
# day(csvDataFromArduino$timeInMinutes) <- 04



#___________________________________________________________________________________________________________________________

#new columns: time difference and value difference
csvDataFromArduino$td <- NA
csvDataFromArduino$valueDifference <- NA

# save the difference as pasitive value
for (i in 1 : (length(csvDataFromArduino$timeInMilliseconds))) {
  csvDataFromArduino$valueDifference[i] <-  abs(csvDataFromArduino$signalStrength[i + 1] - csvDataFromArduino$signalStrength[i] )
}

#start values
i <- 1
j <- 1
k <- 1
#start boolean 
abnormal <- FALSE
#two empty columns
startTimeT1 <-NA
endTimeT2 <- NA


while (i < length(csvDataFromArduino$timeInMilliseconds)-1) {
  if(  ( (csvDataFromArduino$valueDifference[i]  > 2) && (!abnormal)) ){
    startTimeT1[j] <- c(csvDataFromArduino$timeInMilliseconds[i])
    csvDataFromArduino$td[i] <- c(csvDataFromArduino$timeInMilliseconds[i])
    abnormal <- TRUE
    j <- j +1

  }
  else if((csvDataFromArduino$valueDifference[i]  <= 2 && abnormal && csvDataFromArduino$valueDifference[i+1] <= 2 && csvDataFromArduino$valueDifference[i+2] <= 2 ) ){
    
    endTimeT2[k] <- c(csvDataFromArduino$timeInMilliseconds[i])
    csvDataFromArduino$td[i] <- c(csvDataFromArduino$timeInMilliseconds[i])
    abnormal <- FALSE
    k <- k +1
  }
  else{
  }
  i <- i +1  
}

results <- endTimeT2 - startTimeT1

boxplot(results,ylim = c(200, 1000), yaxs = "i")

boxplot(results, horizontal = TRUE, axes = FALSE, staplewex = 1)
text(x=fivenum(results), labels =fivenum(results), y=1.25)
text(x = boxplot.stats(results)$stats, labels = boxplot.stats(results)$stats, y = 1.25)
title("Heisenbergstrasse time results(t2-t1) in milliseconds")


#___________________________________________________________________________________________________________________________
#plot 
ggplot(csvDataFromArduino, aes(x=timeInMinutes, y=signalStrength)) + geom_line() +
  labs(title="Heisenbergstrasse 4/7/2018 WI-Fi Strengths:", x = "Time in hours", y = " Wi-Fi signal strength")



#plot Car
print(ggplot(csvDataFromArduino, aes(x=timeInMilliseconds, y=signalStrength)) + 
        geom_line() +
        #geom_vline(xintercept = jsonCar, colour="yellowgreen", linetype = 4) +
        #geom_vline(xintercept = jsonBicycle, colour="blue", linetype = 3) +
        #geom_vline(xintercept = jsonTruck, colour="green", linetype = 3) +
        geom_vline(xintercept = startTimeT1, colour="red", linetype = 3) +
        geom_vline(xintercept = endTimeT2, colour="blue", linetype = 3) +
        #coord_cartesian(xlim = c(00:00:09, 00:00:12)) +
        labs(title="Heisenbergstrasse 4/7/2018 WI-Fi Strengths:", subtitle = "t1(start)= red line  and  t2(end)= blue line ", x = "Time", y = "Strength"))



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
  AmplitudeList[i]<- c(if (listSignalStrength[i] == nextValue) {
    0 #AmplitudeList[i-1]
  }else{
    (listSignalStrength[i]-listSignalStrength[i+1])
    #/2 it is not ne
    #https://www.inhaltsangabe.info/s-mathematik/die-amplitude-berechnen-bestimmen-definition-formel
    #(listSignalStrength[i]-listSignalStrength[i+1]) / 2
  })
}

#----------------------------------------------------------------------

vehicleFilterCar = 10
filteredAmplitudeListCar <- NA

vehicleFilterBicycle = 4
filteredAmplitudeListBicycle <- NA

vehicleFilterTruck = 1111
filteredAmplitudeListTruck <- NA

#**********************************************************************
# car

for (i in 1: (amplitudecsvDataFromArduinoLength)){
  filteredAmplitudeListCar[i]<- c(if (AmplitudeList[i] <= vehicleFilterCar ) {
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
 print(ggplot(data=data.frame(x=amplitudecsvDataFromArduino$timeInMilliseconds[-length(amplitudecsvDataFromArduino$timeInMilliseconds) ] , y=filteredAmplitudeListCar) ,aes(x=x, y=y)) + 
        #geom_histogram(stat = "identity") + 
        #geom_boxplot() +
        #coord_cartesian(xlim = c(155000, 190000)) +
        geom_line() +
        geom_vline(xintercept = jsonCar, colour="red", linetype = 3) +
        labs(title = ( main = paste("counted cars with wifi:", wifiCountingCars, " counted cars from the video:", videoCountingCars )),
             subtitle = ( main = paste("accuracy", (round(accuracyCars)),"%" , "car=red, bicycle=blue, pedestrain=green" )),
             x = "time", y = "Strength:")  )

 

 boxplot(AmplitudeList != 11)
 plot(AmplitudeList, type = "s")

 
b <-  AmplitudeList[which(AmplitudeList>1)]
 
boxplot(b,ylim = c(0, 12), yaxs = "i")

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
        geom_vline(xintercept = jsonBicycle, colour="red", linetype = 3) +
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






twoY <- paste(csvDataFromArduino$signalStrength, filteredAmplitudeListCar)




