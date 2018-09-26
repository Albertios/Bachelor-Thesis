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
aberrant <- FALSE
#two empty columns
startTimeT1 <-NA
endTimeT2 <- NA
timeT <- NA

t1 <-NA
t2 <- NA

while (i < length(csvDataFromArduino$timeInMilliseconds)-1) {
  if(  ( (csvDataFromArduino$valueDifference[i]  > 2) && (!aberrant)) ){
    startTimeT1[j] <- c(csvDataFromArduino$timeInMilliseconds[i])
    csvDataFromArduino$td[i] <- c(csvDataFromArduino$timeInMilliseconds[i])
    aberrant <- TRUE
    t1[i] <- c(csvDataFromArduino$timeInMilliseconds[i])
    timeT[i] <- paste0("= startTimeT", j  )
    j <- j +1
 
  }
  else if((csvDataFromArduino$valueDifference[i]  <= 2 && aberrant && csvDataFromArduino$valueDifference[i+1] <= 2 && csvDataFromArduino$valueDifference[i+2] <= 2 ) ){
    
    endTimeT2[k] <- c(csvDataFromArduino$timeInMilliseconds[i])
    csvDataFromArduino$td[i] <- c(csvDataFromArduino$timeInMilliseconds[i])
    aberrant <- FALSE
    t2[i] <- c(csvDataFromArduino$timeInMilliseconds[i])
    timeT[i] <- paste0("endTimeT", k, " =")
    k <- k +1
  }
  else{
  }
  i <- i +1  
  t1[i] <- 0
  t2[i] <- 0
  timeT[i] <- NA
}

results <- endTimeT2 - startTimeT1
resultsT2MinusT1 <- t2 -t1

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
        geom_vline(xintercept = jsonCar, colour="yellowgreen", linetype = 4) +
        geom_vline(xintercept = jsonBicycle, colour="black", linetype = 2) +
      #  geom_vline(xintercept = jsonTruck, colour="green", linetype = 3) +
        geom_vline(xintercept = startTimeT1, colour="red", linetype = 3) +
        geom_vline(xintercept = endTimeT2, colour="blue", linetype = 3) +
        coord_cartesian(xlim = c(182371, 183734)) +
        labs(title="Heisenbergstrasse 4/7/2018 peak width:", subtitle = "horizontal lines: startTimeT1= red line; bicycle=black line; endTimeT2= blue line; ", x = "Time in milliseconds", y = "Strength in dB"))



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
    #/2 it is not necessary
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


############################################################################################################################



counterCar <- 0

for (i in 1 : (length(startTimeT1))) {
  for (x in 1 : (length(jsonCar))) {
    if(startTimeT1[i] <= jsonCar[x] && endTimeT2[i] >= jsonCar[x] ){
      counterCar <- counterCar + 1
    }else{
    }
  }
}

############################################################################################################################


counterBicycle <- 0

for (i in 1 : (length(startTimeT1))) {
  for (x in 1 : (length(jsonBicycle))) {
    if(startTimeT1[i] <= jsonBicycle[x] && endTimeT2[i] >= jsonBicycle[x] ){
      counterBicycle <- counterBicycle + 1
    }else{
    }
  }
}

############################################################################################################################


counterTruck <- 0

for (i in 1 : (length(startTimeT1))) {
  for (x in 1 : (length(jsonTruck))) {
    if(startTimeT1[i] <= jsonTruck[x] && endTimeT2[i] >= jsonTruck[x] ){
      counterTruck <- counterTruck + 1
    }else{
    }
  }
}

############################################################################################################################
############################################################################################################################
resultTable <- data.frame(t1, timeT, t2, resultsT2MinusT1, AmplitudeList,  amplitudecsvDataFromArduino$timeInMilliseconds[-length(amplitudecsvDataFromArduino$timeInMilliseconds)] )

#change head name
colnames(resultTable) <- c("t1", "timeT", "t2", "resultsT2MinusT1", "AmplitudeList", "timeInMilliseconds")

newResultTable <-NA
newResultTable <- data.frame(t1 = numeric(), timeT = factor(), t2 = numeric(), resultsT2MinusT1 = numeric(), AmplitudeList = numeric(), filteredAmplitudeListCar= numeric(), filteredAmplitudeListBicycle= numeric(),timeInMilliseconds= integer())
ResultTableT1T2WidthAmplitude <- data.frame(t1 = numeric(),  t2 = numeric(), resultsT2MinusT1 = numeric(), AmplitudeMaxValue= integer())
q <- 1

#results only from startTime until endTime 
for (i in 2: (length(resultTable$timeInMilliseconds))) {
  if(resultTable$t1[i] > 1){
    newResultTable[q, ] <- resultTable[i, ]
    q <- q +1
    while (resultTable$t2[i] < 1) {
      i <- i + 1
      newResultTable[q, ] <- resultTable[i, ]
      q <- q +1
    }

  }else{
   
  }
}




tempList <- c()
temp <- 0
q <- 1
j <- 1
maxValue<- 0
for (i in 1: (length(newResultTable$timeInMilliseconds))) {
  if(newResultTable$t1[i] > 1){
    ResultTableT1T2WidthAmplitude[q, 1] <- newResultTable[i ,1]
    tempList[j] <- newResultTable[i, 5]
    while (newResultTable$t2[i] < 1) {
      i <- i + 1
      j <- j + 1
      tempList[j] <-  newResultTable[i, 5]
      
      if(newResultTable$t2[i] > 1){
        ResultTableT1T2WidthAmplitude[q, 2] <- newResultTable[i,3]
        
        for (x in 1 : length(tempList)) {
          if(tempList[x] >= 0 ){
            temp <- temp + tempList[x]
            if (x == length(tempList) && maxValue < temp){
              maxValue <- temp
              temp <- 0
            }
          }else if(maxValue < temp || tempList[x] < 0  ){
            if (maxValue < temp){
              maxValue <- temp
            }
            temp <- 0
          }
        }
        
        ResultTableT1T2WidthAmplitude$AmplitudeMaxValue[q] <-  maxValue
        # ResultTableT1T2WidthAmplitude$AmplitudeListAll[q] <-  toString(tempList)
        ResultTableT1T2WidthAmplitude$resultsT2MinusT1[q] <- ResultTableT1T2WidthAmplitude[q, 2] - ResultTableT1T2WidthAmplitude[q, 1]
      }
      
    }
    temp <- 0
    tempList <- c()
    maxValue <- 0
    j <- 1
    q <- q +1
  }else{
    
  }
}


############################################################################################################################

boxplotResultsFunction<-function(boxplotResults, headName ){
  print (boxplot(boxplotResults, horizontal = TRUE, axes = FALSE, staplewex = 1))
  print(text(x=fivenum(boxplotResults), labels =fivenum(boxplotResults), y=1.25))
  print(text(x = boxplot.stats(boxplotResults)$stats, labels = boxplot.stats(boxplotResults)$stats, y = 1.25))
  print (title(paste("Heisenbergstrasse", headName )))
}
############################################################################################################################


summary(ResultTableT1T2WidthAmplitude$resultsT2MinusT1)
boxplotResultsFunction(ResultTableT1T2WidthAmplitude$resultsT2MinusT1, "boxplot width results in milliseconds")

#cars counted with width
 sum(ResultTableT1T2WidthAmplitude$resultsT2MinusT1 >= 611)
#[1] 176
 
 #bicycles counted with width
 sum(ResultTableT1T2WidthAmplitude$resultsT2MinusT1 < 611)
#[1] 510
 
 #**********************************************************

 summary(ResultTableT1T2WidthAmplitude$AmplitudeMaxValue)
 boxplotResultsFunction (ResultTableT1T2WidthAmplitude$AmplitudeMaxValue, "AmplitudeMaxValue")
 
 #cars counted with amplitude
 sum(ResultTableT1T2WidthAmplitude$AmplitudeMaxValue >= 14)
 #[1] 177
 
 #bicycles counted with amplitude 
 sum(ResultTableT1T2WidthAmplitude$AmplitudeMaxValue < 14) - sum(ResultTableT1T2WidthAmplitude$AmplitudeMaxValue  < 4)
 #[1] 371
############################################################################################################################

 

 ############################################################################################################################ 
 #width method overlapping with amplitude method
 
 ResultTableT1T2WidthAmplitude$vehicleOverlapping <-NA
 counterBicycle <- 0
 counterCar <- 0
 for (i in 1:length(ResultTableT1T2WidthAmplitude$resultsT2MinusT1)) {
   if  (ResultTableT1T2WidthAmplitude$resultsT2MinusT1[i] >= 611 &&  ResultTableT1T2WidthAmplitude$AmplitudeMaxValue[i] >= 14 ){
     ResultTableT1T2WidthAmplitude$vehicleOverlapping[i] <- "car"
     counterCar <- counterCar + 1
   }
   if (ResultTableT1T2WidthAmplitude$resultsT2MinusT1[i] < 611 &&  4 < ResultTableT1T2WidthAmplitude$AmplitudeMaxValue[i] && ResultTableT1T2WidthAmplitude$AmplitudeMaxValue[i] < 14){
     ResultTableT1T2WidthAmplitude$vehicleOverlapping[i] <- "bicycle"
     counterBicycle <- counterBicycle + 1
   }
 }
 
 print(paste("cars:", counterCar))
 #[1] "cars: 104"
 print(paste("bicycles:", counterBicycle))
 #[1] "bicycles: 252"
 
 #*****************************************************************************************************************************
 
 #delete NA rows
 ResultTableT1T2WidthAmplitude<-ResultTableT1T2WidthAmplitude[complete.cases(ResultTableT1T2WidthAmplitude), ]
 
 counterCar <- 0
 
 for (i in 1 : (length(ResultTableT1T2WidthAmplitude$t1))) {
   for (x in 1 : (length(jsonCar))) {
     if(ResultTableT1T2WidthAmplitude$t1[i] <= jsonCar[x] && ResultTableT1T2WidthAmplitude$t2[i] >= jsonCar[x] && ResultTableT1T2WidthAmplitude$vehicleOverlapping[i] == "car" ){
       counterCar <- counterCar + 1
     }else{
     }
   }
 }
 print(paste("cars:", counterCar))
 
 ############################################################################################################################
 # **WIDTH Table**
 resultTableWidth <- ResultTableT1T2WidthAmplitude[ , 1:3]
 resultTableWidth$vehicle <-NA
 for (i in 1:length(resultTableWidth$resultsT2MinusT1)) {
   if  (resultTableWidth$resultsT2MinusT1[i] >= 611 ){
     resultTableWidth$vehicle[i] <- "car"
   }
   if (ResultTableT1T2WidthAmplitude$resultsT2MinusT1[i] < 611){
     resultTableWidth$vehicle[i] <- "bicycle"
   }
 }
 
 
 ############################################################################################################################
 #A = Actual number of cars overlapping with P  **WIDTH**
 
 counterCar <- 0
 
 for (i in 1 : (length(resultTableWidth$t1))) {
   for (x in 1 : (length(jsonCar))) {
     if(resultTableWidth$t1[i] <= jsonCar[x] && resultTableWidth$t2[i] >= jsonCar[x] && resultTableWidth$vehicle[i] == "car" ){
       counterCar <- counterCar + 1
     }else{
     }
   }
 }
 print(paste("cars:", counterCar))
 ############################################################################################################################
 
 
 counterBicycle <- 0
 
 for (i in 1 : (length(resultTableWidth$t1))) {
   for (x in 1 : (length(jsonBicycle))) {
     if(resultTableWidth$t1[i] <= jsonBicycle[x] && resultTableWidth$t2[i] >= jsonBicycle[x] && resultTableWidth$vehicle[i] == "bicycle" ){
       counterBicycle <- counterBicycle + 1
     }else{
     }
   }
 }
 print(paste("bicycles:", counterBicycle))
 

 
 ############################################################################################################################
 ############################################################################################################################
 # **AMPLITUDE Table**
 resultTableAmplitude <- ResultTableT1T2WidthAmplitude[ , 1:2 ]
 resultTableAmplitude$AmplitudeMaxValue <- ResultTableT1T2WidthAmplitude[ , 4 ]
 resultTableAmplitude$vehicle <-NA
 for (i in 1:length(resultTableAmplitude$AmplitudeMaxValue)) {
   if  (resultTableAmplitude$AmplitudeMaxValue[i] >= 14  ){
     resultTableAmplitude$vehicle[i] <- "car"
   }
   if (ResultTableT1T2WidthAmplitude$AmplitudeMaxValue[i] > 4 && ResultTableT1T2WidthAmplitude$AmplitudeMaxValue[i] < 14){
     resultTableAmplitude$vehicle[i] <- "bicycle"
   }
 }
 
 resultTableAmplitude <- na.omit(resultTableAmplitude)
 
 ############################################################################################################################
 #A = Actual number of cars overlapping with P  **AMPLITUDE**
 
 counterCar <- 0
 
 for (i in 1 : length(resultTableAmplitude$t1)) {
   for (x in 1 : length(jsonCar)) {
     if(resultTableAmplitude$t1[i] <= jsonCar[x] && resultTableAmplitude$t2[i] >= jsonCar[x] && resultTableAmplitude$vehicle[i] == "car" ){
       counterCar <- counterCar + 1
     }else{
     }
   }
 }
 print(paste("cars:", counterCar))
 ############################################################################################################################
 
 
 counterBicycle <- 0
 
 for (i in 1 : (length(resultTableAmplitude$t1))) {
   for (x in 1 : (length(jsonBicycle))) {
     if(resultTableAmplitude$t1[i] <= jsonBicycle[x] && resultTableAmplitude$t2[i] >= jsonBicycle[x] && resultTableAmplitude$vehicle[i] == "bicycle" ){
       counterBicycle <- counterBicycle + 1
     }else{
     }
   }
 }
 print(paste("bicycles:", counterBicycle))
 ############################################################################################################################

 
 ############################################################################################################################
 V = 467 
 P = 252 
 A = 24
 
 
 #precision <- A / P
 print(precision <- A / P )
 
 #Recall
 print(recall <- A / V ) 
 
 #F
 print(f <- 2 * ((precision * recall) / (precision + recall) ))
 
 
 ############################################################################################################################
 
 
 
 
 
 
 
 
 
 
 
 
