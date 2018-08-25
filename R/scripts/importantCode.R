library(ggplot2)        # plot
library(lubridate)      # easily work with dates and times
library(zoo)            # working with time series data
library(jsonlite)       # json
#___________________________________________________________________________________________________________________________

# main folder
setwd("/Users/Alf/Documents/GitHub/Bachelor/R/")
# load data
csvDataFromArduino <- read.table(file = "./csv/26_July_Steinfurterstrasse.csv", sep = ",")

#change head name of the first & second column
colnames(csvDataFromArduino) <- c("timeInMilliseconds", "signalStrength")

jsonCar <- fromJSON("./json/carTime26thJuly.json")
jsonBicycle <-fromJSON("./json/bicycleTime26thJuly.json")
jsonTruck <- fromJSON("./json/truckTime26thJuly.json")
jsonTwoVehicles <- fromJSON("./json/vehicleTime26thJuly.json")


# minus video time until adruino turns on; multiply 1000 to convert seconds to millisecond
jsonCar <- (jsonCar - 677.675543) * 1000
jsonBicycle <- (jsonBicycle - 677.675543) * 1000
jsonTruck <- (jsonTruck - 677.675543) * 1000
jsonTwoVehicles <-(jsonTwoVehicles- 677.675543) * 1000



# Remove elements larger than max of timeInMilliseconds:
jsonCar <- jsonCar[ jsonCar < max(csvDataFromArduino$timeInMilliseconds) ]
jsonBicycle <- jsonBicycle[ jsonBicycle < max(csvDataFromArduino$timeInMilliseconds) ]
jsonTruck <- jsonTruck[ jsonTruck < max(csvDataFromArduino$timeInMilliseconds) ]
jsonTwoVehicles <- jsonTwoVehicles[jsonTwoVehicles< max(csvDataFromArduino$timeInMilliseconds) ]

# Remove elements larger than max of jsonCar:
maxTimeInMilliseconds <- csvDataFromArduino$timeInMilliseconds[ csvDataFromArduino$timeInMilliseconds < max(jsonCar) ]

# Remove elements larger than max of maxTimeInMilliseconds:
csvDataFromArduino <- csvDataFromArduino[1:length(maxTimeInMilliseconds),]

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

#first measurement value - second measurement value
for (i in 1:(amplitudecsvDataFromArduinoLength)){
  nextValue <-listSignalStrength[i+1]
  AmplitudeList[i]<- c(if (listSignalStrength[i] <= nextValue) {
    0
  }else{
    (listSignalStrength[i]-listSignalStrength[i+1]) 
  })
}

#----------------------------------------------------------------------
############################################################################################################################
############################################################################################################################
resultTable <- data.frame(t1, timeT, t2, resultsT2MinusT1, AmplitudeList, amplitudecsvDataFromArduino$timeInMilliseconds[-length(amplitudecsvDataFromArduino$timeInMilliseconds)] )

#change head name
colnames(resultTable) <- c("t1", "timeT", "t2", "resultsT2MinusT1", "AmplitudeList",  "timeInMilliseconds")

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


############################################################################################################################

summary(ResultTableT1T2WidthAmplitude$resultsT2MinusT1)
boxplotResultsFunction(ResultTableT1T2WidthAmplitude$resultsT2MinusT1, "Width resultsT2MinusT1")

#cars counted with width
sum(ResultTableT1T2WidthAmplitude$resultsT2MinusT1 >= 357.5) - sum(ResultTableT1T2WidthAmplitude$resultsT2MinusT1 > 1849) 
#[1] 826
#bicycles counted with width
sum(ResultTableT1T2WidthAmplitude$resultsT2MinusT1 < 357.5)
#[1] 297
#trucks counted with width boxplot: inner fence / Max
sum(ResultTableT1T2WidthAmplitude$resultsT2MinusT1 > 1849)
#[1] 64

#**********************************************************

summary(ResultTableT1T2WidthAmplitude$AmplitudeMaxValue)
boxplotResultsFunction (ResultTableT1T2WidthAmplitude$AmplitudeMaxValue, "AmplitudeMaxValue")

#cars counted with amplitude
sum(ResultTableT1T2WidthAmplitude$AmplitudeMaxValue > 6) - sum(ResultTableT1T2WidthAmplitude$AmplitudeMaxValue > 43)
#[1] 842

#bicycles counted with amplitude
sum(ResultTableT1T2WidthAmplitude$AmplitudeMaxValue == 6)
#[1] 31

#trucks counted with amplitude boxplot: inner fence / Max
sum(ResultTableT1T2WidthAmplitude$AmplitudeMaxValue > 43)
#[1] 45
############################################################################################################################

boxplotResultsFunction<-function(boxplotResults, headName ){
  print (boxplot(boxplotResults, horizontal = TRUE, axes = FALSE, staplewex = 1))
  print(text(x=fivenum(boxplotResults), labels =fivenum(boxplotResults), y=1.25))
  print(text(x = boxplot.stats(boxplotResults)$stats, labels = boxplot.stats(boxplotResults)$stats, y = 1.25))
  print (title(paste("Steinfurter Straße", headName )))
}


