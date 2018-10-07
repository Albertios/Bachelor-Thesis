#Bikes 
#1
plot(csvDataFromArduino$timeInMilliseconds[3:13],csvDataFromArduino$signalStrength[3:13],type="b")
abline(v=10865.47)

#check for  the time classfication
ResultTableT1T2WidthAmplitude[ResultTableT1T2WidthAmplitude$t1 >10000 & ResultTableT1T2WidthAmplitude$t2<12000, ]

f<-1
st<-which(csvDataFromArduino$timeInMilliseconds==startTimeT1[f])
end<-which(csvDataFromArduino$timeInMilliseconds==endTimeT2[f])

plot(csvDataFromArduino$timeInMilliseconds[st:end],csvDataFromArduino$signalStrength[st:end],type="b")
csvDataFromArduino$signalStrength[st:end]
resultTable$AmplitudeList[st:end]

x<-resultTable$AmplitudeList[st:end]
sum(x[x>0])


#2

plot(csvDataFromArduino$timeInMilliseconds[3:13],csvDataFromArduino$signalStrength[3:13],type="b")
abline(v=10865.47)

#check for  the time classfication
ResultTableT1T2WidthAmplitude[ResultTableT1T2WidthAmplitude$t1 >25500 & ResultTableT1T2WidthAmplitude$t2<27000, ]

f<-8
st<-which(csvDataFromArduino$timeInMilliseconds==startTimeT1[f])
end<-which(csvDataFromArduino$timeInMilliseconds==endTimeT2[f])

plot(csvDataFromArduino$timeInMilliseconds[st:end],csvDataFromArduino$signalStrength[st:end],type="b")
csvDataFromArduino$signalStrength[st:end]
resultTable$AmplitudeList[st:end]
x<-resultTable$AmplitudeList[st:end]
sum(x[x>0])


st
end
r<-resultTable$AmplitudeList[122:127]
sum(r[r>0])
plot(csvDataFromArduino$timeInMilliseconds[122:127],csvDataFromArduino$signalStrength[122:127],type="b")

startTimeT1[3]


length(startTimeT1)


length(resultTable$AmplitudeList)


resultTable$AmplitudeList[resultTable$timeInMilliseconds==startTimeT1[5]]
which(resultTable$timeInMilliseconds==startTimeT1[10])


df.timebasedvalue_steinfuter<-data.frame(s1=numeric(1187),s2=numeric(1187),s3=numeric(1187),s4=numeric(1187),s5=numeric(1187),s6=numeric(1187))

startTimeT1_1<-startTimeT1[-1]
length(startTimeT1_1)

for (i in 1:length(startTimeT1_1)) {
  f<-which(resultTable$timeInMilliseconds==startTimeT1_1[i])
  
  df.timebasedvalue_steinfuter$s1[i]<-resultTable$AmplitudeList[f]
  df.timebasedvalue_steinfuter$s2[i]<-resultTable$AmplitudeList[f+1]
  df.timebasedvalue_steinfuter$s3[i]<-resultTable$AmplitudeList[f+2]
  df.timebasedvalue_steinfuter$s4[i]<-resultTable$AmplitudeList[f+3]
  df.timebasedvalue_steinfuter$s5[i]<-resultTable$AmplitudeList[f+4]
  df.timebasedvalue_steinfuter$s6[i]<-resultTable$AmplitudeList[f+5]
  df.timebasedvalue_steinfuter$s7[i]<-resultTable$AmplitudeList[f+6]
  df.timebasedvalue_steinfuter$s8[i]<-resultTable$AmplitudeList[f+7]
  df.timebasedvalue_steinfuter$s9[i]<-resultTable$AmplitudeList[f+8]
  df.timebasedvalue_steinfuter$s10[i]<-resultTable$AmplitudeList[f+9]
  df.timebasedvalue_steinfuter$MaxAmp[i]<-sum(df.timebasedvalue_steinfuter[i,1:10][df.timebasedvalue_steinfuter[i,1:10]>0])
  df.timebasedvalue_steinfuter$t1[i]<-resultTable$t1[f]
 i<-i+1 
}

df.timebasedvalue_steinfuter
nrow(df.timebasedvalue_steinfuter)


resultTable$timeInMilliseconds==ResultTableT1T2WidthAmplitude$t1
head(ResultTableT1T2WidthAmplitude)


df.timebasedvalue_steinfuter$T2MinusT1<-ResultTableT1T2WidthAmplitude$resultsT2MinusT1

s<-sample(10,4)
df.timebasedvalue_steinfuter[s,]
ResultTableT1T2WidthAmplitude[s,]


sample(10,4)

nrow(df.timebasedvalue_steinfuter)
nrow(ResultTableT1T2WidthAmplitude)

#remove first row to make it equal 
df.timebasedvalue_steinfuter <- df.timebasedvalue_steinfuter[-c(1), ]
nrow(df.timebasedvalue_steinfuter)
nrow(ResultTableT1T2WidthAmplitude)
#check df
head(df.timebasedvalue_steinfuter)
tail(df.timebasedvalue_steinfuter)
#check result table
head(ResultTableT1T2WidthAmplitude)
tail(ResultTableT1T2WidthAmplitude)

write.csv2(df.timebasedvalue_steinfuter,"steinfuter_df_knn_30.csv")
write.csv2(df.timebasedvalue_steinfuter,"timeandampextractedvalues_steinfuter_final.csv")


#For bikes
bicycle_vector_stein_1<-bicycle_vector_stein-1
df.timebasedvalue_steinfuter[bicycle_vector_stein_1[1],]
#Final bike vector: bicycle_vector_stein_1

#For Cars
car_vector_stein[2:15]-1
car_vector_stein_1[2:15]<-car_vector_stein[2:15]-1
df.timebasedvalue_steinfuter[car_vector_stein_1[15],]

#Final Car vector: car_vector_stein_1

#Trucks
trucks_vector_stein_1<-trucks_vector_stein-1
df.timebasedvalue_steinfuter[trucks_vector_stein_1[15],]

#Final Truck Vector : trucks_vector_stein_1


#Two cars
twovehicle_vector_stein_1<-twovehicle_vector_stein-1

df.timebasedvalue_steinfuter[twovehicle_vector_stein_1[5],]

#Final Two vehicle Vector  : twovehicle_vector_stein_1



steinfuter_training_dataset<-data.frame(s1=numeric(65),s2=numeric(65),s3=numeric(65),s4=numeric(65),s5=numeric(65),s6=numeric(65), s7=numeric(65), s8=numeric(65), s8=numeric(65), s10=numeric(65),MaxAmp=numeric(65),T2MinusT1=numeric(65))
steinfuter_training_dataset$label<-NA
#Bicycles in training data frame 
steinfuter_training_dataset[1:15,]<-df.timebasedvalue_steinfuter[bicycle_vector_stein_1,c(1:11,13)]
steinfuter_training_dataset$label[1:15]<-c("Bicycle", "Bicycle","Bicycle", "Bicycle", "Bicycle", "Bicycle", "Bicycle", "Bicycle", "Bicycle", "Bicycle", "Bicycle", "Bicycle", "Bicycle", "Bicycle", "Bicycle")

#For Cars in training data frame 
steinfuter_training_dataset[16:30,]<-df.timebasedvalue_steinfuter[car_vector_stein_1,c(1:11,13)]
steinfuter_training_dataset$label[16:30]<-c("Car", "Car","Car", "Car", "Car", "Car", "Car", "Car", "Car", "Car", "Car", "Car", "Car", "Car", "Car")

#FOR TRUCKS in training dataframe
steinfuter_training_dataset[31:45,]<-df.timebasedvalue_steinfuter[trucks_vector_stein_1,c(1:11,13)]
steinfuter_training_dataset$label[31:45]<-c("Truck", "Truck","Truck", "Truck", "Truck", "Truck", "Truck", "Truck", "Truck", "Truck", "Truck", "Truck", "Truck", "Truck", "Truck")



#For two Cars in training data frame 
steinfuter_training_dataset[46:50,]<-df.timebasedvalue_steinfuter[twovehicle_vector_stein_1,c(1:11,13)]
steinfuter_training_dataset$label[46:50]<-c("2 Car", "2 Car","2 Car", "2 Car", "2 Car")

#Noise in training data frame
Noise_vector_stein_1<-c(572, 915,763,1159,475)
steinfuter_training_dataset[51:55,]<-df.timebasedvalue_steinfuter[Noise_vector_stein_1,c(1:11,13)]
steinfuter_training_dataset$label[51:55]<-c("Noise", "Noise","Noise", "Noise", "Noise")


steinfuter_training_dataset<-steinfuter_training_dataset[-c(56:65),]


write.csv2(steinfuter_training_dataset,"Steinfuter_final_trainingdataset_30sept_morning.csv")
write.csv2(steinfuter_training_dataset,"30sept_Steinfuter_final_trainingdataset_morning.csv")



#KNN
ts1<-sample(55,15)
test_set_1_steinfuterstrasse<-steinfuter_training_dataset[ts1,]
test_set_1_steinfuterstrasse
table(test_set_1_steinfuterstrasse$label)

ts2<-sample(55,10)
test_set_2_steinfuterstrasse<-steinfuter_training_dataset[ts2,]
test_set_2_steinfuterstrasse
table(test_set_2_steinfuterstrasse$label)


#First Validation test
test_pred_vehicle_1 <- knn(train = steinfuter_training_dataset[-13], test = test_set_1_steinfuterstrasse[-13], cl = steinfuter_training_dataset$label,k=1,prob=T)

table(test_pred_vehicle_1)
table(test_set_1_steinfuterstrasse$label)

actual_vehicles<-test_set_1_steinfuterstrasse$label

mean(actual_vehicles == test_pred_vehicle_1)

#Second Validation test
test_pred_vehicle_2 <- knn(train = steinfuter_training_dataset[-13], test = test_set_2_steinfuterstrasse[-13], cl = steinfuter_training_dataset$label,k=1,prob=T)

table(test_pred_vehicle_2)
table(test_set_2_steinfuterstrasse$label)

actual_vehicles<-test_set_2_steinfuterstrasse$label
mean(actual_vehicles == test_pred_vehicle_2)

#Whole data analysis 
Steinfuter.alldata.knn<-df.timebasedvalue_steinfuter[-12]
pred_steinfuterstrasse_all <- knn(train = steinfuter_training_dataset[-13], test =Steinfuter.alldata.knn , cl = steinfuter_training_dataset$label,k=9,prob=T)
table(pred_steinfuterstrasse_all)


#NO TWO CARS
steinfuter_training_dataset_no2cars<-steinfuter_training_dataset

steinfuter_training_dataset_no2cars$label[c(46:50)]<-c("Car", "Car", "Car", "Car", "Car")

#KNN
ts1_no2cars<-sample(55,15)
test_set_1_steinfuterstrassets1_no2cars<-steinfuter_training_dataset_no2cars[ts1_no2cars,]
test_set_1_steinfuterstrassets1_no2cars
validation1_trainingset<-table(test_set_1_steinfuterstrassets1_no2cars$label)

ts2<-sample(55,10)
test_set_2_steinfuterstrassets1_no2cars<-steinfuter_training_dataset_no2cars[ts2,]
test_set_2_steinfuterstrassets1_no2cars
validation1_trainingset<-table(test_set_2_steinfuterstrassets1_no2cars$label)





#First Validation test
test_pred_vehicle_1 <- knn(train = steinfuter_training_dataset_no2cars[-13], test = test_set_1_steinfuterstrassets1_no2cars[-13], cl = steinfuter_training_dataset_no2cars$label,k=1,prob=T)

table(test_pred_vehicle_1)
table(test_set_1_steinfuterstrasse$label)

actual_vehicles<-test_set_1_steinfuterstrassets1_no2cars$label

mean(actual_vehicles == test_pred_vehicle_1)

#Second Validation test
test_pred_vehicle_2 <- knn(train = steinfuter_training_dataset_no2cars[-13], test = test_set_2_steinfuterstrassets1_no2cars[-13], cl = steinfuter_training_dataset_no2cars$label,k=1,prob=T)

table(test_pred_vehicle_2)
table(test_set_2_steinfuterstrassets1_no2cars$label)

actual_vehicles<-test_set_2_steinfuterstrassets1_no2cars$label
mean(actual_vehicles == test_pred_vehicle_2)

#Whole data analysis 
Steinfuter.alldata.knn_no2car<-Steinfuter.alldata.knn
pred_steinfuterstrasse_all_1 <- knn(train = steinfuter_training_dataset_no2cars[-13], test =Steinfuter.alldata.knn_no2car , cl = steinfuter_training_dataset_no2cars$label,k=16,prob=T)
table(pred_steinfuterstrasse_all_1)




#Bicycle corrections in training dataset

steinfuter_training_dataset_no2cars
nrow(steinfuter_training_dataset_no2cars)
#Value above maxAMP 15 are given to cars from bicycles
steinfuter_training_dataset_no2cars.bikecorrection<-steinfuter_training_dataset_no2cars
steinfuter_training_dataset_no2cars.bikecorrection$label[c(1,2,3,10,13,14)]<-c("Car","Car","Car","Car", "Car", "Car")
steinfuter_training_dataset_no2cars.bikecorrection


#KNN
ts1_no2cars<-sample(55,15)
test_set_1_steinfuterstrassets1_no2cars<-steinfuter_training_dataset_no2cars.bikecorrection[ts1_no2cars,]
test_set_1_steinfuterstrassets1_no2cars
validation1_trainingset<-table(test_set_1_steinfuterstrassets1_no2cars$label)

ts2<-sample(55,10)
test_set_2_steinfuterstrassets1_no2cars<-steinfuter_training_dataset_no2cars.bikecorrection[ts2,]
test_set_2_steinfuterstrassets1_no2cars
validation1_trainingset<-table(test_set_2_steinfuterstrassets1_no2cars$label)

#First Validation test
test_pred_vehicle_1 <- knn(train = steinfuter_training_dataset_no2cars.bikecorrection[-13], test = test_set_1_steinfuterstrassets1_no2cars[-13], cl = steinfuter_training_dataset_no2cars.bikecorrection$label,k=1,prob=T)

table(test_pred_vehicle_1)
table(test_set_1_steinfuterstrassets1_no2cars$label)

actual_vehicles<-test_set_1_steinfuterstrassets1_no2cars$label

mean(actual_vehicles == test_pred_vehicle_1)

#Second Validation test
test_pred_vehicle_2 <- knn(train = steinfuter_training_dataset_no2cars.bikecorrection[-13], test = test_set_2_steinfuterstrassets1_no2cars[-13], cl = steinfuter_training_dataset_no2cars.bikecorrection$label,k=1,prob=T)

table(test_pred_vehicle_2)
table(test_set_2_steinfuterstrassets1_no2cars$label)

actual_vehicles<-test_set_2_steinfuterstrassets1_no2cars$label
mean(actual_vehicles == test_pred_vehicle_2)

#Whole data analysis 
Steinfuter.alldata.knn_no2car<-Steinfuter.alldata.knn
pred_steinfuterstrasse_all_1 <- knn(train = steinfuter_training_dataset_no2cars.bikecorrection[-13], test =Steinfuter.alldata.knn_no2car , cl = steinfuter_training_dataset_no2cars$label,k=20,prob=T)
table(pred_steinfuterstrasse_all_1)

#remove signal number :- only 2 parameters

Steinfuter.alldata.knn_no2car2_val<-Steinfuter.alldata.knn_no2car[11:12]

steinfuter_training_dataset_no2cars.bikecorrection_2_val<-steinfuter_training_dataset_no2cars.bikecorrection[11:13]



test_set_1_steinfuterstrassets1_no2cars_2val<-test_set_1_steinfuterstrassets1_no2cars[11:13]

#First Validation test
test_pred_vehicle_1_2val <- knn(train = steinfuter_training_dataset_no2cars.bikecorrection_2_val[-3], test = test_set_1_steinfuterstrassets1_no2cars_2val[-3], cl = steinfuter_training_dataset_no2cars.bikecorrection_2_val$label,k=1,prob=T)

table(test_pred_vehicle_1_2val)
table(test_set_1_steinfuterstrassets1_no2cars_2val$label)

actual_vehicles<-test_set_1_steinfuterstrassets1_no2cars_2val$label

mean(actual_vehicles == test_pred_vehicle_1_2val)


#Second Validation test
test_set_2_steinfuterstrassets1_no2cars_2val<-test_set_2_steinfuterstrassets1_no2cars[11:13]
test_pred_vehicle_2_2val <- knn(train = steinfuter_training_dataset_no2cars.bikecorrection_2_val[-3], test = test_set_2_steinfuterstrassets1_no2cars_2val[-3], cl = steinfuter_training_dataset_no2cars.bikecorrection_2_val$label,k=1,prob=T)

table(test_pred_vehicle_2_2val)
table(test_set_2_steinfuterstrassets1_no2cars_2val$label)

actual_vehicles<-test_set_2_steinfuterstrassets1_no2cars_2val$label
mean(actual_vehicles == test_pred_vehicle_2_2val)

#Whole data analysis 
#Previous K value

pred_steinfuterstrasse_all_1 <- knn(train = steinfuter_training_dataset_no2cars.bikecorrection_2_val[-3], test =Steinfuter.alldata.knn_no2car2_val , cl = steinfuter_training_dataset_no2cars.bikecorrection_2_val$label,k=1,prob=T)
table(pred_steinfuterstrasse_all_1)

#Perfect match
Steinfuter.alldata.knn_no2car<-Steinfuter.alldata.knn
pred_steinfuterstrasse_all_1 <- knn(train = steinfuter_training_dataset_no2cars.bikecorrection_2_val[-3], test =Steinfuter.alldata.knn_no2car2_val , cl = steinfuter_training_dataset_no2cars.bikecorrection_2_val$label,k=1,prob=T)
table(pred_steinfuterstrasse_all_1)


pred_steinfuterstrasse_all_1.try <- knn(train = steinfuter_training_dataset_no2cars.bikecorrection_2_val[-3], test =Steinfuter.alldata.knn_no2car2_val , cl = steinfuter_training_dataset_no2cars.bikecorrection_2_val$label,k=11,prob=T)
table(pred_steinfuterstrasse_all_1.try)

Steinfuter_table_final<-table(pred_steinfuterstrasse_all_1)

#precision
42/45

#recall



#f value
pred_steinfuterstrasse_all_1


df.timebasedvalue_steinfuter_predict_resultincluded<-df.timebasedvalue_steinfuter

length(pred_steinfuterstrasse_all_1)

nrow(ResultTableT1T2WidthAmplitude)

predictionresults.steinfuter<-as.data.frame(pred_steinfuterstrasse_all_1)

nrow(predictionresults.steinfuter)


resultsknn.resulttable<-data.frame(ResultTableT1T2WidthAmplitude,predictionresults.steinfuter,stringsAsFactors = F)
names(resultsknn.resulttable$pred_steinfuterstrasse_all_1)<-"kNN result"

class(resultsknn.resulttable$pred_steinfuterstrasse_all_1)
resultsknn.resulttable

############################################################################################################################
#A = Actual number of cars overlapping with P  **WIDTH**

counterCar <- 0

for (i in 1 : (length(resultsknn.resulttable$t1))) {
  for (x in 1 : (length(jsonCar))) {
    if(resultsknn.resulttable$t1[i] <= jsonCar[x] && resultsknn.resulttable$t2[i] >= jsonCar[x] && resultsknn.resulttable$pred_steinfuterstrasse_all_1[i] == "Car" ){
      counterCar <- counterCar + 1
    }else{
    }
  }
}
print(paste("cars:", counterCar))



resultsknn.resulttable$vehicleOverlapping

counterCar <- 0

for (i in 1 : (length(resultsknn.resulttable$t1))) {
  for (x in 1 : (length(jsonCar))) {
    if(resultsknn.resulttable$t1[i] <= jsonCar[x] && resultsknn.resulttable$t2[i] >= jsonCar[x] && resultsknn.resulttable$pred_steinfuterstrasse_all_1[i] == "Car" ){
      counterCar <- counterCar + 1
    }else{
    }
  }
}
print(paste("cars:", counterCar))
############################################################################################################################
#bicycle


counterBicycle <- 0

for (i in 1 : (length(resultsknn.resulttable$t1))) {
  for (x in 1 : (length(jsonBicycle))) {
    if(resultsknn.resulttable$t1[i] <= jsonBicycle[x] && resultsknn.resulttable$t2[i] >= jsonBicycle[x] ){
      counterBicycle <- counterBicycle + 1
    }else{
    }
  }
}
print(paste("Bicycles:", counterBicycle))

#31


counterBicycle <- 0

for (i in 1 : (length(resultsknn.resulttable$t1))) {
  for (x in 1 : (length(jsonBicycle))) {
    if(resultsknn.resulttable$t1[i] <= jsonBicycle[x] && resultsknn.resulttable$t2[i] >= jsonBicycle[x] && resultsknn.resulttable$pred_steinfuterstrasse_all_1[i] == "Bicycle" ){
      counterBicycle <- counterBicycle + 1
    }else{
    }
  }
}
print(paste("Bicycles:", counterBicycle))



#Trucks


counterTrucks <- 0

for (i in 1 : (length(resultsknn.resulttable$t1))) {
  for (x in 1 : (length(jsonTruck))) {
    if(resultsknn.resulttable$t1[i] <= jsonTruck[x] && resultsknn.resulttable$t2[i] >= jsonTruck[x] ){
      counterTrucks <- counterTrucks + 1
    }else{
    }
  }
}
print(paste("Trucks:", counterTrucks))

#31


counterTrucks <- 0

for (i in 1 : (length(resultsknn.resulttable$t1))) {
  for (x in 1 : (length(jsonTruck))) {
    if(resultsknn.resulttable$t1[i] <= jsonTruck[x] && resultsknn.resulttable$t2[i] >= jsonTruck[x]  && resultsknn.resulttable$pred_steinfuterstrasse_all_1[i] == "Truck" ){
      counterTrucks <- counterTrucks + 1
    }else{
    }
  }
}
print(paste("Trucks:", counterTrucks))















#With only max value and time values 

#First Validation test
steinfuter_training_dataset_2val<-steinfuter_training_dataset[11:13]
test_set_1_steinfuterstrasse_2val<-test_set_1_steinfuterstrasse[11:13]



test_pred_vehicle_1 <- knn(train = steinfuter_training_dataset_2val[-3], test = test_set_1_steinfuterstrasse_2val[-3], cl = steinfuter_training_dataset_2val$label,k=1,prob=T)

table(test_pred_vehicle_1)
table(test_set_1_steinfuterstrasse$label)

pred_steinfuterstrasse_all <- knn(train = steinfuter_training_dataset_2val_lessbicycle[-3], test =Steinfuter.alldata.knn_2val , cl = steinfuter_training_dataset_2val_lessbicycle$label,k=16,prob=T)


#Second Validation test
test_set_2_steinfuterstrasse_2val<-test_set_2_steinfuterstrasse[11:13]

test_pred_vehicle_2 <- knn(train = steinfuter_training_dataset_2val[-3], test = test_set_2_steinfuterstrasse_2val[-3], cl = steinfuter_training_dataset_2val$label,k=1,prob=T)

table(test_pred_vehicle_2)
table(test_set_2_steinfuterstrasse$label)

actual_vehicles<-test_set_2_steinfuterstrasse$label
mean(actual_vehicles == test_pred_vehicle_2)

#Whole data analysis 
Steinfuter.alldata.knn<-df.timebasedvalue_steinfuter[-12]
Steinfuter.alldata.knn_2val<-df.timebasedvalue_steinfuter[-c(1:10,12)]





#chaniging the TRAINIGN DATA SET FOR CARS
steinfuter_training_dataset_2val_lessbicycle<-steinfuter_training_dataset_2val
steinfuter_training_dataset_2val_lessbicycle$label[c(1:3,10,12,13,14)]<-c("Car", "Car", "Car", "Car", "Car", "Car", "Car")
steinfuter_training_dataset_2val_lessbicycle$label[c(46:50)]<-c("Car", "Car", "Car", "Car", "Car")

#Perfect steinfuterstrasse
pred_steinfuterstrasse_all <- knn(train = steinfuter_training_dataset_2val_lessbicycle[-3], test =Steinfuter.alldata.knn_2val , cl = steinfuter_training_dataset_2val_lessbicycle$label,k=16,prob=T)
table(pred_steinfuterstrasse_all)





#Using heisenberg training set

Heisenber_training_dataset_2val <-Heisenber_training_dataset[11:13]
pred_steinfuterstrasse_all_heisentrainingset <- knn(train = Heisenber_training_dataset_2val[-3], test =Steinfuter.alldata.knn_2val , cl = Heisenber_training_dataset_2val$label,k=17,prob=T)
table(pred_steinfuterstrasse_all_heisentrainingset)










