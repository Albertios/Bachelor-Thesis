plot(csvDataFromArduino$timeInMilliseconds)
plot(csvDataFromArduino$signalStrength)

jsonBicycle_1
      10865.47   26136.48   50939.78   73017.21   80707.71  107204.74  115610.93  126547.52  164880.39  178183.55  180564.66  183077.67
[14]  194742.04  217878.74  247911.22  251537.55  256205.65  260566.95  263086.91  265666.01  312646.33  335372.87  345724.27  349558.02  361381.28
[27]  378043.97  407288.80  428708.98  434241.41  449256.08  472280.90  476135.67  525276.89  533248.77  539873.49  547462.65  548733.77  564995.33
[40]  575441.77  599457.48  602139.50  608887.48  614740.69  620573.91  630650.27  646298.30  648945.19  649640.59  659304.81  694724.02  701622.48
[53]  702108.33  724600.34  739727.73  747984.47  765532.39  815616.54  847768.99  859812.18  869594.42  870592.83  884415.92  886364.16  886906.69
[66]  916672.40  917113.50  929110.10  944270.41  965267.42  965779.33  969059.84  971301.13  974670.81  978800.18  981419.78  987134.86 1002251.74
[79] 1007176.39 1024018.97 1041331.87 1053933.56 1054924.84 1068662.13 1070842.57 1071105.94 1072285.38 1084447.74 1086519.36 1093880.20 1096835.15


plot(csvDataFromArduino$timeInMilliseconds[1:100]/1000,csvDataFromArduino$signalStrength[1:100],type="b")
plot(csvDataFromArduino$timeInMilliseconds[2000:2500]/1000,csvDataFromArduino$signalStrength[2000:2500],type="b")
plot(csvDataFromArduino$timeInMilliseconds[80:200]/1000,csvDataFromArduino$signalStrength[80:200],type="b")
plot(csvDataFromArduino$timeInMilliseconds[300:500]/1000,csvDataFromArduino$signalStrength[300:500],type="b")
#First bike
plot(subset(csvDataFromArduino, timeInMilliseconds>10000 & timeInMilliseconds<20000))

csvDataFromArduino[csvDataFromArduino$timeInMilliseconds>10000 & csvDataFromArduino$timeInMilliseconds<12000, ]

plot(csvDataFromArduino$timeInMilliseconds[1:30]/1000,csvDataFromArduino$signalStrength[1:30],type="b")
plot(csvDataFromArduino$timeInMilliseconds[3:13],csvDataFromArduino$signalStrength[3:13],type="b")
abline(v=10865.47)
#second bike
csvDataFromArduino[csvDataFromArduino$timeInMilliseconds>25500 & csvDataFromArduino$timeInMilliseconds<27000, ]

plot(csvDataFromArduino$timeInMilliseconds[122:132]/1000,csvDataFromArduino$signalStrength[122:132],type="b")
plot(csvDataFromArduino$timeInMilliseconds[118:128],csvDataFromArduino$signalStrength[118:128],type="b")
abline(v=26136.48)

#third bike 
50939.78
csvDataFromArduino[csvDataFromArduino$timeInMilliseconds>50000 & csvDataFromArduino$timeInMilliseconds<52000, ]

plot(csvDataFromArduino$timeInMilliseconds[319:329]/1000,csvDataFromArduino$signalStrength[319:329],type="b")
plot(csvDataFromArduino$timeInMilliseconds[319:329],csvDataFromArduino$signalStrength[319:329],type="b")
abline(v=50522.78)

#Forth bike 
73017.21
csvDataFromArduino[csvDataFromArduino$timeInMilliseconds>72000 & csvDataFromArduino$timeInMilliseconds<75000, ]

plot(csvDataFromArduino$timeInMilliseconds[500:515]/1000,csvDataFromArduino$signalStrength[500:515],type="b")
plot(csvDataFromArduino$timeInMilliseconds[500:510],csvDataFromArduino$signalStrength[500:510],type="b")
abline(v=73017.21)

#Fifth bike 
80707.71
csvDataFromArduino[csvDataFromArduino$timeInMilliseconds>80000 & csvDataFromArduino$timeInMilliseconds<82000, ]

plot(csvDataFromArduino$timeInMilliseconds[569:579]/1000,csvDataFromArduino$signalStrength[569:579],type="b")
plot(csvDataFromArduino$timeInMilliseconds[568:578],csvDataFromArduino$signalStrength[568:578],type="b")
abline(v=80707.71)

#Sixth bike 
107204.74
csvDataFromArduino[csvDataFromArduino$timeInMilliseconds>105000 & csvDataFromArduino$timeInMilliseconds<110000, ]

plot(csvDataFromArduino$timeInMilliseconds[785:795]/1000,csvDataFromArduino$signalStrength[785:795],type="b")
plot(csvDataFromArduino$timeInMilliseconds[785:795],csvDataFromArduino$signalStrength[785:795],type="b")
abline(v=107204.74)

#Seventh bike 
115610.93
csvDataFromArduino[csvDataFromArduino$timeInMilliseconds>110000 & csvDataFromArduino$timeInMilliseconds<119000, ]

plot(csvDataFromArduino$timeInMilliseconds[847:857],csvDataFromArduino$signalStrength[847:857],type="b")
abline(v=115610.93)

#Eighth bike 
126547.52
csvDataFromArduino[csvDataFromArduino$timeInMilliseconds>120000 & csvDataFromArduino$timeInMilliseconds<130000, ]

plot(csvDataFromArduino$timeInMilliseconds[941:951],csvDataFromArduino$signalStrength[941:951],type="b")
abline(v=126547.52)

#Ninth bike 
164880.39
csvDataFromArduino[csvDataFromArduino$timeInMilliseconds>160000 & csvDataFromArduino$timeInMilliseconds<165000, ]

plot(csvDataFromArduino$timeInMilliseconds[1248:1258],csvDataFromArduino$signalStrength[1248:1258],type="b")
abline(v=164880.39)

#Tenth bike 
178183.55
csvDataFromArduino[csvDataFromArduino$timeInMilliseconds>175000 & csvDataFromArduino$timeInMilliseconds<180000, ]

plot(csvDataFromArduino$timeInMilliseconds[1355:1365],csvDataFromArduino$signalStrength[1355:1365],type="b")
abline(v=178183.55)


jsonCar_1$cars
[1]  141766.1  155885.3  204298.1  278100.7  281681.6  284088.9  341981.3  351289.4  367328.1  412367.5  438587.8  518047.8  556450.6  558297.9
[15]  575904.9  598437.9  611812.8  721724.3  725913.1  754743.7  774798.4  798773.0  805019.6  816410.6  892831.2  895115.1  927062.0  944541.1
[29] 1006020.4 1012415.7 1017562.5 1037468.0 1039864.9 1059835.6 1092224.1 1129686.9 1166099.6 1178203.8 1217250.8 1301446.7 1307338.2 1349442.7

#First Car
141766.1
csvDataFromArduino[csvDataFromArduino$timeInMilliseconds>140000 & csvDataFromArduino$timeInMilliseconds<145000, ]

plot(csvDataFromArduino$timeInMilliseconds[1060:1080],csvDataFromArduino$signalStrength[1060:1080],type="b")
abline(v=141766.1)

#Second Car
155885.3
csvDataFromArduino[csvDataFromArduino$timeInMilliseconds>150000 & csvDataFromArduino$timeInMilliseconds<160000, ]

plot(csvDataFromArduino$timeInMilliseconds[1174:1194],csvDataFromArduino$signalStrength[1174:1194],type="b")
abline(v=155885.3)


#Third Car
204298.1
csvDataFromArduino[csvDataFromArduino$timeInMilliseconds>202000 & csvDataFromArduino$timeInMilliseconds<209000, ]

plot(csvDataFromArduino$timeInMilliseconds[1570:1590],csvDataFromArduino$signalStrength[1570:1590],type="b")
abline(v=204298.1)


#Forth Car
278100.7
csvDataFromArduino[csvDataFromArduino$timeInMilliseconds>270000 & csvDataFromArduino$timeInMilliseconds<280000, ]

plot(csvDataFromArduino$timeInMilliseconds[2175 :2195 ],csvDataFromArduino$signalStrength[2175 :2195 ],type="b")
abline(v=278100.7)


#Fifth Car
281681.6
csvDataFromArduino[csvDataFromArduino$timeInMilliseconds>280000 & csvDataFromArduino$timeInMilliseconds<285000, ]

plot(csvDataFromArduino$timeInMilliseconds[2203 :2223 ],csvDataFromArduino$signalStrength[2203 :2223],type="b")
abline(v=281681.6)


#Sixth Car
284088.9
csvDataFromArduino[csvDataFromArduino$timeInMilliseconds>282000 & csvDataFromArduino$timeInMilliseconds<289000, ]

plot(csvDataFromArduino$timeInMilliseconds[2221:2241  ],csvDataFromArduino$signalStrength[2221  :2241 ],type="b")
abline(v=284088.9)

#Seventh Car
341981.3
csvDataFromArduino[csvDataFromArduino$timeInMilliseconds>340000 & csvDataFromArduino$timeInMilliseconds<350000, ]

plot(csvDataFromArduino$timeInMilliseconds[2695:2715  ],csvDataFromArduino$signalStrength[2695:2715],type="b")
abline(v=341981.3)

#Eighth Car
351289.4
csvDataFromArduino[csvDataFromArduino$timeInMilliseconds>350000 & csvDataFromArduino$timeInMilliseconds<355000, ]

plot(csvDataFromArduino$timeInMilliseconds[2775:2795  ],csvDataFromArduino$signalStrength[2775:2795],type="b")
abline(v=351289.4)

#Ninth Car
367328.1
csvDataFromArduino[csvDataFromArduino$timeInMilliseconds>365000 & csvDataFromArduino$timeInMilliseconds<369000, ]

plot(csvDataFromArduino$timeInMilliseconds[2902:2922  ],csvDataFromArduino$signalStrength[2902:2922],type="b")
abline(v=367328.1)


#Tenth Car
412367.5
csvDataFromArduino[csvDataFromArduino$timeInMilliseconds>410000 & csvDataFromArduino$timeInMilliseconds<414000, ]

plot(csvDataFromArduino$timeInMilliseconds[3266:3286],csvDataFromArduino$signalStrength[3266:3286],type="b")
abline(v=412367.5)


#Trucks
jsonTruck_1$truckTime

2657795 3749305 3911261 4214970 4248403 6472144 7161689

#first truck 
2657795
csvDataFromArduino[csvDataFromArduino$timeInMilliseconds>2650000 & csvDataFromArduino$timeInMilliseconds<2700000, ]

plot(csvDataFromArduino$timeInMilliseconds[21510:21550  ],csvDataFromArduino$signalStrength[21510:21550],type="b")
abline(v=2657795)

#Second truck 
3749305
csvDataFromArduino[csvDataFromArduino$timeInMilliseconds>3740000 & csvDataFromArduino$timeInMilliseconds<3790000, ]

plot(csvDataFromArduino$timeInMilliseconds[30430:30470  ],csvDataFromArduino$signalStrength[30430:30470 ],type="b")
abline(v=3749305)

#Third truck 
3911261
csvDataFromArduino[csvDataFromArduino$timeInMilliseconds>3900000 & csvDataFromArduino$timeInMilliseconds<3930000, ]

plot(csvDataFromArduino$timeInMilliseconds[31740:31780  ],csvDataFromArduino$signalStrength[31740:31780  ],type="b")
abline(v=3911261)

#Forth truck 
4214970
csvDataFromArduino[csvDataFromArduino$timeInMilliseconds>4200000 & csvDataFromArduino$timeInMilliseconds<4220000, ]

plot(csvDataFromArduino$timeInMilliseconds[34215 :34255  ],csvDataFromArduino$signalStrength[34215 :34255],type="b")
abline(v=4214970)

#Fifth truck 
4248403
csvDataFromArduino[csvDataFromArduino$timeInMilliseconds>4240000 & csvDataFromArduino$timeInMilliseconds<4260000, ]

plot(csvDataFromArduino$timeInMilliseconds[34480 :34520  ],csvDataFromArduino$signalStrength[34480 :34520],type="b")
abline(v=4248403)







Classifier.bicycle.df<-data.frame(t1=double(10),t2=double(10),t3=double(10),t4=double(10),t5=double(10),t6=double(10),t7=double(10),t8=double(10),t9=double(10),t10=double(10),t11=double(10))

Classifier.bicycle.df[1,]<-as.data.frame(t(csvDataFromArduino$signalStrength[3:13]))
Classifier.bicycle.df[2,]<-as.data.frame(t(csvDataFromArduino$signalStrength[118:128]))
Classifier.bicycle.df[3,]<-as.data.frame(t(csvDataFromArduino$signalStrength[319:329]))
Classifier.bicycle.df[4,]<-as.data.frame(t(csvDataFromArduino$signalStrength[500:510]))
Classifier.bicycle.df[5,]<-as.data.frame(t(csvDataFromArduino$signalStrength[568:578]))
Classifier.bicycle.df[6,]<-as.data.frame(t(csvDataFromArduino$signalStrength[785:795]))
Classifier.bicycle.df[7,]<-as.data.frame(t(csvDataFromArduino$signalStrength[847:857]))
Classifier.bicycle.df[8,]<-as.data.frame(t(csvDataFromArduino$signalStrength[941:951]))
Classifier.bicycle.df[9,]<-as.data.frame(t(csvDataFromArduino$signalStrength[1248:1258]))
Classifier.bicycle.df[10,]<-as.data.frame(t(csvDataFromArduino$signalStrength[1355:1365]))
Classifier.bicycle.df[11,]<-as.data.frame(t(csvDataFromArduino$signalStrength[14:24]))
Classifier.bicycle.df[12,]<-as.data.frame(t(csvDataFromArduino$signalStrength[15:25]))
Classifier.bicycle.df[13,]<-as.data.frame(t(csvDataFromArduino$signalStrength[16:26]))
Classifier.bicycle.df[14,]<-as.data.frame(t(csvDataFromArduino$signalStrength[17:27]))
Classifier.bicycle.df[15,]<-as.data.frame(t(csvDataFromArduino$signalStrength[18:28]))
Classifier.bicycle.df[16,]<-as.data.frame(t(csvDataFromArduino$signalStrength[19:29]))
Classifier.bicycle.df[17,]<-as.data.frame(t(csvDataFromArduino$signalStrength[20:30]))
Classifier.bicycle.df$label<-c("bicycle","bicycle","bicycle","bicycle","bicycle","bicycle","bicycle","bicycle","bicycle","bicycle")

Classifier.bicycle.df$label[17:35]<-c("No bicycle","No bicycle", "No bicycle", "No bicycle", "No bicycle", "No bicycle", "No bicycle", "No bicycle", "No bicycle", "No bicycle", "No bicycle", "No bicycle", "No bicycle", "No bicycle", "No bicycle", "No bicycle", "No bicycle", "No bicycle" )


write.csv2(Classifier.bicycle.df,"Bicycle_classifier_1_25.csv")


#Car classifier
Classifier.Cars.df<-data.frame(t1=double(10),t2=double(10),t3=double(10),t4=double(10),t5=double(10),t6=double(10),t7=double(10),t8=double(10),t9=double(10),t10=double(10),t11=double(10),t12=double(10),t13=double(10),t14=double(10),t15=double(10),t16=double(10),t17=double(10),t18=double(10),t19=double(10),t20=double(10),t21=double(10))

Classifier.Cars.df[1,]<-as.data.frame(t(csvDataFromArduino$signalStrength[1060:1080]))
Classifier.Cars.df[2,]<-as.data.frame(t(csvDataFromArduino$signalStrength[1174:1194]))
Classifier.Cars.df[3,]<-as.data.frame(t(csvDataFromArduino$signalStrength[1570:1590]))
Classifier.Cars.df[4,]<-as.data.frame(t(csvDataFromArduino$signalStrength[2175 :2195 ]))
Classifier.Cars.df[5,]<-as.data.frame(t(csvDataFromArduino$signalStrength[2203 :2223]))
Classifier.Cars.df[6,]<-as.data.frame(t(csvDataFromArduino$signalStrength[2221  :2241 ]))
Classifier.Cars.df[7,]<-as.data.frame(t(csvDataFromArduino$signalStrength[2695:2715]))
Classifier.Cars.df[8,]<-as.data.frame(t(csvDataFromArduino$signalStrength[2775:2795]))
Classifier.Cars.df[9,]<-as.data.frame(t(csvDataFromArduino$signalStrength[2902:2922]))
Classifier.Cars.df[10,]<-as.data.frame(t(csvDataFromArduino$signalStrength[3266:3286]))
write.csv2(Classifier.Cars.df,"Cars_classifier.csv")

#Mixing non-car values
Classifier.Cars.df.mixvalues<-Classifier.Cars.df
Classifier.Cars.df.mixvalues$label<-c("Car","Car","Car","Car","Car","Car","Car","Car","Car","Car")
Classifier.Cars.df.mixvalues







#Truck Classifier
Classifier.truck.df<-data.frame(t1=double(5),t2=double(5),t3=double(5),t4=double(5),t5=double(5),t6=double(5),t7=double(5),t8=double(5),t9=double(5),t10=double(5),t11=double(5),t12=double(5),t13=double(5),t14=double(5),t15=double(5),t16=double(5),t17=double(5),t18=double(5),t19=double(5),t20=double(5),t21=double(5),t22=double(5),t23=double(5),t24=double(5),t25=double(5),t26=double(5), t27=double(5), t28=double(5), t29=double(5), t30=double(5), t31=double(5), t32=double(5), t33=double(5), t34=double(5), t35=double(5), t36=double(5), t37=double(5), t38=double(5), t39=double(5), t40=double(5), t41=double(5))


Classifier.truck.df[1,]<- as.data.frame(t(csvDataFromArduino$signalStrength[21510:21550]))
Classifier.truck.df[2,]<- as.data.frame(t(csvDataFromArduino$signalStrength[30430:30470 ]))
Classifier.truck.df[3,]<- as.data.frame(t(csvDataFromArduino$signalStrength[31740:31780  ]))
Classifier.truck.df[4,]<- as.data.frame(t(csvDataFromArduino$signalStrength[34215 :34255]))
Classifier.truck.df[5,]<- as.data.frame(t(csvDataFromArduino$signalStrength[34480 :34520]))
write.csv2(Classifier.truck.df,"Trucks_classifier.csv")
