# Bachelor

This Bachelor Thesis presents a Wi-Fi-based solution for collecting city traffic data, using a wireless communication technology as a traffic sensing technology. The traffic data is collected with the help of a transmitter and receiver. The devices could be cost- effective solution. The Wi-Fi signal strength is measured in two test areas in M??nster. The first test area is Heisenbergstrasse, a single-lane road with little traffic. The second test area is an ending two-lane federal highway with a high traffic volume. The evaluation of the Wi-Fi data is based on amplitude-method and width-method. The amplitude- method shows the signal strength change of a passing vehicle. The width-method, on the other hand, shows the time span of the signal strength change of a passing vehicle. Both procedures give an indication of the vehicle type. Accuracy varies according to vehicle type and location. The accuracy, which it is e.g. a car, is with both procedures in the Steinfurter road about 50%. Thus, both methods are initially suitable to a limited extent for vehicle identification and traffic counting.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. 

### Prerequisites

What things you need to install the software and how to install them

```
R Studio
Browser
```

### Installing

The video evaluation index.html is ready to use. 
Just change the path of the video in the index.html code and open html site in a brwoser.

Also the R script is ready to use. As with the html page, the path of csv and JSON file have to be changed here as well.

## How the website works

The purpose of this website was to collect data as accurately as possible. For this goal
the website has a few tools like buttons, keyboard shortcuts, tables and live information about current video time in milliseconds. There were several ways to save the video time when, for example, a car passed by:
1. By pushing one of the three buttons below the video.
2. Using the keyboard shortcuts 1, 2 or 3 (see figure 3.5 red box 1).
The current video time will be immediately saved in milliseconds in the red box 2 and 3. The first one is only for the visual view and shows the newest first (see column bicycle). After every time stamp is an x button. It deletes the time from box 2 and 3. The red box 3 is just a text area where JSON document is stored. After, it can be copied out. The speed of the video can speed up, slow down or skip 5 seconds back.

## How the R script works

The raw data from Arduino and the JSON files from the video evaluation are imported into the R script. After that, the data is read, converted and synchronized into the same time series. As a consequence, the length of the data is compared and adjusted. To avoid an error message, the data from Arduino must be even. The algorithm goes through different phases to get the grand truth (see Fig.4.1). These phases are applied in the amplitude method and the width method.


### Amplitude - based method
Amplitude is a term from mathematics as well as from physics and technology to de- scribe vibrations. It can be applied to vibration variation over time. There are different approaches to calculate the amplitude, but basically one simply calculates the difference between the highest point (highest value) and the lowest point (lowest value) of a peak. A peak occurs when the Wi-Fi signal is weaken short time, which could be triggered by passing traffic.
The loop goes through each value from the signal strength list and compares the current value with the next value. If the current value is equal with the next value, then null will be save in the new list AmplitudeList. Otherwise it is subtracted from the next value and will also be saved in the AmplitudeList.
```R
#first measurement value - second measurement value
for (i in 1:(amplitudecsvDataFromArduinoLength)){
      nextValue <-listSignalStrength[i+1]
      AmplitudeList[i]<- c(if (listSignalStrength[i] == nextValue) { 50
  }else{
        (listSignalStrength[i]-listSignalStrength[i+1])
  })
}
```

### Width - Based Method

The aim of this method is to determine the width of every peak. The calculated width of a peak is the time span, i.e. how long the peak lasted. Several problems occur with this method. The first problem is that not every peak is relevant and the second problem is to determine when a peak starts and ends. The following code solves these problems. The code works with the difference between every signal strength (the current value and the previous one). In contrast to the amplitude method, this method calculates every difference. The calculated values are all set positively. To determine the starting point, two queries are required. The first query selects the noise and accepts values greater than 2. That the noise is at the value of 2 could easily be seen from the plots of the Heisenbergstrasse and Steinfurter Strasse. The second query is a logic query. The Boolean value must be FALSE for if to apply. In the if query, the Boolean variable is converted to TRUE. This guarantees that each starting point has an end point. To find the end of each peak, four conditions must be met: the Boolean value must be TRUE and three consecutive values must be less than or equal to 2. This ensures that it is actually the end of a peak.

```R
#start values
i <- 1
j <- 1
k <- 1
#start boolean aberrant <- FALSE #two empty columns startTimeT1 <-NA endTimeT2 <- NA
while (i < length(csvDataFromArduino$timeInMilliseconds)-1) {
  if(  ( (csvDataFromArduino$valueDifference[i]  > 2) && (!aberrant)) ){
    startTimeT1[j] <- c(csvDataFromArduino$timeInMilliseconds[i])
    csvDataFromArduino$td[i] <- c(csvDataFromArduino$timeInMilliseconds[i])
    aberrant <- TRUE
    j <- j +1
}
else if((csvDataFromArduino$valueDifference[i]  <= 2 && aberrant &&
    csvDataFromArduino$valueDifference[i+1] <= 2 && csvDataFromArduino$
    valueDifference[i+2] <= 2 ) ){
    endTimeT2[k] <- c(csvDataFromArduino$timeInMilliseconds[i])
    csvDataFromArduino$td[i] <- c(csvDataFromArduino$timeInMilliseconds[i])
    aberrant <- FALSE
    k <- k +1
}
else{
} i<-i+1
}
results <- endTimeT2 - startTimeT1

```


## Authors

* **Albert Hamzin** - *Initial work* - [Albertios](https://github.com/Albertios)


## License

This project is licensed under the MIT License - see the [LICENSE.md](https://github.com/Albertios/Bachelor-Thesis/blob/master/LICENSE) file for details
