#Sam Perng
#GEOG 317 Lab03 
#Spokane County



act_csv <- read.csv("data/1_InClassAct_Data.csv")
precipitation_csv <- read.csv("data/WashDC_40years.csv")

#The factors in the precipitation dataframe are the columns excluding the index

sapply(precipitation_csv,is.factor)
range(precipitation_csv$Variable2)
range(precipitation_csv$Variable3)
range(precipitation_csv$Variable4)

sapply(precipitation_csv[,-4],range,na.rm=T)
sapply(precipitation_csv[,-4],sd,na.rm=T)

summary(precipitation_csv)

#Questions
#1: Greatest mean: Variable 2
#Greatest range: Variable 2
#Greatest SD: Variable 2
#2: Variable 2, 3 missing values

precipitation_csv.sort <- 
  precipitation_csv[order(precipitation_csv$DC_Wash_40_Precipitation),]

precipitation_csv.sort[1:5,]

hist(precipitation_csv$DC_Wash_40_Precipitation)

hist(precipitation_csv$DC_Wash_40_Precipitation, scale="frequency", breaks="STURGES",
     col="darkgray", xlab="Annual precipitation (in inches)", ylab=
       "Frequency (in years)")

hist(precipitation_csv$DC_Wash_40_Precipitation, scale="frequency", breaks=4,
     col="darkgray", xlab="Annual precipitation (in inches)", ylab=
       "Frequency (in years)")

hist(precipitation_csv$DC_Wash_40_Precipitation, scale="frequency", breaks=15,
     col="darkgray", xlab="Annual precipitation (in inches)", ylab=
       "Frequency (in years)")

Rain.hist <- hist(precipitation_csv[ ,"DC_Wash_40_Precipitation"],
                  breaks="Sturges",
                  xlab="Annual precipitation (in inches)",
                  ylab="Frequency (in years)",
                  main = "Sturges Rule")
lines(Rain.hist$breaks, c(0,Rain.hist$counts),lwd=2,lty=2)
lines(Rain.hist$breaks, c(0,Rain.hist$counts),lwd=1,lty=4)

Rain.hist$counts <- cumsum(Rain.hist$counts)
plot(Rain.hist,main="Cumulative Income with Ogive",
     xlab="Annual precipitation (in inches)", ylab= "Frequency (in years)")
lines(Rain.hist$breaks, c(0,Rain.hist$counts),lwd=2,lty=2)

boxplot(precipitation_csv$DC_Wash_40_Precipitation, xlab="Annual precipitation (in inches)",
        ylab = "Frequency (in years)")

boxplot(precipitation_csv$DC_Wash_40_Precipitation,
        precipitation_csv$Variable2, names=c("DC_Wash_Precipitation", 
                                             "Variable2"))
boxplot(precipitation_csv$DC_Wash_40_Precipitation,
        precipitation_csv$Variable2, precipitation_csv$Variable3,
        names=c("DC_Wash_Precipi", "Variable2", "Variable3"))

summary(precipitation_csv)
sapply(precipitation_csv[,-4],sd,na.rm=T)

SD_Precip <- 7.411 / 39.96
SD_Precip

