# This code file is designed to load the Electric power consumption dataset from the UC Irvine Machine Learning Repository
# then perform some transformations on the data and finally create an exploratory plot for a subset of the data
# Author: Adam B. Brown
# Date: 5/8/2014
# Version: 1.0

#load the R.cache package so that we can use the checksum function for object comparison
if(!"package:R.cache" %in% search()) {
        install.packages("R.cache")
        library(R.cache)
}

# look for the correct data set in memory and test the checksum to make sure it matches what we need
if(!(exists("electricPowerConsumptionSubset") && getChecksum(electricPowerConsumptionSubset) == "caf8a10fa417f9b8fec556aca5edbe6b")) {
        #either the object doesn't exist or the checksum doesn't match our reference
        
        #download the zip file from the url provided in the assignment
        sourceurl <- "http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        zipDestFile <- "./ElectricPowerConsumption.zip"
        download.file(sourceurl, zipDestFile, mode="wb")
        dateDownloaded <- date()
        
        #unzip files
        unzip(zipDestFile, overwrite = TRUE)
        
        #import full table
        electricPowerConsumptionFull <- read.table("./household_power_consumption.txt", header = TRUE, sep = ";")
        #close connection
        
        #convert date column to acctual values so we can filter easier than a character comparison
        electricPowerConsumptionFull[,1] <- as.Date(electricPowerConsumptionFull[,1], "%d/%m/%Y")
        
        #subset the data to just use data captured 2007-02-01 and 2007-02-02
        electricPowerConsumptionSubset <- subset(electricPowerConsumptionFull, Date > as.Date('2007-01-31') & Date < as.Date('2007-02-03'))
        #free the memory used to hold the full data set
        electricPowerConsumptionFull = NA
        #reformat the date column
        electricPowerConsumptionSubset <- cbind(strptime(paste(electricPowerConsumptionSubset[,1], electricPowerConsumptionSubset[,2]), format='%Y-%m-%d %H:%M:%S'),
                                                electricPowerConsumptionSubset[,3:9])
        #reset column name for datetime merged column
        names(electricPowerConsumptionSubset)[1] <- "datetime"
        
        #convert global_active_power to numeric if necessary
        if(!class(electricPowerConsumptionSubset$Global_active_power) == "numeric") {
                electricPowerConsumptionSubset$Global_active_power <- type.convert(as.character(electricPowerConsumptionSubset$Global_active_power), na.strings= c("?", "NA"), dec = ".")
        }
        if(!class(electricPowerConsumptionSubset$Voltage) == "numeric") {
                electricPowerConsumptionSubset$Voltage <- type.convert(as.character(electricPowerConsumptionSubset$Voltage), na.strings= c("?", "NA"), dec = ".")
        }
        if(!class(electricPowerConsumptionSubset$Global_reactive_power) == "numeric") {
                electricPowerConsumptionSubset$Global_reactive_power <- type.convert(as.character(electricPowerConsumptionSubset$Global_reactive_power), na.strings= c("?", "NA"), dec = ".")
        }
        if(!class(electricPowerConsumptionSubset$Sub_metering_1) == "numeric") {
                electricPowerConsumptionSubset$Sub_metering_1 <- type.convert(as.character(electricPowerConsumptionSubset$Sub_metering_1), na.strings= c("?", "NA"), dec = ".")
        }
        if(!class(electricPowerConsumptionSubset$Sub_metering_2) == "numeric") {
                electricPowerConsumptionSubset$Sub_metering_2 <- type.convert(as.character(electricPowerConsumptionSubset$Sub_metering_2), na.strings= c("?", "NA"), dec = ".")
        }
        if(!class(electricPowerConsumptionSubset$Sub_metering_3) == "numeric") {
                electricPowerConsumptionSubset$Sub_metering_3 <- type.convert(as.character(electricPowerConsumptionSubset$Sub_metering_3), na.strings= c("?", "NA"), dec = ".")
        }
}

#check that the figure folder exists in our working directory
if(!file_test("-d", paste(getwd(), "/figure/", sep = ""))) {
        #create the directory
        dir.create(paste(getwd(), "/figure/", sep = ""))
}

#create png file device with correct size and background
png(file = "./figure/plot4.png", width = 480, height = 480, units = "px", bg = "transparent")

#set multi chart layout
par(mfrow = c(2,2), mar = c(4, 4, 4, 2), oma = c(0, 0, 1, 0)) #bottom, left, top, right

#1
with(electricPowerConsumptionSubset, plot(datetime, Global_active_power, type="l", xlab = "", ylab = "Global Active Power"))
#2
with(electricPowerConsumptionSubset, plot(datetime, Voltage, type="l"))
#3
with(electricPowerConsumptionSubset, plot(datetime, Sub_metering_1, type="n", xlab = "", ylab = "Energy sub metering"))
with(electricPowerConsumptionSubset, lines(datetime, Sub_metering_1, col = "black", type="l"))
with(electricPowerConsumptionSubset, lines(datetime, Sub_metering_2, col = "red", type="l"))
with(electricPowerConsumptionSubset, lines(datetime, Sub_metering_3, col = "blue", type="l"))
legend("topright", lty = c(1,1), bty = "n", col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
#4
with(electricPowerConsumptionSubset, plot(datetime, Global_reactive_power, type="l"))

#close the connection
dev.off()