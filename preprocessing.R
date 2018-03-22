crimeData=read_csv("/Users/abhishekmangla/Desktop/R/DataSet.csv",col_names = TRUE)

crimeData <- na.omit(crimeData)
crimeData$finaldate <- as.POSIXct(crimeData$Date, format="%m/%d/%Y %I:%M:%S %p")
crimeData$date <- format(strptime(crimeData$finaldate, "%Y-%m-%d %H:%M:%S"), "%Y/%m/%d")
crimeData$time <- format(strptime(crimeData$finaldate, "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")


crimeData$finaldate <- NULL


timeTag=chron(times=c("00:00:00", "06:00:00", "12:00:00", "18:00:00", "23:59:00"))
timeTag

crimeData$timeTag =cut(crimeData$time, breaks= timeTag, labels=c("00-06","06-12", "12-18", "18-00"), include.lowest=TRUE)
