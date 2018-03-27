crimeData=read_csv("/Users/abhishekmangla/Desktop/R/DataSet.csv",col_names = TRUE)

crimeData <- na.omit(crimeData)
crimeData$finaldate <- as.POSIXct(crimeData$Date, format="%m/%d/%Y %I:%M:%S %p")
crimeData$date <- format(strptime(crimeData$finaldate, "%Y-%m-%d %H:%M:%S"), "%Y/%m/%d")
crimeData$time <- format(strptime(crimeData$finaldate, "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")


crimeData$finaldate <- NULL


crimeData$time = chron(times = crimeData$time)


timeTag=chron(times=c("00:00:00", "06:00:00", "12:00:00", "18:00:00", "23:59:00")) 


crimeData$timeTag=cut(crimeData$time,breaks= timeTag,labels=c("00-06","06-12", "12-18", "18-00"), include.lowest=TRUE)



crimeData$time1 <- NULL

crimeData$date <- format(strptime(crimeData$Date, "%m/%d/%Y"), "%m/%d/%Y")

crimeData$day=weekdays(crimeData$date,abbreviate = TRUE)

crimeData$month=months(crimeData$date,abbreviate = TRUE)

crimeData$PosixctDate <- as.POSIXct(crimeData$date, format="%m/%d/%Y")

crimeData$arrest=ifelse(as.character(crimeData$Arrest)=="true",1,0)

crimeData$abc <-NULL


crimeData$crime=as.character(crimeData$`Primary Type`)

crimeData$crime <- ifelse(crimeData$crime %in% c("CRIM SEXUAL ASSAULT","PROSTITUTION", "SEX OFFENSE"), 'SEXUAL OFFENSE', crimeData$crime)



crimeData$crime <- ifelse(crimeData$crime %in% c("MOTOR VEHICLE THEFT"), "MVT", crimeData$crime)


crimeData$crime <- ifelse(crimeData$crime %in% c("GAMBLING", "INTERFERE WITH PUBLIC OFFICER”, “INTERFERENCE WITH PUBLIC OFFICER", "INTIMIDATION", "LIQUOR LAW VIOLATION", "OBSCENITY", "NON-CRIMINAL", "PUBLIC PEACE VIOLATION", "PUBLIC INDECENCY", "STALKING", "NON-CRIMINAL (SUBJECT SPECIFIED)"), "NONVIO", crimeData$crime)


crimeData$crime <- ifelse(crimeData$crime == "CRIMINAL DAMAGE", "DAMAGE", crimeData$crime)

crimeData$crime <- ifelse(crimeData$crime=="CRIMINAL TRESPASS", "TRESPASS", crimeData$crime)

crimeData$crime <- ifelse(crimeData$crime %in% c("NARCOTICS", "OTHER NARCOTIC VIOLATION", "OTHER NARCOTIC VIOLATION"), "DRUG", crimeData$crime)


crimeData$crime <- ifelse(crimeData$crime =="DECEPTIVE PRACTICE", "FRAUD", crimeData$crime)

crimeData$crime <- ifelse(crimeData$crime %in% c("OTHER OFFENSE", "OTHER OFFENSE"), "OTHER", crimeData$crime)


crimeData$crime <- ifelse(crimeData$crime %in% c("KIDNAPPING", "WEAPONS VIOLATION", "OFFENSE INVOLVING CHILDREN"), "VIO", crimeData$crime)


crimeData$abc <- as.POSIXct(crimeData$Date,format = "%m/%d/%Y %I:%M:%S %p")


chota <- crime.data[seq(2,nrow(crimeData),10),]
