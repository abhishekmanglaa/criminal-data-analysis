crime.data=read_csv("/Users/abhishekmangla/Desktop/R/DataSet.csv",col_names = TRUE)
str(crime.data)
summary(crime.data)
crime.data=subset(crime.data,!duplicated(crime.data$`Case Number`))
summary(crime.data)
crime.data=subset(crime.data,!is.na(crime.data$Latitude))
crime.data=subset(crime.data,!is.na(crime.data$Ward))
head(crime.data$Date)
crime.data$Date=as.POSIXlt(crime.data$Date,format = "%m/%d/%Y %H:%M")
head(crime.data$Date)
crime.data$time=times(format(crime.data$Date,"%H:%M:%S"))
head(crime.data$time)
time.tag=chron(times=c("00:00:00", "06:00:00", "12:00:00", "18:00:00", "23:59:00")) 
time.tag
crime.data$time.tag=cut(crime.data$time,breaks= time.tag,labels=c("00-06","06-12", "12-18", "18-00"), include.lowest=TRUE)
table(crime.data$time.tag)
crime.data$Date=as.POSIXlt(strptime(crime.data$Date,format = "%Y-%m-%d"))
head(crime.data$Date)
crime.data$day=weekdays(crime.data$Date,abbreviate = TRUE)
crime.data$month=months(crime.data$Date,abbreviate = TRUE)
table(crime.data$`Primary Type`)
crime.data$crime=as.character(crime.data$`Primary Type`)
crime.data$crime <- ifelse(crime.data$crime %in% c("CRIM SEXUAL ASSAULT","PROSTITUTION", "SEX OFFENSE"), 'SEX', crime.data$crime)
crime.data$crime <- ifelse(crime.data$crime %in% c("MOTOR VEHICLE THEFT"), "MVT", crime.data$crime)
crime.data$crime <- ifelse(crime.data$crime %in% c("GAMBLING", "INTERFERE WITH PUBLIC OFFICER”, “INTERFERENCE WITH PUBLIC OFFICER", "INTIMIDATION", "LIQUOR LAW VIOLATION", "OBSCENITY", "NON-CRIMINAL", "PUBLIC PEACE VIOLATION", "PUBLIC INDECENCY", "STALKING", "NON-CRIMINAL (SUBJECT SPECIFIED)"), "NONVIO", crime.data$crime)
crime.data$crime <- ifelse(crime.data$crime == "CRIMINAL DAMAGE", "DAMAGE", crime.data$crime)                                                    
crime.data$crime <- ifelse(crime.data$crime=="CRIMINAL TRESPASS", "TRESPASS", crime.data$crime)
crime.data$crime <- ifelse(crime.data$crime %in% c("NARCOTICS", "OTHER NARCOTIC VIOLATION", "OTHER NARCOTIC VIOLATION"), "DRUG", crime.data$crime)
crime.data$crime <- ifelse(crime.data$crime =="DECEPTIVE PRACTICE", "FRAUD", crime.data$crime)
crime.data$crime <- ifelse(crime.data$crime %in% c("OTHER OFFENSE", "OTHER OFFENSE"), "OTHER", crime.data$crime)
crime.data$crime <- ifelse(crime.data$crime %in% c("KIDNAPPING", "WEAPONS VIOLATION", "OFFENSE INVOLVING CHILDREN"), "VIO", crime.data$crime)
table(crime.data$crime)
crime.data$Arrest=ifelse(as.character(crime.data$Arrest)=="Y",1,0)


crime.data$newColumn<- crime.data$Block
crime.data = transform(crime.data, FOO = colsplit(newColumn, split = " ", names = c('a', 'b')))
unique(crime.data$FOO.NA.)

commArea = read_csv("CA.csv",col_names = FALSE)
commNames = commArea$X2
commNames

crime.aggr=ddply(crime.data,.(crime,Arrest,Beat,Date,`X Coordinate`,`Y Coordinate`,time.tag,day,month),summarise,count=length(Date),.progress = `text`)
