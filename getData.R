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
commArea <- commArea[order(commArea$X1),]
commNames = commArea$X2


crime.data$FOO.NA..5 <- NULL
crime.data$FOO.NA..4 <- NULL
crime.data$FOO.NA..3 <- NULL
crime.data$FOO.NA..2 <- NULL
crime.data$FOO.NA..1 <- NULL
crime.data$FOO.NA. <- NULL
crime.data$FOO.b <- NULL
crime.data$FOO.a <- NULL
crime.data$newColumn <- NULL


crime.simple.data <- crime.data[c(4,6:8,19,24,26,27)]

qplot(crime.data$crime,xlab = "Crime",main = "Crimes in Chicago")+scale_y_continuous("Number of Crimes")

qplot(crime.data$time.tag, xlab="Time of day", main="Crimes by time of day") + scale_y_continuous("Number of crimes")

crime.data$day <- factor(crime.data$day, levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
qplot(crime.data$day,xlab = "Day of Week",main="Crimes by day of Week")+scale_y_continuous("Number of crimes")

crime.data$month <- factor(crime.data$month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
qplot(crime.data$month, xlab= "Month", main="Crimes by month")+ scale_y_continuous("Number of crimes")


chota <- seq(2,nrow(crime.data),10)
chota <- crime.data[chota,]




time.tag.small=chron(times=c("00:00:00", "02:00:00", "04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00", "23:59:59")) 
time.tag.small

crime.data$time.tag.small=cut(crime.data$time,breaks= time.tag.small,labels=c("00-02","02-04", "04-06", "06-08","08-10","10-12","12-14","14-16","16-18","18-20","20-22","22-24"), include.lowest=TRUE)

temp=aggregate(crime.data$Primary.Type,by=list(crime.data$Primary.Type,crime.data$time.tag.small),FUN=length)

names(temp)=c("crime","time.tag.small","count")

ggplot(temp,aes(x=factor(time.tag.small),y=crime))+geom_tile(aes(fill=count))+scale_x_discrete("Crime",expand=c(0,0))+scale_y_discrete("Time of the day",expand = c(0,-2))+scale_fill_gradient("Number of crimes",low="white",high="steelblue")+theme_bw()+ggtitle("Crimes by time of day")+theme(panel.grid.major = element_line(color=NA),panel.grid.minor = element_line(color=NA))




tempData <- subset(crime.data,crime.data$time > "14:00:00")


chota$Date=as.POSIXct(chota$Date)
crime.agg=ddply(chota,.(crime,Arrest,Beat,Date,X.Coordinate,Y.Coordinate,time.tag,day,month),summarise,count=length(Date),.progress = 'text')


length(unique(crime.agg$Date))
beats <- sort(unique(crime.agg$Beat))
dates <- sort(as.character(unique(crime.agg$Date)))

temp <- expand.grid(beats, dates)
names(temp) <- c("Beat", "Date")
temp <- orderBy(~Beat, data=temp)


model.data <- aggregate(crime.agg[, c("count","Arrest")], by=list(crime.agg$Beat, as.character(crime.agg$Date)), FUN=sum)

names(model.data) <- c("Beat","Date","count","Arrest")

model.data <- merge(temp, model.data, by=c("Beat", "Date"), all.x= TRUE)

View(model.data)


model.data$count[is.na(model.data$count)] <- 0
model.data$Arrest[is.na(model.data$Arrest)] <- 0


model.data$day <- weekdays(as.Date(model.data$Date), abbreviate= TRUE)
model.data$month <- months(as.Date(model.data$Date), abbreviate= TRUE)



pastDays <- function(x) {c(0, rep(1, x))}

model.data$past.crime.1 <- ave(model.data$count, model.data$Beat,FUN = function(x) filter(x, pastDays(2), sides=1))


model.data$past.crime.7 <- ave(model.data$count, model.data$Beat,FUN= function(x) filter(x, pastDays(7), sides=1))
model.data$past.crime.30 <- ave(model.data$count, model.data$Beat,FUN= function(x) filter(x, pastDays(30), sides= 1))


meanNA <- function(x){
  mean(x, na.rm= TRUE)
}



model.data$past.crime.1 <- ifelse(is.na(model.data$past.crime.1),meanNA(model.data$past.crime.1), model.data$past.crime.1)



model.data$past.crime.7 <- ifelse(is.na(model.data$past.crime.7),
                                  meanNA(model.data$past.crime.7), model.data$past.crime.7)
model.data$past.crime.30 <- ifelse(is.na(model.data$past.crime.30),
                                     meanNA(model.data$past.crime.30), model.data$past.crime.30)



model.data$past.arrest.30 <- ave(model.data$Arrest, model.data$Beat,FUN=function(x) filter(x, pastDays(30), sides=1))


model.data$past.arrest.30 <- ifelse(is.na(model.data$past.arrest.30),meanNA(model.data$past.arrest.30), model.data$past.arrest.30)


cor(model.data$past.crime.7, model.data$past.arrest.7)



model.data$policing <- ifelse(model.data$past.crime.30 == 0, 0,model.data$past.arrest.30/model.data$past.crime.30)



