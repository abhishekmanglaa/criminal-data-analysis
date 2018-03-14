qplot(crime.data$crime,xlab = "Crime",main = "Crimes in Chicago")+scale_y_continuous("Number of Crimes")

qplot(crime.data$time.tag, xlab="Time of day", main="Crimes by time of day") + scale_y_continuous("Number of crimes")

crime.data$day <- factor(crime.data$day, levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
qplot(crime.data$day,xlab = "Day of Week",main="Crimes by day of Week")+scale_y_continuous("Number of crimes")

crime.data$month <- factor(crime.data$month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
qplot(crime.data$month, xlab= "Month", main="Crimes by month")+ scale_y_continuous("Number of crimes")

beat.shp <- readShapePoly("/Users/Akshansh/Desktop/R programming/shinyApps/CriminalDataAnalysis/Boundaries - Police Beats (current)/geo_export_8e9104f8-6d48-44ff-88e1-b12df66ccaf6.shp")
plot(beat.shp)

crimeSubset <- subset(crime.data, Year == 2015 , select = ID:crime)

sum(crime.data$month == "Jan" & crime.data$Primary.Type == "BATTERY")


