





beat.shp <- readShapePoly("/Users/Akshansh/Desktop/R programming/shinyApps/CriminalDataAnalysis/Boundaries - Police Beats (current)/geo_export_8e9104f8-6d48-44ff-88e1-b12df66ccaf6.shp")
plot(beat.shp)

crimeSubset <- subset(crime.data, Year == 2015 , select = ID:crime)

sum(crime.data$month == "Jan" & crime.data$Primary.Type == "BATTERY")


