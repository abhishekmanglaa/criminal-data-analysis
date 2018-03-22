output$plots <- renderPlot({
  p <- qplot(crimeData$crime,xlab = "Crime",fill = I("pink"),col = I("red"), main = "Crimes in Chicago")+scale_y_continuous("Number of Crimes")
  p <- qplot(crimeData$crime,xlab = "Crime",main = "Crimes in Chicago")+scale_y_continuous("Number of Crimes")
  q <- qplot(crimeData$timeTag, xlab="Time of day", main="Crimes by time of day") + scale_y_continuous("Number of crimes")
  r <- qplot(crimeData$day,xlab = "Day of Week",main="Crimes by day of Week")+scale_y_continuous("Number of crimes")
  s <- qplot(crimeData$month, xlab= "Month", main="Crimes by month")+ scale_y_continuous("Number of crimes")
  type <- input$typeofplot
  if(type == "Number of crimes vs CrimeType") { print(p)}
  if(type == "Crime by time of Day") { print(q)}
  if(type == "Crime By month") { print(s)}
  if(type == "Crime by day") { print(r)}
  
})