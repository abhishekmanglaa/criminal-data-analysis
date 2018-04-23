Barr<-Barr[!is.na(Barr$`ZIP CODE`),]



ddply(Barr,Barr$`ZIP CODE`,summarise,number_of_distinct_orders=length(unique(Barr$`ZIP CODE`)))


sapply(Barr$`ZIP CODE`, function(x) length(unique(x)))


xyz<-as.data.frame(table(crimeData$ZIP))
sum(xyz$Freq)


ttyl<- revgeocode(c(crimeData$Longitude[10],crimeData$Latitude[10]),output = "more")

crimeData$ZIP <- NA

for(i in 1:length(crimeData$Latitude)){
  x<-revgeocode(c(crimeData$Longitude[i],crimeData$Latitude[i]),output = "more")
  
  if(!is.na(x)){
    chutiyap=data.frame(x$postal_code)
    print(chutiyap)
    abc <- as.numeric(as.character(chutiyap$x.postal_code))
    print(class(abc))
  crimeData$ZIP[i]<-abc}
  else
  {
    crimeData$ZIP[i] <-NA
    
  }
  
}




data(diamonds, economics_long, mpg, package = "ggplot2")
library(dplyr)
m<-na.omit(m)
hchart(m, "scatter", hcaes(x = m$Freq.x, y = m$Freq.y,group = m$Var1))


xyz1<-as.data.frame(table(Barr$`ZIP CODE`))
xyz1 <-xyz1[order(xyz1$Var1),]
xyz <-xyz[order(xyz$Var1),]

m<-merge(xyz,xyz1,by="Var1",all = TRUE)

