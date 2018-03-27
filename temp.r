tempp <- aggregate(crimeData$crime, by=list(crimeData$crime,
                                            crimeData$timeTag), FUN= length)
tempq <- aggregate(crimeData$crime, by=list(crimeData$crime,
                                            crimeData$day), FUN= length)
tempr <- aggregate(crimeData$crime, by=list(crimeData$crime,
                                            crimeData$month), FUN= length)
names(tempp) <- c("crime", "timeTag", "count")
names(tempp) <- c("crime", "day", "count")
names(tempr) <- c("crime", "month", "count")