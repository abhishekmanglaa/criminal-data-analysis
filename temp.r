mergeMore<-read_csv("PublicFacilitiesCount.csv")
commArea <- readr::read_csv("CA.csv", col_names = FALSE)
commArea <- commArea[order(commArea$X1),]
commNames<-commArea$X2
