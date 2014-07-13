#library(xlsx)
#rowIndex <- 18:23
#colIndex <- 7:15
#dat <- read.xlsx("./GCDquiz1/getdata-data-DATA.gov_NGAP.xlsx", sheetIndex = 1, colIndex = colIndex, rowIndex = rowIndex)


##library(XML)
##doc <- xmlTreeParse("./GCDquiz1/getdata-data-restaurants.xml", useInternal = TRUE)
##rootNode <- xmlRoot(doc)
##xmlName(rootNode)

library(data.table)
##fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
##download.file(fileUrl, destfile="./GCDquiz1/housing.csv", method = "curl")
##dateDownloaded <- date()
DT <- fread("./GCDquiz1/housing.csv")
print(system.time(for(i in 1:1000){tapply(DT$pwgtp15,DT$SEX,mean)}))
print(system.time(for(i in 1:1000){DT[,mean(pwgtp15),by=SEX]}))
print(system.time(for(i in 1:1000){mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)}))
print(system.time(for(i in 1:1000){tapply(DT$pwgtp15,DT$SEX,mean)}))