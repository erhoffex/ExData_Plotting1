plot1<-function(inputfile,startdate,enddate) {
## input the household power consumption data
## read headers
testdataheader<-read.table(inputfile,header=TRUE,sep=";",nrows=1)
testdataselect<-read.table(inputfile,header=FALSE,sep=";",skip=66636,nrows=2883)
colnames(testdataselect)<-colnames(testdataheader)

## convert data for date/time
testdataselect$Date<-as.character(testdataselect$Date)
testdataselect$Time<-as.character(testdataselect$Time)

## remove the non-data values "?"

## cull out the data for specified dates
SelectedDates<-data.frame()
i<-1
totalobs<-nrow(testdataselect)

for (i in 1:totalobs) {
 if (testdataselect$Date[i]==startdate) {
    SelectedDates<-rbind(SelectedDates,testdataselect[i,])
 }
 if (testdataselect$Date[i]==enddate) {
    SelectedDates<-rbind(SelectedDates,testdataselect[i,])
 }

i<-i+1
}

## set up the plot data
png(file="plot1.png")
hist(SelectedDates$Global_active_power,col="red")
title(main=NULL,sub=NULL)
title(main="Global Active Power", sub = "Global Active Power(kilowatts)",outer=TRUE)

dev.off()

}