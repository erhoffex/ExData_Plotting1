plot4<-function(inputfile,startdate,enddate) {
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
## Add DoW column
datime<-as.POSIXct(paste(SelectedDates$Date,SelectedDates$Time),format="%d/%m/%Y %H:%M:%S",tz="GMT")
SelectDates2<-cbind(datime,SelectedDates)
##SelectDates2$datime<-strptime(SelectDates2$datime,format="%d/%m/%Y %H:%M:%S")

##format(SelectedDates$Date,"%d/%m/%Y")
##SelectedDates2<-SelectedDates

datimeConversion<-weekdays(as.Date(SelectDates2$Date,format="%d/%m/%Y"))
SelectDates3<-cbind(datimeConversion,SelectDates2)

png(file="plot4.png")
par(mfrow=c(2,2))

## upper left
plot(SelectDates3$Global_active_power~SelectDates3$datime,ylab="Global Active Power (kilowatts)",xlab="",type="l")
title(main = "Plot4")

##upper right
plot(SelectDates3$Voltage~SelectDates3$datime,ylab="Voltage",xlab="",type="l")

## lower left
with(SelectedDates, plot(datime, Sub_metering_1, type = "n", ylab="Energy sub metering"))
    with(SelectedDates, lines(datime, Sub_metering_1, col = "black"))
    with(SelectedDates, lines(datime, Sub_metering_2, col = "red"))
    with(SelectedDates, lines(datime, Sub_metering_3, col = "blue"))
legend("topright",pch=1,col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))

##lower right
plot(SelectDates3$Global_reactive_power~SelectDates3$datime,ylab="Global_reactive_power",xlab="",type="l")

dev.off()

}