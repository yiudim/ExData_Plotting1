plot3 <- function() {
    ## size estimate
    ## size estimaee = 2,075,259 rows * 9 columns * 8bytes/numeric /1000000000 ~= 1.48GB

    hpc.all <- read.table(fileurl, sep=";", nrow=2075259, header=TRUE, na.strings="?")
    hpc.all$fDate <- as.Date(hpc.all$Date, format="%d/%m/%Y")
    hpc.all <- subset(hpc.all, select=-c(Date))
    hpc <- subset(hpc.all, fDate == "2007/02/01" | fDate == "2007/02/02")
    rm(hpc.all)	## free large data table memory

    library(dplyr)

    hpc <- hpc.all %>%
	mutate(fDate = as.Date(Date, format("%d/%m/%Y"))) %>%
	select(-Date) %>%
	filter(fDate == "2007/02/01" | fDate == "2007/02/02")

    ## plot 3
    png(filename="plot3.png", width=480, height=480)
    ts1 <- ts(hpc$Sub_metering_1)
    ts2 <- ts(hpc$Sub_metering_2)
    ts3 <- ts(hpc$Sub_metering_3)
    plot(
    ts.plot(ts1, ts2, ts3, gpars = list(col=c("black","red","blue")), xaxt="n", xlab="", ylab="Energy sub metering")
    axis(side=1, at=c(0,nrow(hpc)/2,nrow(hpc)),labels=c("Thu","Fri","Sat"))
    dev.off()
}
