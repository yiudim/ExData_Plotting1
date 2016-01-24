plot4 <- function() {
    fileurl <- "./household_power_consumption.txt"

    library(dplyr)
    hpc.all <- read.table(fileurl, sep=";", nrow=2075259, header=TRUE, na.strings="?")

    hpc <- hpc.all %>%
	mutate(fDate = as.Date(Date, format("%d/%m/%Y"))) %>%
	select(-Date) %>%
	filter(fDate == "2007/02/01" | fDate == "2007/02/02") %>%
	mutate(fDate = as.POSIXct(paste(fDate,Time)))
    rm(hpc.all)

    ## plot 4
    png(filename="plot4.png", width=480, height=480)
	
    ## setup 2x2 plotting area
    par(mfrow=c(2,2),mar=c(4,4,3,1),oma=c(1,2,1,1))

    ## add plot [1,1]
    with(hpc, plot(fDate, Global_active_power, ylab="Global Active Power", xlab="", type="l"))

    ## add plot [1,2]
    with(hpc, plot(fDate, Voltage, ylab=names(Voltage), xlab="datetime", type="l"))

    ## add plot [2,1]
    with(hpc, plot(fDate, Sub_metering_1, col="black", type="l", xlab="", ylab="Energy sub metering"))
    with(hpc, lines(fDate,Sub_metering_2, col="red"))
    with(hpc, lines(fDate,Sub_metering_3, col="blue"))
    legend("topright", legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
	   col=c("black","blue","red"),lty=1, lwd=1)

    ## add plot [2,2]
    with(hpc, plot(fDate, Global_reactive_power, ylab=names(Global_reactive_power), xlab="datetime", type="l"))

    dev.off()
}
