plot3 <- function() {
    fileurl <- "./household_power_consumption.txt"

    library(dplyr)
    hpc.all <- read.table(fileurl, sep=";", nrow=2075259, header=TRUE, na.strings="?")

    hpc <- hpc.all %>%
	mutate(fDate = as.Date(Date, format("%d/%m/%Y"))) %>%
	select(-Date) %>%
	filter(fDate == "2007/02/01" | fDate == "2007/02/02") %>%
	mutate(fDate = as.POSIXct(paste(fDate,Time)))
    rm(hpc.all)

    ## plot 3
    png(filename="plot3.png", width=480, height=480)
    with(hpc, plot(fDate, Sub_metering_1, col="black", type="l", xlab="", ylab="Energy sub metering"))
    with(hpc, lines(fDate,Sub_metering_2, col="red"))
    with(hpc, lines(fDate,Sub_metering_3, col="blue"))
    legend("topright", legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
	   col=c("black","blue","red"),lty=1, lwd=1)
    dev.off()
}
