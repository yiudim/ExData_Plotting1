plot2 <- function() {
    fileurl <- "./household_power_consumption.txt"

    library(dplyr)
    hpc.all <- read.table(fileurl, sep=";", nrow=2075259, header=TRUE, na.strings="?")

    hpc <- hpc.all %>%
	mutate(fDate = as.Date(Date, format("%d/%m/%Y"))) %>%
	select(-Date) %>%
	filter(fDate == "2007/02/01" | fDate == "2007/02/02") %>%
	mutate(fDate = as.POSIXct(paste(fDate,Time)))
    rm(hpc.all)


    ## plot 2
    png(filename="./plot2.png", width=480, height=480)
    with(hpc, plot(fDate, Global_active_power, ylab="Global Active Power", xlab="", type="l"))
    dev.off()
}
