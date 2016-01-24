plot1 <- function() {
    ## size estimate
    ## size estimaee = 2,075,259 rows * 9 columns * 8bytes/numeric /1000000000 ~= 1.48GB
    ## alternatively... code to estimate size... for reference only

    fileurl <- "./household_power_consumption.txt"
    ## fsize1000 <- object.size(read.table(fileurl, sep=";", nrow=1000))
    ## lines <- as.numeric(gsub("[^0-9]", "", system("wc -l household_power_consumption.txt", intern=T)))
    ## fsizeest <- lines / 1000 * fsize1000 

    library(dplyr)

    hpc.all <- read.table(fileurl, sep=";", nrow=2075259, header=TRUE, na.strings="?")

    hpc <- hpc.all %>%
	mutate(fDate = as.Date(Date, format("%d/%m/%Y"))) %>%
	select(-Date) %>%
	filter(fDate == "2007/02/01" | fDate == "2007/02/02")
    rm(hpc.all)

    ## plot 1
    png(filename="./plot1.png", width=480, height=480)
    with(hpc, hist( x=as.numeric(Global_active_power), 
		    col="red", 
		    main="Global Active Power", 
		    xlab="Global Active Power (kilowatts)"))
    dev.off()
}

