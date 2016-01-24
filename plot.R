plot1 <- function() {
    ## size estimate
    ## size estimaee = 2,075,259 rows * 9 columns * 8bytes/numeric /1000000000 ~= 1.48GB
    ## alternatively... code to estimate size... for reference only
    ## fileurl <- "./household_power_consumption.txt"
    ## fsize1000 <- object.size(read.table(fileurl, sep=";", nrow=1000))
    ## lines <- as.numeric(gsub("[^0-9]", "", system("wc -l household_power_consumption.txt", intern=T)))
    ## fsizeest <- lines / 1000 * fsize1000 

    ## [1] "Time"                  "Global_active_power"   "Global_reactive_power"
    ## [4] "Voltage"               "Global_intensity"      "Sub_metering_1"       
    ## [7] "Sub_metering_2"        "Sub_metering_3"        "fDate"          
    ## read only the rows we need
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

    ## plot 1
    png(filename="plot1.png", width=480, height=480)
    with(hpc, hist( x=as.numeric(Global_active_power), 
		    col="red", 
		    main="Global Active Power", 
		    xlab="Global Active Power (kilowatts)"))
    dev.off()

    ## plot 2
    png(filename="plot2.png", width=480, height=480)
    plot.ts(ts(hpc$Global_active_power), ylab="Global Active Power (kilowatts)", xlab="", xaxt="n")
    axis(side=1, at=c(0,nrow(hpc)/2,nrow(hpc)),labels=c("Thu","Fri","Sat"))
    dev.off()

    ## plot 3
    png(filename="plot3.png", width=480, height=480)
    ts1 <- ts(hpc$Sub_metering_1)
    plot.ts(ts1, ylab="Energy sub metering", xlab="", xaxt="n")
    ts2 <- ts(hpc$Sub_metering_2)
    plot.ts(ts2, xaxt="n", col=r"red")
    ts2 <- ts(hpc$Sub_metering_3)
    plot.ts(ts2, xaxt="n", col=r"blue")
    dev.off()








    ## hpc$DateTime = strptime(paste(hpc$fDate,hpc$Time), format("%Y-%m-%d %H:%M:%S"))
    ## hpc <- subset(hpc, select=-c(Time))
