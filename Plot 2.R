##
##
##
##

library(data.table)
library(dplyr)
library(lubridate)

## Create data directory:
data_directory <- c("~/Desktop/Coursera/4 Exploratory Analysis/Program Files")    

## Read Data and return data tbl:
file_name <- file.path(data_directory,"household_power_consumption.txt")

## Read first 5 rows to define column classes -- this technique is from
## Dr. Peng found on:
## http://www.biostat.jhsph.edu/~rpeng/docs/R-large-tables.html

tab5rows <- read.table(file_name, header = TRUE, sep = ";", nrows = 5)
classes <- as.vector(sapply(tab5rows, class))
column_names <- colnames(tab5rows)

## Use first date/time stamp of tab5rows to create a POSIXlt class
start_date <- c(as.character(tab5rows$Date[1]), as.character(tab5rows$Time[1]))

data_starts_on <- as.POSIXlt(strptime(paste(start_date[1], start_date[2], 
                            sep = " "), "%d/%m/%Y %H:%M:%S"))

period_start_date <- as.POSIXlt(strptime("2007-01-31 23:59:00", 
                                         "%Y-%m-%d %H:%M:%S"))

period_end_date <- as.POSIXlt(strptime("2007-02-02 23:59:00", 
                                         "%Y-%m-%d %H:%M:%S"))
        
rows_to_skip <-  as.integer(difftime(period_start_date, data_starts_on, 
                                     units = "mins"))

rows_to_read <- as.integer(difftime(period_end_date, period_start_date, 
                                    units = "mins"))

power_consume_data <- read.table(file_name, header = TRUE, sep = ";",
        colClasses = classes, na.strings = "?", skip = (rows_to_skip+1),
        nrows = rows_to_read, col.names = column_names)

power_consume_data <- tbl_df(power_consume_data)

new_date_time <- new_date_time <- tbl_df(with(power_consume_data, 
                 as.POSIXlt(strptime(paste(Date,Time, sep = " "),
                                     "%d/%m/%Y %H:%M:%S"))))

new_date_time <- rename(new_date_time, "day_time" = value)

power_consume_data <- power_consume_data %>% mutate(Date = 
                                        new_date_time$day_time)
##
##

png(filename = "plot2.png", width = 480, height = 480)

with(power_consume_data, plot(Date,Global_active_power, type = "l", 
                ylab = "Global Active Power (kilowatts)",               
                xlab = ""))

dev.off()
