##   Peer-graded Assignment: Course Project 1
##   Author:  Andrew D. Stewart
##   Course:  Exploratory Analysis
##
## Our overall goal here is simply to examine how household energy usage varies 
## over a 2-day period in February, 2007. Your task is to reconstruct the 
## following plots below, all of which were constructed using the base 
## plotting system.
##
## First you will need to fork and clone the following GitHub
## repository: https://github.com/rdpeng/ExData_Plotting1
##
##  For each plot you should
##    • Construct the plot and save it to a PNG file with a width of 480 pixels
## and a height of 480 pixels.
##    • Name each of the plot files as plot1.png, plot2.png, etc.
##    • Create a separate R code file (plot1.R, plot2.R, etc.) that constructs
## the corresponding plot, i.e. code in plot1.R constructs the plot1.png plot. 
## Your code file should include code for reading the data so that the plot can
## be fully reproduced. You must also include the code that creates the PNG 
## file.
##   • Add the PNG file and R code file to the top-level folder of your git 
## repository (no need for separate sub-folders)
## When you are finished with the assignment, push your git repository to 
## GitHub so that the GitHub version of your repository is up to date. 
## There should be four PNG files and four R code
##files, a total of eight files in the top-level folder of the repo.
##
## Calls to setup libraries:
library(data.table)
library(dplyr)
library(lubridate)

## Create data directory for file path:
data_directory <- c("~/Desktop/Coursera/4 Exploratory Analysis/Program Files")    

## Form file name with direcotry and path:
file_name <- file.path(data_directory,"household_power_consumption.txt")

## Read first 5 rows to define column classes -- this technique is from
## our instructor, Dr. Peng found on:
## http://www.biostat.jhsph.edu/~rpeng/docs/R-large-tables.html
tab5rows <- read.table(file_name, header = TRUE, sep = ";", nrows = 5)
classes <- as.vector(sapply(tab5rows, class)) ##Store Class types in a vector
column_names <- colnames(tab5rows) ## Store Column names in a vector

## Use first date/time stamp of tab5rows to create a POSIXlt class
## This is the first date of the data
start_date <- c(as.character(tab5rows$Date[1]), as.character(tab5rows$Time[1]))

## Create POSIXlt class of the start date:
data_starts_on <- as.POSIXlt(strptime(paste(start_date[1], start_date[2], 
                            sep = " "), "%d/%m/%Y %H:%M:%S"))

## Per the instructions the start date is 01 Feb 2007 
period_start_date <- as.POSIXlt(strptime("2007-01-31 23:59:00", 
                                         "%Y-%m-%d %H:%M:%S"))

## Per the instructions the end date is 02 Feb 2007 
period_end_date <- as.POSIXlt(strptime("2007-02-02 23:59:00", 
                                         "%Y-%m-%d %H:%M:%S"))

## Calculate # of rows to skip as # of minutes from table start to start period:        
rows_to_skip <-  as.integer(difftime(period_start_date, data_starts_on, 
                                     units = "mins"))

## Calculate # of rows to read as # of minutes in the period of interest:        
rows_to_read <- as.integer(difftime(period_end_date, period_start_date, 
                                    units = "mins"))

## Read in subset of power data; label columns; define column classes:
power_consume_data <- read.table(file_name, header = TRUE, sep = ";",
        colClasses = classes, na.strings = "?", skip = (rows_to_skip+1),
        nrows = rows_to_read, col.names = column_names)

## Convert into tbl_df:
power_consume_data <- tbl_df(power_consume_data)

## Use first two date and time columns to create a column of POSIXlt date-times:
new_date_time <- new_date_time <- tbl_df(with(power_consume_data, 
                 as.POSIXlt(strptime(paste(Date,Time, sep = " "),
                                     "%d/%m/%Y %H:%M:%S"))))
## Rename date-time POSIXlt column:
new_date_time <- rename(new_date_time, "day_time" = value)

## Mutate the table with date-time POSIXlt column, replacing previous data in
## the Date column (preserves time column):
power_consume_data <- power_consume_data %>% mutate(Date = 
                                        new_date_time$day_time)

##      Let's Draw Some Plots!
##      PLOT #1:

png(filename = "plot1.png", width = 480, height = 480)

with(power_consume_data, hist(Global_active_power, col="red", 
                main = "Global Active Power", 
                xlab = "Global Active Power (kilowatts)"))

dev.off()
