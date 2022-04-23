setwd("C:/Users/Krishnan CS/424_Project3")
getwd()

# LOADING LIBRARIES
library(dplyr)
library(ggplot2)
library(stringr)
library(purrr)
library(pryr)


#LOADING DATA FROM SOURCE
#File can be downloaded from https://data.cityofchicago.org/Transportation/Taxi-Trips-2019/h4cq-z3dy

taxi <- read.table(file = "Taxi_Trips_-_2019.tsv", sep = "\t", header = TRUE, quote = "\"")

#Basic stats/EDA
# head(taxi)
# colnames(taxi)
# summary(taxi)
# str(taxi)
# dim(taxi)

#Retaining only necessary columns and removing unwanted ones
taxi <- select(taxi, Trip.Start.Timestamp, Trip.Seconds, Trip.Miles, Pickup.Community.Area, Dropoff.Community.Area, Company )

print("selected cols needed")

#Removing trips less than 0.5 and more than 100 miles and less than 60 seconds, and greater than 5 hours,
taxi <- subset(taxi,  Trip.Miles >= 0.5 & Trip.Miles <= 100 & Trip.Seconds >= 60 & Trip.Seconds <= 18000 )
dim(taxi)

print("subset df")

#Renaming cols 
taxi <- taxi %>%
  rename( Date = Trip.Start.Timestamp,
          Duration = Trip.Seconds,
          Miles = Trip.Miles,
          Pickup = Pickup.Community.Area,
          Dropoff = Dropoff.Community.Area)
print("renamed cols")

#Removing unwanted details from company names
taxi$Company <- sub("[0-9]+ - [0-9]+ ", "", taxi$Company)
taxi$Company <- sub("[0-9]+ - ", "", taxi$Company)
print("cleaned up names")


#Encoding taxi company name to integer
taxi$Company <- as.numeric(as.factor(taxi$Company))
print("encoded company names")
print(head(taxi))

#Converting timestamp from char to posix
taxi$Date <- strptime(taxi$Date, "%m/%d/%Y %I:%M:%S %p")

#Creating a separate col and storing only the starting hour rather than the 15 minute intervals in 24hr format(removing min,sec and AM/PM)
 taxi$Hour <- strftime(taxi$Date, "%H")

 #Storing only the date as hour is separately stored
 taxi$Date <- strptime(taxi$Date, "%Y-%m-%d")

 print("date and hour fixed")
 print(head(taxi))
 
#Splitting data file into smaller chunks
no_of_chunks <- 15
split_vector <- ceiling(1: nrow(taxi)/nrow(taxi) * no_of_chunks)
res <- split(taxi, split_vector)
map2(res, paste0("part_", names(res), ".csv"), write.csv, row.names=FALSE)

