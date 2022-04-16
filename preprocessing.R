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
head(taxi)
colnames(taxi)
summary(taxi)
str(taxi)
dim(taxi)

#Retaining only necessary columns and removing unwanted ones
taxi <- select(taxi, Trip.Start.Timestamp, Trip.Seconds, Trip.Miles, Pickup.Community.Area, Dropoff.Community.Area, Company )

#Removing trips less than 0.5 and more than 100 miles and less than 60 seconds, and greater than 5 hours,
taxi <- subset(taxi,  Trip.Miles >= 0.5 & Trip.Miles <= 100 & Trip.Seconds >= 60 & Trip.Seconds <= 18000 )
dim(taxi)

#converting factor to char
taxi %>% modify_if(is.factor, as.character) -> taxi

#Removing unwanted details from company names
taxi$Company <- sub("[0-9]+ - [0-9]+ ", "", taxi$Company)
taxi$Company <- sub("[0-9]+ - ", "", taxi$Company)

#Encoding taxi company name to integer to save space by using a lookup table
hash <- new.env(hash = TRUE, parent = emptyenv(), size = 100L)

# Helper functions -vectorize assign, get and exists for convenience
assign_hash <- Vectorize(assign, vectorize.args = c("x", "value"))
get_hash <- Vectorize(get, vectorize.args = "x")
exists_hash <- Vectorize(exists, vectorize.args = "x")

# Keys would be the unique name and the corresponding pair would be an integer from 1-58(total no of taxi company names)
keys <- sort(unique(taxi$Company))
values <- 1:58

#assigning the hash
assign_hash(keys, values, hash)

#For retreiving values
#get_hash('Flash Cab', hash)
#hash[["24 Seven Taxi"]]

#Using the hash table(dict) to convert company names from text to int cutting space needed by half
taxi$Company <- ifelse( exists_hash(taxi$Company, hash), get_hash(taxi$Company, hash))
head(taxi)

#Converting timestamp from char to posix
taxi$Trip.Start.Timestamp <- strptime(taxi$Trip.Start.Timestamp, "%m/%d/%Y %I:%M:%S %p")

#Creating a separate col and storing only the starting hour rather than the 15 minute intervals in 24hr format(removing min,sec and AM/PM)
taxi_date_removed$Hour <- strftime(taxi_date_removed$Trip.Start.Timestamp, "%H")

#Converting to char and storing only the date
taxi$Trip.Start.Timestamp <- strftime(taxi$Trip.Start.Timestamp, "%m/%d/%Y")

#Converting NAs in Community Area code to 0 to save some more space
taxi$Pickup.Community.Area[is.na(taxi$Pickup.Community.Area)] <- 0
taxi$Dropoff.Community.Area[is.na(taxi$Dropoff.Community.Area)] <- 0

#Splitting data file into smaller chunks
no_of_chunks <- 25
split_vector <- ceiling(1: nrow(taxi)/nrow(taxi) * no_of_chunks)
res <- split(taxi, split_vector)
map2(res, paste0("part_", names(res), ".csv"), write.csv)

