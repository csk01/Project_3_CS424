######## PREPROCESSING 

setwd("C:/Users/Krishnan CS/424_Project3")
getwd()

# LOAD LIBRARIES
library(dplyr)
library(ggplot2)
library(stringr)
library(purrr)
library(pryr)


#LOAD DATA
#File can be downloaded from https://data.cityofchicago.org/Transportation/Taxi-Trips-2019/h4cq-z3dy

taxi <- read.table(file = "Taxi_Trips_-_2019.tsv", sep = "\t", header = TRUE, quote = "\"")

#Basic stats
head(taxi)
colnames(taxi)
summary(taxi)
str(taxi)
dim(taxi)

#Dropping unwanted cols
taxi <- select(taxi, Trip.Start.Timestamp, Trip.Seconds, Trip.Miles, Pickup.Community.Area, Dropoff.Community.Area, Company )

#Removing trips less than 0.5 and more than 100 miles and less than 60 seconds, and greater than 5 hours, 
taxi_cleaned <- subset(taxi,  Trip.Miles > 0.5 | Trip.Miles <= 100 | Trip.Seconds < 60 | Trip.Seconds > 18000 )
dim(taxi_cleaned)

#Splitting data file into smaller chunks
no_of_chunks <- 25
split_vector <- ceiling(1: nrow(taxi_cleaned)/nrow(taxi_cleaned) * no_of_chunks)
res <- split(taxi_cleaned, split_vector)
map2(res, paste0("part_", names(res), ".csv"), write.csv)

