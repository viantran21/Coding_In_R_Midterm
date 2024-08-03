#Midterm
#Coding in R Language
#Vian Tran 
#Summer 2024

rm(list = ls())
#install packages necessary for Exploratory Data Analysis
#install.packages("tidyverse")
#install.packages("funModeling")
#install.packages("Hmisc")

library(tidyverse)
library(funModeling) 
library(Hmisc)

#contains data that represents a station where users can pick up or return bikes
station <- read_csv("station.csv")
#data about individual bike trips
trip <- read_csv("trip.csv")
#data about the weather on a specific day for certain zip codes/cities
weather <- read_csv("weather.csv")

#get an understanding of get dataset to help clean the data
summary(station)
str(station)
dim(station)
names(station)

summary(trip)
str(trip)
dim(trip)
names(trip)

summary(weather)
str(weather)
dim(weather)
names(weather)

#CLEAN
#remove duplicates 
station_clean <- distinct(station)
trip_clean <- distinct(trip)
weather_clean <- distinct(weather)

#use lubidate to change the dates to Date class
library(lubridate)
#station - installation_date change to MDY
station_clean$installation_date <- mdy(station_clean$installation_date)
class(station_clean$installation_date)
#trip - start_date change to MDY_HM
trip_clean$start_date <- mdy_hm(trip_clean$start_date)
class(trip_clean$start_date)
#trip - end_date change to MDY_HM
trip_clean$end_date <- mdy_hm(trip_clean$end_date)
class(trip_clean$end_date)
#weather - date change to MDY
weather_clean$date <- mdy(weather_clean$date)
class(weather_clean$date)

#trip - change zip codes to only include USA zip codes (based on the areas the hubs are posted in), change the rest to NA 
trip_clean <- trip_clean %>%
  mutate(zip_code = ifelse(grepl("^[0-9]{5}$", zip_code) & !zip_code %in% c("99999", "0"), zip_code, NA))
#keep the ZIP code if it starts with a digit (0-9) and it is exactly 5 digits long, otherwise set it to NA
#also ensure "99999" and "0" are set to NA

#weather - take the T's in precipitate and make them NAs
weather_clean <- weather_clean %>% 
  mutate(precipitation_inches = ifelse(precipitation_inches == "T", NA, precipitation_inches))
#precipitation_inches is currently a character, change to numeric
weather_clean$precipitation_inches <- as.numeric(weather_clean$precipitation_inches)
summary(weather_clean)

#Exploratory Data Analysis 

statistics <- function(data){
    print(summary(data)) #summary of dataset
    str(data) #number of observations (rows) and variables, and a head() of the first cases
    print(status(data)) #summary - quantity and percentages of zeros/NAs/infinite numbers, datatype and quantity of unique values
}

statistics(station_clean)
statistics(trip_clean)
statistics(weather_clean)

#display necessary categorical variables in graphs
colours <- c("darkblue", "blue", "skyblue", "lightblue")

#station
barplot(table(station_clean$dock_count), 
          main = paste("Frequency of Dock Counts"), 
          xlab = "Dock Counts", 
          ylab = "Frequency", 
          col = colours,
          ylim = c(0, 35),
          cex.names = 1)

barplot(table(station_clean$city), 
        main = paste("Frequency of Cities"), 
        xlab = "Cities", 
        ylab = "Frequency", 
        col = colours,
        ylim = c(0, 35),
        cex.names = 0.8)

#trip
barplot(table(trip_clean$start_station_name), 
        main = paste("Frequency of Start Stations"), 
        xlab = "Start Stations", 
        ylab = "Frequency", 
        col = colours,
        ylim = c(0, 35000),
        cex.names = 0.5)

barplot(table(trip_clean$end_station_name), 
        main = paste("Frequency of End Stations"), 
        xlab = "End Stations", 
        ylab = "Frequency", 
        col = colours,
        ylim = c(0, 35000),
        cex.names = 0.5)
max(table(trip_clean$end_station_name))

barplot(table(trip_clean$subscription_type), 
        main = paste("Frequency of Subscription Type"), 
        xlab = "Subscription Type", 
        ylab = "Frequency", 
        col = colours,
        ylim = c(0, 300000),
        cex.names = 1)

#this could also been done in a function but I wanted to personalized certain metrics of the graphs

#the function will display necessary numerical variable
#trip
hist(log(trip_clean$duration), 
        main = paste("Frequency of Duration"), 
        xlab = "Duration", 
        ylab = "Frequency", 
        col = colours,
        ylim = c(0, 15e+04))
#NOTE - there are riders who have done on bike rides for more than a day

#weather - update it based on the city 
#max_temperature_f
#mean_temperature_f
#min_temperature_f
#max_visibility_miles
#mean_visibility_miles
#min_visibility_miles
#max_wind_Speed_mph
#mean_wind_Speed_mph
#max_gust_speed_mph
#precipitation_inches
#cloud_cover























































#Cancelled Trips
#check if the duration is less than 3 minutes (180 seconds) and if the start_station_id is the same as the end_station_id this is a cancelled trip
#make a new column that states its "cancelled" or "trip"
trip_clean2 <- trip_clean %>%
  mutate(trip_status = ifelse(duration < 180 & start_station_id == end_station_id, "cancelled", "trip"))

#record the trip IDs by isolating necessary variables in a new dataset
trip_cancelled <- trip_clean2 %>% 
  select(id, duration, start_station_name, end_station_name, start_station_id, 
         end_station_id, trip_status) %>%
  filter(trip_status == "cancelled") #filter for cancelled trip
#this dataset will contain the IDS of the cancelled trips 

#update the main dataset and remove the cancelled trip for further use
trip_clean2 <- trip_clean2 %>%
  filter(trip_status == "trip")

