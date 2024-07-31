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

#station - the number of dock_count ranged from 11-27, change it to a factor to see the groups of dock_count
station_clean$dock_count <- as.factor(station_clean$dock_count)
summary(station_clean)

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

