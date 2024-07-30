
install.packages("tidyverse")
install.packages("funModeling")
install.packages("Hmisc")

library(tidyverse)
library(funModeling) 
library(Hmisc)

#contains data that represents a station where users can pick up or return bikes
station <- read_csv("station.csv")
#data about individual bike trips
trip <- read_csv("trip.csv")
#data about the weather on a specific day for certain zip codes/cities
weather <- read_csv("weather.csv")

