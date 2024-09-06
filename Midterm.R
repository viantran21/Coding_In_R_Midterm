#Midterm
#Coding in R Language
#Vian Tran 
#Summer 2024

#install packages necessary for Exploratory Data Analysis
#install.packages("tidyverse")
#install.packages("funModeling")
#install.packages("Hmisc")

library(tidyverse)
library(funModeling) 

rm(list = ls())

#contains data that represents stations where users can pick up or return bikes
station <- read_csv("station.csv")
#data about individual bike trips
# SK (Points taken) trip.csv is missing from repo. I am adding it.
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

#use lubridate to change the dates to Date/POSIX 
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
#keep the ZIP code if it starts with a digit (0-9) and is exactly 5 digits long, otherwise set it to NA
#also ensure "99999" and "0" are set to NA

#weather - take the T's in precipitate and make them NAs
# The codebook on the Kaggle link provided informs us that 'T' stands
# for trace amounts. They should be greater than zero.
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
# SK The output of the code below shows 70 unique start/end station IDs, and 
# 74 unique start/end station names. This is a discrepancy worth noting and 
# I realize you prefer to use station ids for this reason but this is not
# mentioned in your report.
statistics(trip_clean)
statistics(weather_clean)

#display necessary categorical variables in graphs
#set the colours for the graphs 
colours <- c("darkblue", "blue", "skyblue", "lightblue")

#station - display the categorical variables in graphs including dock counts and cities
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

#trip - display the categorical variables in graphs including start stations, end stations and subscription type
barplot(table(trip_clean$start_station_name), 
        main = paste("Frequency of Start Stations"), 
        xlab = "Start Stations", 
        ylab = "Frequency", 
        col = colours,
        ylim = c(0, 35000),
        cex.names = 1)

barplot(table(trip_clean$end_station_name), 
        main = paste("Frequency of End Stations"), 
        xlab = "End Stations", 
        ylab = "Frequency", 
        col = colours,
        ylim = c(0, 35000),
        cex.names = 1)

barplot(table(trip_clean$subscription_type), 
        main = paste("Frequency of Subscription Type"), 
        xlab = "Subscription Type", 
        ylab = "Frequency", 
        col = colours,
        ylim = c(0, 300000),
        cex.names = 1)

#display necessary numerical variables in graphs 
#trip - display necessary numerical variables including log(duration)
hist(log(trip_clean$duration), 
        main = paste("Frequency of loge(Duration)"), 
        xlab = "loge(Duration)", 
        ylab = "Frequency", 
        col = colours,
        ylim = c(0, 15e+04))
#NOTE - there are riders who have done on bike rides for more than a day

#the function will display necessary numerical variables in graphs 
#weather - display necessary and important weather variables filtered by city in graphs

#creating separate datasets and variables for each city
weather_clean_sanfran <- weather_clean %>% filter(weather_clean$city == "San Francisco")
weather_clean_red <- weather_clean %>% filter(weather_clean$city == "Redwood City")
weather_clean_palo <- weather_clean %>% filter(weather_clean$city == "Palo Alto")
weather_clean_mount <- weather_clean %>% filter(weather_clean$city == "Mountain View")
weather_clean_jose <- weather_clean %>% filter(weather_clean$city == "San Jose")

sanfran <- "San Francisco"
redwood <- "Redwood City"
palo <- "Palo Alto"
mountain <- "Mountain View"
jose <- "San Jose"

#weather plotting function will plot necessary variables (weather metrics) for each city
weather_plots_cities <- function(data, city) {
plot(data$date, data$max_temperature_f, main = paste0("Max Temperature (F) in ", city),
     xlab = "Date", ylab = "Max Temperature (F)", col = colours, ylim = c(30, 100))
  
plot(data$date, data$mean_temperature_f, main = paste0("Mean Temperature (F) in ", city),
     xlab = "Date", ylab = "Mean Temperature (F)", col = colours, ylim = c(30, 100))

plot(data$date, data$min_temperature_f, main = paste0("Min Temperature (F) in ", city),
     xlab = "Date", ylab = "Min Temperature (F)", col = colours, ylim = c(30, 100))

plot(data$date, data$max_visibility_miles, main = paste0("Max Visibility Miles in ", city),
     xlab = "Date", ylab = "Max Visibility Miles", col = colours, ylim = c(0, 10))

plot(data$date, data$mean_visibility_miles, main = paste0("Mean Visibility Miles in ", city),
     xlab = "Date", ylab = "Mean Visibility Miles", col = colours, ylim = c(0, 10))

plot(data$date, data$min_visibility_miles, main = paste0("Min Visibility Miles in ", city),
     xlab = "Date", ylab = "Min Visibility Miles", col = colours, ylim = c(0, 10))

plot(data$date, data$min_visibility_miles, main = paste0("Min Visibility Miles in ", city),
     xlab = "Date", ylab = "Min Visibility Miles", col = colours, ylim = c(0, 10))

plot(data$date, data$max_wind_Speed_mph, main = paste0("Max Wind Speed (mph) ", city),
     xlab = "Date", ylab = "Max Wind Speed (mph)", col = colours, ylim = c(5, 40))

plot(data$date, data$mean_wind_speed_mph, main = paste0("Mean Wind Speed (mph) in ", city),
     xlab = "Date", ylab = "Mean Wind Speed (mph)", col = colours, ylim = c(5, 40))

plot(data$date, data$max_gust_speed_mph, main = paste0("Max Gust Speed (mph) in ", city),
     xlab = "Date", ylab = "Max Gust Speed (mph)", col = colours)

plot(data$date, data$precipitation_inches, main = paste0("Precipitation (inches) in ", city),
     xlab = "Date", ylab = "Precipitation (inches)", col = colours)

plot(data$date, data$cloud_cover, main = paste0("Cloud Cover in ", city),
     xlab = "Date", ylab = "Cloud Cover", col = colours)
}

#use the function to plot the weather conditions for each city throughout 2014
weather_plots_cities(weather_clean_sanfran, sanfran)
weather_plots_cities(weather_clean_red, redwood)
weather_plots_cities(weather_clean_mount, mountain)
weather_plots_cities(weather_clean_palo, palo)
weather_plots_cities(weather_clean_jose, jose)

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

#export the cancelled trip IDs as a CSV file 
write.csv(trip_cancelled, "trip_cancelled.csv", row.names = TRUE)

#update the main dataset and remove the cancelled trip for further use
trip_clean2 <- trip_clean2 %>%
  filter(trip_status == "trip")

#check if all the cancelled trips are removed 
sum(trip_clean2$trip_status == "cancelled")

#remove the helper column trip_status to remove cancelled trips
trip_clean2 <- subset(trip_clean2, select=c(-trip_status))

#Outliers
#we are interested in seeing the return of bikes to each station within the next 3 days so the outliers should be above 3 days (259200 seconds)
# SK Can you provide evidence for your assumption above? When identifying outliers
# try to use either subject matter expert opinion, or statistical approaches.
# Gut feelings are not always helpful.
trip_clean3 <- trip_clean2 %>%
  mutate(outliers = ifelse(duration < 259200, "data", "outlier")) 

#record the trip IDs that were outliers by isolating necessary variables in a new dataset
trip_outliers <- trip_clean3 %>% 
  select(id, duration, start_station_name, end_station_name, outliers) %>%
  filter(outliers == "outlier") #filter for outlier trip
#this dataset will contain the IDS of the outlier trips 

#export the outlier trip IDs as a CSV file 
write.csv(trip_outliers, "trip_outliers.csv", row.names = TRUE)

#update the main dataset and remove the outlier trip for further use
trip_clean3 <- trip_clean3 %>%
  filter(outliers == "data") #filter and isolates entries that not outliers (< 3 days)
#check if all the outliers are removed 
sum(trip_clean3$outliers == "outliers")
#remove the helper column outliers to keep the dataset clean 
trip_clean3 <- subset(trip_clean3, select=c(-outliers))

#histogram update of duration with the outliers removed
hist(log(trip_clean3$duration), 
     main = paste("Frequency of log(Duration)"), 
     xlab = "log(Duration)", 
     ylab = "Frequency", 
     col = colours,
     ylim = c(0, 15e+04))

#Rush Hour
#make a new column that separates and states whether its a weekday or weekend 
trip_clean4 <- trip_clean3 %>%
  mutate(
    day_of_week = wday(start_date, label = TRUE, abbr = FALSE),
    day_type = ifelse(day_of_week %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
  )

#filter for weekdays
trip_clean_weekday <- trip_clean4 %>%
  filter(day_type == "Weekday")

#check if there are any weekends
sum(trip_clean_weekday$day_type != "Weekday")

#extract the hour of the day from the start time 
trip_clean_weekday <- trip_clean_weekday %>%
  mutate(hour = hour(start_date))

#group by each hour of the day during the week
trip_clean_weekday <- trip_clean_weekday %>%
  group_by(hour)

#lets look at the # of trips for each hour of the day
hourly_trip_counts <- as.data.frame(table(trip_clean_weekday$hour))
names(hourly_trip_counts) <- c("hour", "trip_count")
#order them in ascending order 
hourly_trip_counts <- hourly_trip_counts %>%
  arrange(desc(trip_count))

#display the hours in a histogram to visualize the rush hours times for weekdays only 
#find when the trip volume for the hours is the highest
ggplot(hourly_trip_counts, aes(x = hour, y = trip_count)) +
  geom_bar(stat = "identity", fill = "skyblue") + #heights of the bars to represent the number of trips, use stat="identity"
  labs(title = "Trip Volume by Hour on Weekdays",
       x = "Hour of Day",
       y = "Number of Trips") +
  theme_minimal() 

#Rush Hour Frequencies 
#rush hour during the week is considered from 7 to 9 am and 4 (16) to 6 (18) pm 

#for the rush hours range, find the 10 most frequent starting stations and ending stations 
#filter for rush hours entries during the week (7 to 9 am and 4 to 6 pm)
trip_clean_rush_hour <- trip_clean_weekday %>%
  filter (hour %in% c(7, 8, 9, 16, 17, 18))
#check if the filtering worked
table(trip_clean_rush_hour$hour)

#lets look at the # of trips for each starting station ID
start_station_trips <- as.data.frame(table(trip_clean_rush_hour$start_station_id)) 
names(start_station_trips) <- c("start_station_ID", "trip_count")

#there are 4 differently named stations compared to the original station file so doing it based on the ID ensures all is accounted for
setdiff(start_station_trips$start_station_ID, station_clean$id)

#add the station name to start_station_trips
station_names <- station_clean %>%
  select(id, name)

station_names$id <- as.factor(station_names$id)
start_station_trips <- start_station_trips %>%
  left_join(station_names, by = c("start_station_ID" = "id"))

#look at the top 10 start station names 
start_station_trips <- start_station_trips %>%
  arrange(desc(trip_count)) %>% 
  head(10)

#lets look at the # of trips for each ending station
end_station_trips <- as.data.frame(table(trip_clean_rush_hour$end_station_id)) 
names(end_station_trips) <- c("end_station_ID", "trip_count")

#there are 4 differently named stations compared to the original station file so doing it based on the ID ensures all is accounted for
setdiff(end_station_trips$end_station_ID, station_clean$id)

#add the station name to end_station_trips
end_station_trips <- end_station_trips %>%
  left_join(station_names, by = c("end_station_ID" = "id"))

#look at the top 10 end station names 
end_station_trips <- end_station_trips %>%
  arrange(desc(trip_count)) %>% 
  head(10)

#Weekend Frequencies
#filter for weekend dates (use trip_clean4)
trip_clean_weekend <- trip_clean4 %>%
  filter(day_type == "Weekend")

#check if there are any weekdays
sum(trip_clean_weekend$day_type != "Weekend")

#lets look at the # of trips for each starting station 
start_station_weekend <- as.data.frame(table(trip_clean_weekend$start_station_id))
names(start_station_weekend) <- c("start_station_ID", "trip_count")

#there are 4 differently named stations compared to the original station file so doing it based on the ID ensures all is accounted for
setdiff(start_station_weekend$start_station_ID, station_clean$id)

#add the station name to start_station_weekend
start_station_weekend <- start_station_weekend %>%
  left_join(station_names, by = c("start_station_ID" = "id"))

#determine the top 10 starting station names
start_station_weekend <- start_station_weekend %>%
  arrange(desc(trip_count)) %>% 
  head(10)

#lets look at the # of trips for each ending station
end_station_weekend <- as.data.frame(table(trip_clean_weekend$end_station_id))
names(end_station_weekend) <- c("end_station_ID", "trip_count")

#there are 4 differently named stations compared to the original station file so doing it based on the ID ensures all is accounted for
setdiff(end_station_weekend$end_station_ID, station_clean$id)

#add the station name to end_station_weekend
end_station_weekend <- end_station_weekend %>%
  left_join(station_names, by = c("end_station_ID" = "id"))

#determine the top 10 ending station names
end_station_weekend <- end_station_weekend %>%
  arrange(desc(trip_count)) %>% 
  head(10)

#Average Use Per Month
#make a new column showcasing each month (similar to weekend/weekday)
trip_clean_month <- trip_clean4 %>%
  mutate(month = month(start_date, label = TRUE, abbr = FALSE))

#remove the day_of_week and day_type from the new dataset 
trip_clean_month <- trip_clean_month %>%
  select(-day_of_week, -day_type)

#find the total time used for each month and convert it to total time used in hours (currently in seconds)
month_duration <- trip_clean_month %>%
  group_by(month) %>%
  mutate(total_time_used = sum(duration),
         total_time_used = total_time_used/3600) %>%
  select(month, total_time_used) %>%
  unique()
  
#total time available - assign each month the # of days in each month (ie. Jan 31 days, Feb 28 days, etc.)
# SK lubridate package has a nice function for this
days_month <- data.frame(
  month = month.name,
  days = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
)

#assign these days to the month_duration dataset, merging them by the month (you can also do left_join)
month_duration <- merge(month_duration, days_month, by.x = "month", by.y = "month")

#total time available - convert the # of days to # of hours per each month
month_duration <- month_duration %>% 
  mutate(total_time_available = days*24)

#calculate the average utilization of bikes for each month(total time used/total time in month)
month_duration <- month_duration %>% 
  mutate(average_utilization = total_time_used/total_time_available) %>% 
  arrange(desc(average_utilization)) #see what months people use the bikes the most vs. least
# SK from your report "There is lower use in bikes in the winter months where it is colder 
# and may be snowing.". Your weather dataset events do not have a single entry for snow,
# so I guess snow in Bay Area is unlikely. 
#Weather Conditions
#clean the weather dataset a bit more - the wind speed above 50 mph is a strong gale to hurricane force level (over 75 mph)
#there was no reported high wind speed in those cities in 2014
#therefore any wind speed above 50 mph should be given NA
weather_clean1 <- weather_clean %>%
  mutate(max_wind_Speed_mph = ifelse(max_wind_Speed_mph > 50, NA, max_wind_Speed_mph))

#create separate trip and station dataset to includes variables that are necessary and remove the rest 
#subset trip - include id, duration, start_date, start_station_name, start_station_id
trip_subset <- trip_clean4 %>%
  select(id, duration, start_date, start_station_name, start_station_id)

#subset station - include id and city 
station_subset <- station_clean %>%
  select(id, city)

#join new trip and station dataset by the start_station_id (trip) and id (station) so that for each trip entry, it showcases the starting city 
trip_with_city <- trip_subset %>%
  left_join(station_subset, by = c("start_station_id" = "id"))

#make the start_date just the date YMD format 
trip_with_city <- trip_with_city %>%
  mutate(start_date = as.Date(start_date, format = "%m/%d/%Y"))

#group it by city and start_date and find the number of trips/entries for each day (start_date) 
trips_city_day <- trip_with_city %>%
  group_by(city, start_date) %>%
  summarise(num_trips = n())

#combined the trip and weather dataset based on city and date (weather) and start_date and city (trip)
trip_weather <- trips_city_day %>%
  left_join(weather_clean1, by = c("city", "start_date" = "date"))
# SK From your report; excluding events from the correlation analysis because
# of NAs is not realistic. The column records events, and the lack of an event does
# not mean it should be excluded. In terms of weather, it means no rain/fog
# happened. It would have been better to replace empty strings with "No event" and
# include them in the correlation as a factor column.

#group it by city and look at each weather metric to see which one shows any correlation using cor() function 
correlation <- trip_weather %>%
  group_by(city) %>%
  summarise(
    max_temperature = cor(num_trips, max_temperature_f, use = "complete.obs"),
    mean_temperature = cor(num_trips, mean_temperature_f, use = "complete.obs"),
    mmin_temperature = cor(num_trips, min_temperature_f, use = "complete.obs"),
    max_visibility = cor(num_trips, max_visibility_miles, use = "complete.obs"),
    mean_visibility = cor(num_trips, mean_visibility_miles, use = "complete.obs"),
    min_visibility = cor(num_trips, min_visibility_miles, use = "complete.obs"),
    max_wind_speed = cor(num_trips, max_wind_Speed_mph, use = "complete.obs"),
    mean_wind_speed = cor(num_trips, mean_wind_speed_mph, use = "complete.obs"),
    max_gust_speed = cor(num_trips, max_gust_speed_mph, use = "complete.obs"),
    precipitation = cor(num_trips, precipitation_inches, use = "complete.obs")
  ) 
#the correlation for max_visibility is NA because for San Jose and Mountain View, for every entry (everyday in the year), 
#they had a max visibility of 10 so the standard deviation is 0 
#this explains the warning in cor()

#transpose the matrix to make cities on the column and the weather metric for the rows
correlation_t <- as.matrix(t(correlation[,-1]))
#rename the columns the cities 
colnames(correlation_t) <- correlation$city

view(correlation_t)

