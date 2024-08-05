Midterm
Coding in R Language
Vian Tran 
Summer 2024

PLAN

load the datasets (station, trips and weather)

*note the trips.csv file is too large to upload and commit to GitHub so please download the file and add it to your working directory

CLEAN

1. remove duplicates 
2. use lubidate to change the dates to POSIX
3. change zip codes to only include USA zip codes, change the rest to NA 
4. take the T's in precipitate and make them NAs

EXPLORATORY DATA ANALYSIS 

1. use a function to get a summary, the number of observations (rows) and variables and a head of the first cases, and get the metrics about data types, zeros, infinite numbers, and missing values
2. display the categorical variables for each dataset using a barplot - we can visually see any trends/distributions 
3. display the numerical variables for each dataset in graphs - we can visually see any trends/distributions, outliers, variations, etc.
based on the results from the EDA, it will be useful to clean the datasets and prep it for further downstream analysis 


CANCELLED TRIPS

1. check if the start station id is the same as the end station id 
2. if it is less than 3 minutes it is a cancelled trip 
3. duration variable is currently in seconds right now (3 minutes is 180 seconds)
4. find the number of cancelled trip - you can make a new column for that 
5. record the trip ids for your report - you can make a new dataset with cancelled trip variable with IDs, duration, start station id and end station id (for reporting later summarized)
6. in the new dataset filter for just the cancelled trips - export this dataset as a CSV for the report
7. in the old dataset filter for not the cancelled trips and remove the cancelled trips from the dataset - this file will be used for further exploration down the line 

OUTLIERS

1. identify outliers in the dataset (the EDA will help you narrow down which outliers to takeout)

NOTE: the world record for longest distanced cycled without stopping was 202.1 km in 10 hours and 44 minutes. therefore, it is unrealistic for someone to bike for more than 11 hours  (39600 seconds) in one sitting. since it is a rental bike station, each individual will return the bike to a hub when they are not using the bike. also between cities bike, the further distance (San Francisco to San Jose) should not take more than 5 hours to bike without breaks so considering anything above 11 hours as outliers is reasonable 
2. make a new column with outliers or not 
3. make a new dataset with the necessary information similar to cancelled trip so we have a copy on file for reporting later
4. record the trips ids for the final report - export it as a CSV file 
5. remove outliers from dataset that will be used further down the line 

RUSH HOUR

1. make a new column that separates and states whether its a weekday or weekend 
2. filter for weekdays
3. extract the hour of the day from the start time 
4. group by each hour of the day during the week and look at the number of trips for each hour of the day (order them in descending order)
5. display the hours in a histogram to visualize the rush hours times for weekdays only 
6. find when the trip volume for the hours is the highest 

RUSH HOUR FREQUENCIES

1. filter for rush hours entries during the week 
2. look at the frequency (# of trips) for each starting stations
3. find the 10 most frequent starting stations 
4. look at the frequency (# of trips) for each ending stations
5. find the 10 most frequent ending stations 

WEEKEND FREQUENCIES
1. filter for weekend dates 
2. look at the frequency (# of trips) for each starting stations
3. find the 10 most frequent starting stations 
4. look at the frequency (# of trips) for each ending stations
5. find the 10 most frequent ending stations 

AVERAGE USE PER MONTH

1. make a new column showcasing each month
2. find the total time used (bikes) for each month and convert it to total time used in hours (currently in seconds)
3. for total time available - assign each month the # of days in each month (ie. Jan 31 days, Feb 28 days, etc.) and convert the # of days to # of hours per each month
4. calculate the average utilization of bikes for each month (total time used/total time in month)

WEATHER CONDITIONS

1. clean the weather dataset a bit more - the wind speed above 50 mph is a strong gale to hurricane force level (over 75 mph)

NOTE: there was no reported high wind speed in those cities in 2014. therefore any wind speed above 50 mph should be given NA
2. bofore combining trip and weather dataset: create separate trip and station dataset to includes variables that are necessary and remove the rest 

NOTE: to combine trip and weather we need to merge them by city and date but the trip dataset does not have the city. the station dataset has the station name and city 
3. need to merge trip and station dataset based on station id to include the city in the trip dataset 
4. group it by city and start_date and find the number of trips/entries for each day (start_date) 
5. combine trip and weather dataset based on date and city 
6. group it by city and look at each weather metrics to see which one shows any correlation using cor() function 


