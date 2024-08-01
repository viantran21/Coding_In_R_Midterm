#Midterm
#Coding in R Language
#Vian Tran 
#Summer 2024

#PLAN

#load the datasets (station, trips and weather) 
#note the trips.csv file is too large to upload and commit to GitHub so please 
#download the file and add it to your working directory 

#CLEAN

#use lubidate to change the dates to POSIX 
#change zip codes to only include USA zip codes (based on the areas the hubs are posted in), change the rest to NA 
#take the T's in precipitate and make them NAs
#remove duplicates 

#EXPLORATORY DATA ANALYSIS 

#1. get the number of observations (rows) and variables, and a head of the first cases (gimpse())

#2. get the metrics about data types, zeros, infinite numbers, and missing values
#Are all the variables in the correct data type?
#Variables with lots of zeros or NAs?
#Any high cardinality variable?

#3. freq function runs for all factor or character variables automatically
#If freq receives one variable -freq(data$variable)- it retruns a table. Useful to treat high cardinality variables (like zip code).
#Export the plots to jpeg into current directory: freq(data, path_out = ".")
#Does all the categories make sense?
#Lots of missing values?
#Always check absolute and relative values

#4. analyzing numerical variables - graphically and quantitatively
#GRAPHICALLY
#Try to identify high-unbalanced variables
#Visually check any variable with outliers
#QUANTITATIVELY
#Try to describe each variable based on its distribution (also useful for reporting)
#Pay attention to variables with high standard deviation
#Select the metrics that you are most familiar with: data_prof %>% select(variable, variation_coef, range_98): A high value in variation_coef may indictate outliers. range_98 indicates where most of the values are

#5. analyzing numerical and categorical at the same time
#Check min and max values (outliers)
#Check Distributions (same as before)

#based on the results from the EDA, it will be useful to clean the datasets and prep it for further downstream analysis 

#CANCELLED TRIPS

#check if the start station id is the same as the end station id 
#if it is less than 3 minutes it is a cancelled trip 
#duration variable is currently in seconds right now (3 minutes is 180 seconds)
#find the number of cancelled trip - you can make a new column for that 
#record the trip ids for your report - you can make a new dataset with cancelled trip variable with IDs, duration, start station id and end station id (for reporting later summarized)
#in the new dataset filter for just the cancelled trips 
#in the old dataset filter for not hte cancelled trips 
#remove the cancelled trips from the dataset 

#OUTLIERS

#identify outliers in the dataset (the EDA will help you narrow down which outliers to takeout)
#make a new column with outliers or not 
#make a new dataset with the necessary information similar to cancelled trip so we have a copy on file for reporting later
#record the trips ids for the final report 
#remove outliers from dataset 

#RUSH HOUR

#separate the start date (currently as MDY_HM) to its individual compements of of date (MDY) and time (HM)
#make a new column that separates and states whether its a weekday or weekend 
#display the hours in a histogram to visualize the rush hours times for weekdays only 
#find when the trip volume for the hours is the highest 

#RUSH HOUR FREQUENCIES

#for the rush hours range, find the 10 most frequent starting stations and ending stations 
#filter for rush hours entries during the week 
#have a graph that displays the frequnecy of starting station and end stations and determine the top 10

#WEEKEND FREQUENCIES

#filter for weekend dates 
#have a graph that displays the frequency of starting stations and ending stations 
#determine the top 10

#AVERAGE USE PER MONTH

#calculate the average utilization of bikes for each month
#(total time used/total time in month)

#WEATHER CONDITIONS

#combine trip and weather dataset 
#maybe we can look at it based on the city (via zip code - it may leave out a lot of entries so MAYBE right now) and date
#would you look at the end station name and match it to the right zip code that would match to weather (it might take long to do this)
#we can divide it by city and date and look at each weather metric to see which one shows any correlation (ie. graphs?)
#then with the final weather conditions we do correlation (cor() function)


