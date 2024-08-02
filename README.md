#Midterm
#Coding in R Language
#Vian Tran 
#Summer 2024

#PLAN

#load the datasets (station, trips and weather) 
#note the trips.csv file is too large to upload and commit to GitHub so please 
#download the file and add it to your working directory 

#EXPLORATORY DATA ANALYSIS - FIX ACCORDING TO NEW PLAN
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