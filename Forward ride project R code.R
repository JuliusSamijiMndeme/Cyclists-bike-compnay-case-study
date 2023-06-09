# I imported the libraries that are going to be used in analysing the data
library(tidyverse)
library(lubridate)
library(hms)
library(data.table)

# the first step is to clean the data by generating new columns that are needed
# and removing the columns that are not needed
# also during data cleaning it is important to look for missing values
# and also remove the NA values and duplicate values 

# I imported the data sets for every month
january_df = read.csv("202201-jan-tripdata.csv")
february_df = read.csv("202202-feb-tripdata.csv")
march_df = read.csv("202203-march-tripdata.csv")
april_df = read.csv("202204-april-tripdata.csv")
may_df = read.csv("202205-may-tripdata.csv")
june_df = read.csv("202206-june-tripdata.csv")
july_df = read.csv("202207-july-tripdata.csv")
august_df = read.csv("202208-august-tripdata.csv")
september_df = read.csv("202209-september-tripdata.csv")
october_df = read.csv("202210-october-tripdata.csv")
november_df = read.csv("202211-nov-tripdata.csv")
december_df = read.csv("202212-dec-tripdata.csv")

# I merged the month data frames into one data frame to show all the data for the year 2022
rides_df = rbind(january_df,february_df,march_df,april_df,may_df,
                   june_df,july_df,august_df,september_df,october_df,november_df
                   ,december_df) 
# view the data set to see the structure of the columns
head(rides_df)

# I removed the individual month data sets to clear up space in the environment
remove(january_df,february_df,march_df,april_df,may_df,june_df,
       july_df,august_df,september_df,october_df,november_df,december_df)


# I calculated ride length from the difference between the start time and end time and store it in the ride length column
rides_df$ride_length = difftime(rides_df$ended_at, rides_df$started_at,units="mins")

# I created a data table for the dates using data table package
rides_date = as.data.table(rides_df)
rides_date
remove(rides_df)

# I creates a new column for the date on the data table
rides_date$date <- as.Date(rides_date$started_at)

# I created a new column for the day of the week
rides_date$day_of_week <- wday(rides_date$started_at)

# I created a new column for the name of the day of the week change the numbers used to names
rides_date$day_of_week <- format(as.Date(rides_date$date), "%A")

# I created a new column for the month
rides_date$month = format(as.Date(rides_date$date), "%m")

# I created a new column for the day
rides_date$day = format(as.Date(rides_date$date), "%d")

# I created a new column for the year 
rides_date$year = format(as.Date(rides_date$date),"%Y")

# I created a new column for the time format as HHMMSS empty format
rides_date$time = format(as.Date(rides_date$started_at),"%H:%M:%S")

# I used the hms package to get the time in hours minutes and seconds 
rides_date$time <- as_hms(strptime(rides_date$started_at, "%Y-%m-%d %H:%M:%S"))

# I created a new column for the hour
rides_date$hour = hour(rides_date$time)

# I removed all the columns that are not needed in the analysis 
rides_date <- rides_date %>%
  select(-c(ride_id, start_station_id, end_station_id,start_lat,start_lng,end_lat,end_lng))

 
# I removed rows with NA values
rides_date <- na.omit(rides_date)

# I removed duplicate rows
rides_date <- distinct(rides_date)

# I removed where ride_length is 0 or negative
rides_date <- rides_date[!(rides_date$ride_length <=0),]

# I created a column for the time of the day using the hours column
rides_date <- rides_date %>% 
                        mutate(time_of_day = case_when(hour == "0" ~ "Night",
                                                       hour == "1" ~ "Night",
                                                       hour == "2" ~ "Night",
                                                       hour == "3" ~ "Night",
                                                       hour == "4" ~ "Night",
                                                       hour == "5" ~ "Night",
                                                       hour == "6" ~ "Morning",
                                                       hour == "7" ~ "Morning",
                                                       hour == "8" ~ "Morning",
                                                       hour == "9" ~ "Morning",
                                                       hour == "10" ~ "Morning",
                                                       hour == "11" ~ "Morning",
                                                       hour == "12" ~ "Afternoon",
                                                       hour == "13" ~ "Afternoon",
                                                       hour == "14" ~ "Afternoon",
                                                       hour == "15" ~ "Afternoon",
                                                       hour == "16" ~ "Afternoon",
                                                       hour == "17" ~ "Afternoon",
                                                       hour == "18" ~ "Evening",
                                                       hour == "19" ~ "Evening",
                                                       hour == "20" ~ "Evening",
                                                       hour == "21" ~ "Evening",
                                                       hour == "22" ~ "Evening",
                                                       hour == "23" ~ "Evening"))

# I created column for different seasons: Spring, Summer, Fall, Winter
rides_date <-rides_date %>% 
                             mutate(season = case_when(month == "03" ~ "Spring",
                                                       month == "04" ~ "Spring",
                                                       month == "05" ~ "Spring",
                                                       month == "06"  ~ "Summer",
                                                       month == "07"  ~ "Summer",
                                                       month == "08"  ~ "Summer",
                                                       month == "09" ~ "Fall",
                                                       month == "10" ~ "Fall",
                                                       month == "11" ~ "Fall",
                                                       month == "12" ~ "Winter",
                                                       month == "01" ~ "Winter",
                                                       month == "02" ~ "Winter"))


# I created a new data frame that is going to be used to make visualizations in Tableau
rides_tableau <- rides_date

# I removed the columns that are not needed for analysis when using tableau to make visualizations
rides_tableau <- rides_tableau %>%  
  select(-c(start_station_name, end_station_name, time, started_at, ended_at))

# I saved the new data as a .csv file in the files directory so as to use to make visualizations on tableau
fwrite(rides_tableau,"rides_tableau.csv")

# I calculated the total number of rides by finding the total number of rows using the nrow function
nrow(rides_date)

# I calculated the total number of rides basing on the member type
rides_date %>%
  group_by(member_casual) %>%
  count(member_casual)


# I calculated the total number of rides basing on the type of bike
rides_date %>%
  group_by(rideable_type) %>% 
  count(rideable_type)

# I calculated the total rides by member type basing on every hour when the ride was made
rides_date %>%
  group_by(member_casual) %>% 
  count(hour) %>% 
  print(n = 48) #lets you view the entire tibble

# total rides
rides_date %>%
  count(hour) %>% 
  print(n = 24) #lets you view the entire tibble

# I calculated the total rides by member type during the morning time of day 
rides_date %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)

# I calculated total rides in the morning time of day
rides_date %>%
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)

# I calculated the total rides by member type during the afternoon time of day 
rides_date %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)

# I calculated the total rides in the afternoon time of day 
rides_date %>%
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)

# I calculated total rides by member type
rides_date %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)

# total rides
rides_date %>%
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)

# I calculated the total number of rides by member type during the night time of day
rides_date %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Night") %>% 
  count(time_of_day)

# I calculated the total number of rides during the night type
rides_date %>%
  filter(time_of_day == "Night") %>% 
  count(time_of_day)

# total rides by member type 
rides_date %>%
  group_by(member_casual) %>% 
  count(time_of_day)

# number of rides
rides_date %>%
  group_by(time_of_day) %>% 
  count(time_of_day)

# I calculated the total rides by member type basing on the day of the week
rides_date %>%
  group_by(member_casual) %>% 
  count(day_of_week)

# I calculated the total rides in each day of the week 
rides_date %>%
  count(day_of_week)

# I calculated the total rides by member type basing on the month day
rides_date %>%
  group_by(member_casual) %>% 
  count(day) %>% 
  print(n = 62) 

# I calcualted the total rides in each day
rides_date %>%
  count(day) %>% 
  print(n = 31) 

# I calculated the total rides by member type in each month
rides_date %>%
  group_by(member_casual) %>% 
  count(month) %>% 
  print(n = 24) 

# I calculated the total rides in each month
rides_date %>%
  count(month) 

# I calculated the total rides by member type basing on the seasons
rides_date %>%
  group_by(member_casual) %>% 
  filter(season == "Spring") %>% 
  count(season)

# total rides in spring
rides_date %>%
  filter(season == "Spring") %>% 
  count(season)

# total rides by member type
rides_date %>%
  group_by(member_casual) %>% 
  filter(season == "Summer") %>% 
  count(season)

# total rides
rides_date %>%
  filter(season == "Summer") %>% 
  count(season)

# total rides by member type
rides_date %>%
  group_by(member_casual) %>% 
  filter(season == "Fall") %>% 
  count(season)

# total rides
rides_date %>%
  filter(season == "Fall") %>% 
  count(season)

# total rides by member type
rides_date %>%
  group_by(member_casual) %>% 
  filter(season == "Winter") %>% 
  count(season)

# total rides 
rides_date %>%
  filter(season == "Winter") %>% 
  count(season)

# total rides by member type
rides_date %>%
  group_by(season, member_casual) %>% 
  count(season)

# total rides
rides_date %>%
  group_by(season) %>% 
  count(season)

# I calculated the average time of the ride_length
ridetime_avg <- mean(rides_date$ride_length)
print(ridetime_avg)

# average ride_length by the member type
rides_date %>% 
  group_by( member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# total rides by member type 
rides_date %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# I counted the number of rides basing on the type of bike used
rides_date %>% 
  group_by(member_casual, rideable_type) %>% 
  count(rideable_type)

# average ride_length
rides_date %>% 
  group_by(rideable_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# average ride_length by member type basing on the hours
rides_date %>% group_by(hour, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=48)

# average ride_length for every hour
rides_date %>% group_by(hour) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=24) 

# I calculated the average ride length by member type
rides_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Morning") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# avg ride length
rides_date %>% 
  filter(time_of_day == "Morning") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# avg ride length by member type
rides_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Afternoon") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# avg ride length
rides_date %>% 
  filter(time_of_day == "Afternoon") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# avg ride length by member type
rides_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Evening") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# avg ride length
rides_date %>% 
  filter(time_of_day == "Evening") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# avg ride length by member type 
rides_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Night") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# avg ride length
rides_date %>% 
  filter(time_of_day == "Night") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# avg ride length by member type
rides_date %>% 
  group_by(time_of_day, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# avg ride length
rides_date %>% 
  group_by(time_of_day) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# avg ride_length by member type
rides_date %>% group_by(member_casual, day_of_week) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# avg ride_length 
rides_date %>% group_by(day_of_week) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# avg ride_length by member type
rides_date %>% group_by(day, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=62)  

# avg ride_length
rides_date %>% group_by(day) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=31)  #lets you view entire tibble

# avg ride_length by member type
rides_date %>% group_by(month, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=24)  #lets you view entire tibble

# avg ride_length
rides_date %>% group_by(month) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# avg ride length by member type
rides_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Spring") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# avg ride length
rides_date %>% 
  filter(season == "Spring") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# avg ride length by member type for summer 
rides_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Summer") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# avg ride length for summer 
rides_date %>% 
  filter(season == "Summer") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# avg ride length by member type
rides_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Fall") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# avg ride length
rides_date %>% 
  filter(season == "Fall") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# avg ride length by member type
rides_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Winter") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# avg ride length
rides_date %>% 
  filter(season == "Winter") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# avg ride length by member type
rides_date %>% 
  group_by(season, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# avg ride length 
rides_date %>% 
  group_by(season) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))