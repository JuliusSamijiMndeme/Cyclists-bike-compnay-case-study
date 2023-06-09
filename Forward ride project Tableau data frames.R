
library(tidyverse)
library(lubridate)
library(hms)
library(data.table)

# make a new data frame that is going to be used to make visualizations in Tableau
rides_tableau <- rides_date

# view the data frame 
head(rides_tableau)

# remove the columns that are not needed for analysis when using tableau to make visualizations
rides_tableau <- rides_tableau %>%  
  select(-c(start_station_name, end_station_name, time, started_at, ended_at))

# change the months into names
rides_tableau$month = format(as.Date(rides_tableau$date), "%b")

#data frame 1: total rides basing on the member type using the pipe operator then use group by to categorize into two groups and count each group
df_1 =
  rides_tableau %>%
  group_by(member_casual) %>%
  count(member_casual)
fwrite(df_1,"total_number_of_rides_basing_on the_member_type.csv")

#data frame 2: total rides basing on the type of bike
df_2=
  rides_tableau %>%
  group_by(rideable_type) %>% 
  count(rideable_type)
fwrite(df_2,"total_number_of_rides_basing_on_the type of bike.csv")

#data frame 3: total rides by member type in each hour 
df_3=
  rides_tableau %>%
  group_by(member_casual) %>% 
  count(hour) 
fwrite(df_3,"total_number_of_rides_basing_on_the hour.csv")

#data frame 4: total rides in each hour
df_4=
  rides_tableau %>%
  count(hour) 
fwrite(df_4,"total_number_of_rides_each hour.csv")

#data frame 5: total rides by member type during the morning 
df_5=
  rides_tableau %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)
fwrite(df_5,"total_number_of_rides_per member in morning.csv")

#data frame 6: total rides by member type during the evening
df_6=
  rides_tableau %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)
fwrite(df_6,"total_number_of_rides_per member in evening.csv")


#data frame 7: total rides by member type during the night 
df_7=
  rides_tableau %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Night") %>% 
  count(time_of_day)
fwrite(df_7,"total_number_of_rides_per member at night.csv")

#data frame 8: total rides by member type during the afternoon  
df_8=
  rides_tableau %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)
fwrite(df_8,"total_number_of_rides_per member in afternoon.csv")

#data frame 9: total rides by member type in each time of day
df_9=
  rides_tableau %>%
  group_by(member_casual) %>% 
  count(time_of_day)
fwrite(df_9,"total_number_of_rides_per member per time of day.csv")

#data frame 10: total rides in each time of day
df_10=
  rides_tableau %>%
  group_by(time_of_day) %>% 
  count(time_of_day)
fwrite(df_10,"total_number_of_rides in each time of day.csv")

#data frame 11: total rides by member type basing on the day of the week
df_11=
  rides_tableau %>%
  group_by(member_casual) %>% 
  count(day_of_week)
fwrite(df_11,"total_number_of_rides by member in each time of day.csv")

#data frame 12: total rides in each day of the week 
df_12=
  rides_tableau %>%
  count(day_of_week)
fwrite(df_12,"total_number_of_rides in each day of week.csv")

#data frame 13: total rides by member type basing on the month day
df_13=
  rides_tableau %>%
  group_by(member_casual) %>% 
  count(day)
fwrite(df_13,"total_number_of_rides by member in each month day.csv")

#data frame 14: total rides in each day
df_14=
  rides_tableau %>%
  count(day) 
fwrite(df_14,"total_number_of_rides in each day.csv")

#data frame 15: total rides by member type in each month
df_15=
  rides_tableau %>%
  group_by(member_casual) %>% 
  count(month) 
fwrite(df_15,"total_number_of_rides by member in each month.csv")

#data frame 16: total rides in each month
df_16=
  rides_tableau %>%
  count(month)
fwrite(df_16,"total_number_of_rides in each month.csv")

#data frame 17: total rides by member type in each season
df_17=
  rides_tableau %>%
  group_by(season, member_casual) %>% 
  count(season)
fwrite(df_17,"total_number_of_rides by member in each season.csv")

#data frame 18: total rides in each season
df_18=
  rides_tableau %>%
  group_by(season) %>% 
  count(season)
fwrite(df_18,"total_number_of_rides in each season.csv")

#data frame 19: total number of rides by member in fall season
df_19=
  rides_tableau %>%
  group_by(member_casual) %>% 
  filter(season == "Fall") %>% 
  count(season)
fwrite(df_19,"total_number_of_rides by member in fall season.csv")

#data frame 20: total number of rides by member in summer season 
df_20=
  rides_tableau %>%
  group_by(member_casual) %>% 
  filter(season == "Summer") %>% 
  count(season)
fwrite(df_20,"total_number_of_rides by member in summer season.csv")

#data frame 21: total number of rides by member in spring season 
df_21=
  rides_tableau %>%
  group_by(member_casual) %>% 
  filter(season == "Spring") %>% 
  count(season)
fwrite(df_21,"total_number_of_rides by member in spring season.csv")

#data frame 22: total number of rides by member in winter season 
df_22=
  rides_tableau %>%
  group_by(member_casual) %>% 
  filter(season == "Winter") %>% 
  count(season)
fwrite(df_22,"total_number_of_rides by member in winter season.csv")
