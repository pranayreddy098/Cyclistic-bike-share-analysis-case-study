setwd("C:/Users/Pranay/Desktop/data")
d1<-read.csv("202301-divvy-tripdata.csv")
d2<-read.csv("202302-divvy-tripdata.csv")
d3<-read.csv("202303-divvy-tripdata.csv")
d4<-read.csv("202304-divvy-tripdata.csv")
d5<-read.csv("202305-divvy-tripdata.csv")
d6<-read.csv("202306-divvy-tripdata.csv")
d7<-read.csv("202307-divvy-tripdata.csv")
d8<-read.csv("202308-divvy-tripdata.csv")
d9<-read.csv("202309-divvy-tripdata.csv")
d10<-read.csv("202310-divvy-tripdata.csv")
d11<-read.csv("202311-divvy-tripdata.csv")
d12<-read.csv("202312-divvy-tripdata.csv")

library(tidyverse)
library(janitor)
library(scales)
library(lubridate)
library(dplyr)
library(modeest)
library(tidyr)
## combining the csv files
tripdata_2023<- rbind(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12)
##Check for duplicates

df_unique <- tripdata_2023 %>%
  distinct(ride_id, .keep_all = TRUE)

# Print the number of rows before and after removing duplicates
original_row_count <- nrow(df)
new_row_count <- nrow(df_unique)
duplicates_removed <- original_row_count - new_row_count

print(paste("Number of duplicates removed:", duplicates_removed))

## split the started_at and ended_at 
tripdata_2023 <- tripdata_2023 %>%
  mutate(
    started_date = as.Date(started_at),
    started_time = format(as.POSIXct(started_at), "%H:%M:%S"),
    ended_date = as.Date(ended_at),
    ended_time = format(as.POSIXct(ended_at), "%H:%M:%S")
  )
## creating a another data frame to take only required data 

tripdata_2023_v2 <- tripdata_2023[, c("ride_id", "rideable_type","start_station_name","started_date", "started_time","end_station_name", "ended_date", "ended_time", "member_casual")]

##replacing ride_id with numbers in sequence and converting ride_id to character data type
tripdata_2023_v2$ride_id <- seq(1, nrow(tripdata_2023_v2))
tripdata_2023_v2$ride_id <- as.character(tripdata_2023_v2$ride_id)

##calculating ride duration and assigning week and month name
tripdata_2023_v2 <- tripdata_2023_v2 %>%
  mutate(
    # Calculate ride duration in minutes
    ride_duration_minutes = as.numeric(difftime(
      ymd_hms(paste(ended_date, ended_time, sep=" ")),
      ymd_hms(paste(started_date, started_time, sep=" ")),
      units = "mins"
    )),
    
    # Extract week name and month name
    week_name = wday(ymd_hms(paste(started_date, started_time, sep=" ")), label = TRUE, abbr = FALSE),
    month_name = month(ymd_hms(paste(started_date, started_time, sep=" ")), label = TRUE, abbr = FALSE)
  )
# filter to remove negative values
tripdata_2023_v2.1<- tripdata_2023_v2 %>%
  filter(ride_duration_minutes>=0)


# calculating total rides
total_ride<-tripdata_2023_v2.1 %>%
  group_by(member_casual) %>%
  summarise(total_rides=n())

#plot for rides for members and casuals 
ggplot(data = total_ride) +
  geom_bar(mapping = aes(x = member_casual, y = total_rides, fill = member_casual), stat = "identity") +
  geom_text(aes(x = member_casual, y = total_rides, label = scales::comma(total_rides)), 
            vjust = -0.3, size = 4) +  # Adjust position and size of labels
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Members vs Casuals",
    y = "Number of Rides in 2023"
  ) +
  theme_minimal()

#calculating mean rides
avg_ride<-tripdata_2023_v2.1 %>%
  group_by(member_casual) %>%
  summarise(avg_length=mean(ride_duration_minutes,na.rm = TRUE))

#plot for average rides for members and casuals
ggplot(data = avg_ride) +
  geom_bar(mapping = aes(x = member_casual, y = avg_length, fill = member_casual), stat = "identity") +
  geom_text(aes(x = member_casual, y = avg_length, label = scales::number(avg_length,accuracy = 0.001)), 
            vjust = -0.3, size = 4) +  # Adjust position and size of labels
  scale_y_continuous(labels = scales::number) +
  labs(
    title = "Members vs Casuals",
    y = "Average of lenght of Rides in 2023"
  ) +
  theme_minimal()

#type of ride used for members and casuals
tripdata_2023_v2.1 %>%
  group_by(rideable_type, member_casual) %>%
  summarise(total_rides = n(), avg_duration = mean(ride_duration_minutes, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x = rideable_type, y = total_rides, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Type of Ride most preferred",
    y = "Total Rides"
  ) +
  scale_y_continuous(labels = scales::number) +
  theme_minimal()

#cal
total_rides_cal <-tripdata_2023_v2.1 %>%
  group_by(member_casual) %>%
  summarise(
    mean_rides=mean(ride_duration_minutes,na.rm = TRUE),
    median_rides=median(ride_duration_minutes,na.rm = TRUE),
    mode_rides=mfv(ride_duration_minutes,na_rm = TRUE),
    sd_rides=sd(ride_duration_minutes,na.rm = TRUE),
    max_rides=max(ride_duration_minutes,na.rm = TRUE),
    min_rides=min(ride_duration_minutes)
    
  )


#average ride duration
tripdata_2023_v2.1 %>%
  group_by(member_casual, week_name) %>%
  summarise(total_rides = n(), avg_duration = mean(ride_duration_minutes, na.rm = TRUE), .groups = 'drop') %>%
  arrange(member_casual, week_name) %>%
  ggplot(aes(x = week_name, y = avg_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(
    title = "Average Ride Duration by Weekdays",
    x = "Day of the Week",
    y = "Average Ride Duration (minutes)",
    fill = "Member Type"
  ) +
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1))



#total rides by weekdays
tripdata_2023_v2.1 %>%
  group_by(member_casual, week_name) %>%
  summarise(total_rides = n(), avg_duration = mean(ride_duration_minutes, na.rm = TRUE), .groups = 'drop') %>%
  arrange(member_casual, week_name) %>%
  ggplot(aes(x = week_name, y = total_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(
    title = "Total Rides by Weekdays",
    x = "Day of the Week",
    y = "Number of Rides",
    fill = "Member Type"
  ) +scale_y_continuous(labels = scales::number)+
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1))


#dividing months by seasons
tripdata_2023_v2.1 %>%
  group_by(month_name) %>%
  summarise(total_rides=n(),avg_duration=mean(ride_duration_minutes,na.rm = TRUE))%>%
  arrange(month_name)

get_season <- function(month) {
  case_when(
    month %in% c("December", "January", "February") ~ "Winter",
    month %in% c("March", "April", "May") ~ "Spring",
    month %in% c("June", "July", "August") ~ "Summer",
    month %in% c("September", "October", "November") ~ "Fall"
  )
}

# Plot total rides by month and season
tripdata_2023_v2.1 %>%
  group_by(month_name) %>%
  summarise(
    total_rides = n(),
    avg_duration = mean(ride_duration_minutes, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(season = get_season(month_name)) %>%
  arrange(month_name) %>%
  ggplot(aes(x = month_name, y = total_rides, fill = season)) +
  geom_col(position = "dodge") +
  labs(
    title = "Total Rides by Month and Season",
    x = "Month",
    y = "Number of Rides",
    fill = "Season"
  ) +
  scale_y_continuous(labels = scales::number) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Winter" = "blue", "Spring" = "green", "Summer" = "yellow", "Fall" = "darkorange"))

#average rides by months in 2023
tripdata_2023_v2.1 %>%
  group_by(member_casual,month_name) %>%
  summarise(total_rides=n(),avg_duration=mean(ride_duration_minutes,na.rm = TRUE))%>%
  arrange(member_casual,month_name)%>%
  ggplot(aes(x = month_name, y = avg_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(
    title = "Average Rides by Month",
    x = "Months",
    y = "Average Rides",
    fill = "Member Type"
  ) +scale_y_continuous(labels = scales::number)+
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1))


#total rides in 2023
tripdata_2023_v2.1 %>%
  group_by(member_casual,month_name) %>%
  summarise(total_rides=n(),avg_duration=mean(ride_duration_minutes,na.rm = TRUE))%>%
  arrange(member_casual,month_name)%>%
  ggplot(aes(x = month_name, y = total_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(
    title = "Total Rides by Month",
    x = "Months",
    y = "Number of Rides",
    fill = "Member Type"
  ) +scale_y_continuous(labels = scales::number)+
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1))

# popular starting stations
tripdata_2023_v2.1 %>%
  group_by(start_station_name) %>%
  summarise(no_of_starts = n()) %>%
  filter(start_station_name != "") %>%
  arrange(desc(no_of_starts))

# popular starting stations for members
tripdata_2023_v2.1 %>%
  filter(member_casual == 'member') %>%
  group_by(start_station_name) %>%
  summarise(no_of_starts = n()) %>%
  filter(start_station_name != "") %>%
  arrange(desc(no_of_starts))

# popular starting stations for casual
tripdata_2023_v2.1 %>%
  filter(member_casual == 'casual') %>%
  group_by(start_station_name) %>%
  summarise(no_of_starts = n()) %>%
  filter(start_station_name != "") %>%
  arrange(desc(no_of_starts))

# popular final stations for all users by end station
tripdata_2023_v2.1 %>%
  group_by(end_station_name) %>%
  summarise(no_of_ends = n()) %>%
  filter(end_station_name != "") %>%
  arrange(desc(no_of_ends))

# popular final stations for members by end station
tripdata_2023_v2.1 %>%
  filter(member_casual == 'member') %>%
  group_by(end_station_name) %>%
  summarise(no_of_ends = n()) %>%
  filter(end_station_name != "") %>%
  arrange(desc(no_of_ends))

# popular final stations for casual users by end station
tripdata_2023_v2.1 %>%
  filter(member_casual == 'casual') %>%
  group_by(end_station_name) %>%
  summarise(no_of_ends = n()) %>%
  filter(end_station_name != "") %>%
  arrange(desc(no_of_ends))
