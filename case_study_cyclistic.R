#load needed library
library(tidyverse)
library(dplyr)
library(readr)
library(janitor)
library(lubridate)
library(ggplot2)

# load and merge dataset
dataset <- list.files(path="C:\\Users\\jerem\\OneDrive\\Desktop\\Google Data Analytics\\data", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 
head(dataset)
glimpse(dataset) # 5,667,718 rows

#data cleaning
#remove duplicated ride_id
dataset <- dataset %>% distinct(ride_id, .keep_all = TRUE)
glimpse(dataset) #5,667,718 rows
summary(dataset)

#clean col names
dataset <- dataset %>% clean_names()
glimpse(dataset)
#add new column
#remove rows with null
dataset <- dataset[ , colSums(is.na(dataset)) < nrow(dataset)]

# add additional columns for further analysis
dataset$date <- as.Date(dataset$started_at)    
dataset$year <- format(as.Date(dataset$date), "%Y")
dataset$month <- format(as.Date(dataset$date), "%m")   
dataset$day <- format(as.Date(dataset$date), "%d")
dataset$day_of_week <- format(as.Date(dataset$date), "%A")
dataset$ride_length <- difftime(dataset$ended_at,dataset$started_at)
dataset$ride_length <- as.numeric(as.character(dataset$ride_length))
is.numeric(dataset$ride_length) # true
# discover rows with ride_length <= 0
# delete rows
dataset <- dataset[!(dataset$ride_length<0),]
View(dataset)

dataset <- dataset %>%
  mutate(weekday = wday(started_at, label = TRUE))
# save to file csv
write_csv(dataset, "merged_cleaned_data_v2.csv")

#load data
dataset <-read.csv("C:\\Users\\jerem\\OneDrive\\Desktop\\Google Data Analytics\\merged_cleaned_data_v2.csv")
#data visualization
dataset_1 <- dataset %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) 

ggplot(data=subset(dataset_1, !is.na(weekday)), aes(x = weekday, y = number_of_rides, fill = member_casual))+
  geom_col(position = "dodge")  # Sorts

dataset_2 <- dataset %>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, month)

ggplot(data=subset(dataset_2, !is.na(month)), aes(x = month, y = number_of_rides, fill = member_casual))+
         geom_col(position = "dodge") + scale_x_continuous(breaks=c(seq(1,12)))
# more to do on tableau

  
