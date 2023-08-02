#Installing and loading required packages

library(skimr)
library(janitor)
library(lubridate)
library(openair)
library(ggpubr)
library(viridis)
library(ggrepel)
library(dplyr)

#Importing data (CSV's)

daily_activity <- read.csv("C:/Users/santh/Downloads/case study/fitness/dailyActivity_merged.csv")
daily_sleep <- read.csv( "C:/Users/santh/Downloads/case study/fitness/sleepDay_merged.csv")
hourly_steps <- read.csv("C:/Users/santh/Downloads/case study/fitness/hourlySteps_merged.csv")
hourly_intensities <- read.csv("C:/Users/santh/Downloads/case study/fitness/hourlyIntensities_merged.csv")
hourly_calories <- read.csv("C:/Users/santh/Downloads/case study/fitness/hourlyCalories_merged.csv")

#Viewing metadata

head(daily_activity)
skim_without_charts(daily_activity)

head(daily_sleep)
skim_without_charts(daily_sleep)

head(hourly_steps)
skim_without_charts(hourly_steps)

head(hourly_intensities)
skim_without_charts(hourly_intensities)

head(hourly_calories)
skim_without_charts(hourly_calories)

#Cleaning and uniting data as required for our analysis

#1.Counting total number of users in each table
n_distinct(daily_activity$Id)
n_distinct(daily_sleep$Id)
n_distinct(hourly_steps$Id)
n_distinct(hourly_intensities$Id)
n_distinct(hourly_calories$Id)

#2.Finding Duplicates 
sum(duplicated(daily_activity))
sum(duplicated(daily_sleep))
sum(duplicated(hourly_steps))
sum(duplicated(hourly_intensities))
sum(duplicated(hourly_calories))

#we can see know there are dulpicate observations in daily_sleep

#lets clean them 
daily_sleep <- distinct(daily_sleep)

#Verifying it 

sum(duplicated(daily_sleep))

#3.Counting N/A values 
colSums(is.na(daily_activity))
colSums(is.na(daily_sleep))
colSums(is.na(hourly_steps))
colSums(is.na(hourly_intensities))
colSums(is.na(hourly_calories))

#4.Making variable names consistent

daily_activity <- clean_names(daily_activity)
daily_sleep <- clean_names(daily_sleep)
hourly_steps <- clean_names(hourly_steps)
hourly_intensities <- clean_names(hourly_intensities)
hourly_calories <- clean_names(hourly_calories)

#5.Making Date and time consistent
daily_activity <- daily_activity %>%
  rename(date = activity_date) %>%
  mutate(date = as_date(date, format = "%m/%d/%Y"))

daily_sleep <- daily_sleep %>%
  rename(date = sleep_day) %>%
  mutate(date = as_date(date,format ="%m/%d/%Y %I:%M:%S %p"))

hourly_steps<- hourly_steps %>% 
  rename(date_time = activity_hour) %>% 
  mutate(date_time = as.POSIXct(date_time,format ="%m/%d/%Y %I:%M:%S %p"))

hourly_calories<- hourly_calories %>% 
  rename(date_time = activity_hour) %>% 
  mutate(date_time = as.POSIXct(date_time,format ="%m/%d/%Y %I:%M:%S %p"))

#Verifying it
head(daily_activity, 3)
head(daily_sleep, 3)
head(hourly_steps,3)
head(hourly_calories,3)

#Merging Data sets as per our need
daily_activity.sleep <- right_join(daily_activity, daily_sleep, by=c ("id", "date"))

glimpse(daily_activity.sleep)

hourly_steps.calories <- full_join(hourly_steps, hourly_calories, by = c("id", "date_time"))
glimpse(hourly_steps.calories)

#Using Subsets to keep only necessary data
daily_activity.sleep <- select(daily_activity.sleep, `id`, `date`, `total_steps`, `calories`,
                               `total_minutes_asleep`)
head(daily_activity.sleep)

