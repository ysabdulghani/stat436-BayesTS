# Aggregates weather data and raptor bird files into one CSV

library(readr)
library(tidyverse)
library(dplyr)
library(tidyr)
library(lubridate)

# Read in data
weather_data <- read_csv("./stat436-BayesTS/WeatherData/weather_data_bzn.csv")
raptor_bird_data <- read_csv("./stat436-BayesTS/raptor_bird_data.csv")

# Modify raptor data
raptor_bird_data <- raptor_bird_data %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  rename("Time" = Date)

# Add more dates
date_1 <- seq(as.Date("2019/11/01"), as.Date("2020/08/26"), by = 1)
date_2 <- seq(as.Date("2020/11/01"), as.Date("2021/08/26"), by = 1)
date_3 <- seq(as.Date("2021/11/01"), as.Date("2022/08/26"), by = 1)
date_4 <- seq(as.Date("2022/11/01"), as.Date("2023/08/26"), by = 1)
date_5 <- seq(as.Date("2023/11/01"), as.Date("2024/08/26"), by = 1)

# Slice years together
raptor_years <- raptor_bird_data %>%
  mutate(Year = year(Time), Month = month(Time), Day = day(Time)) %>%
  select(Year, Time)

raptor_date_1 <- raptor_years[raptor_years$Year == "2019", ] %>% select(Time)
raptor_date_2 <- raptor_years[raptor_years$Year == "2020", ] %>% select(Time)
raptor_date_3 <- raptor_years[raptor_years$Year == "2021", ] %>% select(Time)
raptor_date_4 <- raptor_years[raptor_years$Year == "2022", ] %>% select(Time)
raptor_date_5 <- raptor_years[raptor_years$Year == "2023", ] %>% select(Time)

Time_1 <- data.frame(Time = raptor_date_1)
Time_2 <- data.frame(Time = date_1)
Time_3 <- data.frame(Time = raptor_date_2)
Time_4 <- data.frame(Time = date_2)
Time_5 <- data.frame(Time = raptor_date_3)
Time_6 <- data.frame(Time = date_3)
Time_7 <- data.frame(Time = raptor_date_4)
Time_8 <- data.frame(Time = date_4)
Time_9 <- data.frame(Time = raptor_date_5)
Time_10 <- data.frame(Time = date_5)

Dates <- rbind(Time_1, Time_2, Time_3, Time_4, Time_5, Time_6, Time_7, Time_8, Time_9, Time_10)

# Merge Dates with Raptor Data
raptor_bird_data_full <- merge(Dates, raptor_bird_data, all.x = T)

# Zero-impute raptor data
raptor_bird_data_full <- raptor_bird_data_full %>%
  mutate(Obs_Hrs = ifelse(is.na(Obs_Hrs) | is.nan(Obs_Hrs), 0, Obs_Hrs),
         Sharp_Shinned_Hawk = ifelse(is.na(Sharp_Shinned_Hawk) | is.nan(Sharp_Shinned_Hawk), 0, Sharp_Shinned_Hawk),
         Cooper_Hawk = ifelse(is.na(Cooper_Hawk) | is.nan(Cooper_Hawk), 0, Cooper_Hawk),
         Golden_Eagle = ifelse(is.na(Golden_Eagle) | is.nan(Golden_Eagle), 0, Golden_Eagle))

# Add Year, Month, and Day
raptor_bird_data_full <- raptor_bird_data_full %>%
  mutate(Year = year(Time), Month = month(Time), Day = day(Time))

# Create a subset of raptor data for August to October
raptor_bird_data_subset <- raptor_bird_data_full %>%
  subset(subset = (Month==8|Month==9|Month==10))

# Save out data 
write_csv(raptor_bird_data_full, "./stat436-BayesTS/raptor_bird_data.csv")
write_csv(raptor_bird_data_subset, "./stat436-BayesTS/raptor_bird_data_aug_to_oct.csv")




