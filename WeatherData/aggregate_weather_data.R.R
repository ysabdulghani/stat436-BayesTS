# Aggregates separate weather data files into one CSV
# Data is pulled from MSU's Grafana Database (from the Optical Remote Sensor Laboratory (ORSL))

library(readr)
library(tidyverse)
library(dplyr)
library(tidyr)
library(lubridate)

# Read in data
temp_data <- read_csv("./stat436-BayesTS/WeatherData/WeatherDataSep/Temperature-data-2024-10-31 15_42_08.csv") #temp is F
precip_data <- read_csv("./stat436-BayesTS/WeatherData/WeatherDataSep/Precipitation-data-2024-10-31 15_43_03.csv") #precip is In
pressure_data <- read_csv("./stat436-BayesTS/WeatherData/WeatherDataSep/Absolute Pressure-data-2024-10-31 15_44_04.csv") #pressure is InHG (Inches of Mercury)
wind_data <- read_csv("./stat436-BayesTS/WeatherData/WeatherDataSep/Wind Speed-data-2024-10-31 15_42_36.csv") #wind speed is in m/s

# Join tables by date
dfs <- list(temp_data, precip_data, pressure_data, wind_data)
weather_data <- dfs %>% 
  reduce(inner_join, by="Time")

# Convert time to YMD
weather_data <- weather_data %>%
  mutate(Time = as.Date(ymd_hms(Time)))

# Rename cols
weather_data <- weather_data %>%
  rename(Temp = Temperature_degF_, Precip = Precipitation_in_, Air_Pressure = Absolute_Pressure_inHg_, Wind_Speed = 'Wind_Speed_m/s_')

# Mean-impute missing data
date_sequence <- data.frame(Time = seq.Date(from = min(weather_data$Time),
                                            to = max(weather_data$Time),
                                            by = "day"))
weather_data_impute <- date_sequence %>%
  left_join(weather_data, by = "Time")

# take the weekly mean of each weather measurement and impute the missing data with calculated mean
weather_data_impute <- weather_data_impute %>%
  mutate(week = floor_date(Time, "week")) %>%
  group_by(week) %>%
  mutate(
    Temp = ifelse(is.na(Temp), mean(Temp, na.rm = TRUE), Temp),
    Precip = ifelse(is.na(Precip), mean(Precip, na.rm = TRUE), Precip),
    Air_Pressure = ifelse(is.na(Air_Pressure), mean(Air_Pressure, na.rm = TRUE), Air_Pressure),
    Wind_Speed = ifelse(is.na(Wind_Speed), mean(Wind_Speed, na.rm = TRUE), Wind_Speed)
  ) %>%
  ungroup() %>%
  select(-week)

weather_data <- weather_data_impute

# Add separate columns for Year, Month, and Date
weather_data <- weather_data %>%
  mutate(Year = year(Time), Month = month(Time), Day = day(Time))

# subset data to only include weather for August, September, and October
weather_data <- weather_data %>%
  subset(subset = (Month==8|Month==9|Month==10))

# Save to CSV
write_csv(weather_data, "./stat436-BayesTS/WeatherData/weather_data_bzn.csv")





