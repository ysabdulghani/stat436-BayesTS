# Aggregates separate weather data files into one CSV
# Wind Speed and Air Pressure Data is pulled from MSU's Grafana Database (from the Optical Remote Sensor Laboratory (ORSL)) and Temperature and Precipitation data is the Sacajawea SNOTEL Station

library(readr)
library(tidyverse)
library(dplyr)
library(tidyr)
library(lubridate)

# Read in data
# temp_data <- read_csv("./stat436-BayesTS/WeatherData/WeatherDataSep/Temperature-data-2024-11-01 13_21_31.csv") #temp is F
# precip_data <- read_csv("./stat436-BayesTS/WeatherData/WeatherDataSep/Precipitation-data-2024-11-01 13_22_55.csv") #precip is In
pressure_data <- read_csv("./stat436-BayesTS/WeatherData/WeatherDataSep/Absolute Pressure-data-2024-11-01 13_21_49.csv") #pressure is InHG (Inches of Mercury)
wind_data <- read_csv("./stat436-BayesTS/WeatherData/WeatherDataSep/Wind Speed-data-2024-11-01 13_22_25.csv") #wind speed is in m/s
sac_data <- read_csv("./stat436-BayesTS/WeatherData/WeatherDataSep/sac_station_weather.csv") #temp is in F; precip is in in (same with snow depth)

# Fix data labels
sac_data <- sac_data %>%
  mutate(Date = ymd(mdy(Date))) %>%
  rename("Time" = Date)

pressure_data <- pressure_data %>%
  mutate(Time = as.Date(ymd_hms(Time)))

wind_data <- wind_data %>%
  mutate(Time = as.Date(ymd_hms(Time)))

# Join tables by date
dfs <- list(wind_data, pressure_data, sac_data)
weather_data <- dfs %>% 
  reduce(inner_join, by="Time")

# Rename cols
weather_data <- weather_data %>%
  rename(Avg_Temp = `Air Temperature Average (degF)`, Min_Temp = `Air Temperature Minimum (degF)`, Max_Temp = `Air Temperature Maximum (degF)`, Precip = `Precipitation Increment (in)`, Snow_Depth = `Snow Depth (in) Start of Day Values`, Air_Pressure = Absolute_Pressure_inHg_, Wind_Speed = 'Wind_Speed_m/s_')

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
  mutate( #replace NaN values with zero
    Avg_Temp = ifelse(is.na(Avg_Temp) | is.nan(Avg_Temp), 0, Avg_Temp),
    Min_Temp = ifelse(is.na(Min_Temp) | is.nan(Min_Temp), 0, Min_Temp),
    Max_Temp = ifelse(is.na(Max_Temp) | is.nan(Max_Temp), 0, Max_Temp),
    Precip = ifelse(is.na(Precip) | is.nan(Precip), 0, Precip),
    Air_Pressure = ifelse(is.na(Air_Pressure) | is.nan(Air_Pressure), 0, Air_Pressure),
    Wind_Speed = ifelse(is.na(Wind_Speed) | is.nan(Wind_Speed), 0, Wind_Speed),
    Snow_Depth = ifelse(is.na(Snow_Depth) | is.nan(Snow_Depth), 0, Snow_Depth)
  ) %>%
  mutate( #impute with the weekly mean for each non-date value
    Avg_Temp = ifelse(Avg_Temp == 0, mean(Avg_Temp, na.rm = TRUE), Avg_Temp),
    Min_Temp = ifelse(Min_Temp == 0, mean(Min_Temp, na.rm = TRUE), Min_Temp),
    Max_Temp = ifelse(Max_Temp == 0, mean(Max_Temp, na.rm = TRUE), Max_Temp),
    Precip = ifelse(Precip == 0, mean(Precip, na.rm = TRUE), Precip),
    Air_Pressure = ifelse(Air_Pressure == 0, mean(Air_Pressure, na.rm = TRUE), Air_Pressure),
    Wind_Speed = ifelse(Wind_Speed == 0, mean(Wind_Speed, na.rm = TRUE), Wind_Speed),
    Snow_Depth = ifelse(Snow_Depth == 0, mean(Snow_Depth, na.rm = TRUE), Snow_Depth)
  ) %>%
  ungroup() %>%
  select(-week)

weather_data <- weather_data_impute

# Adjust Absolute Air Pressure to account for the Bridger mountain weather station (Sacagawea) 
# Adjustment was calculated using the Barometric formula

mean_air_pressure <- mean(weather_data$Air_Pressure)
adj_val <- mean_air_pressure - 22.49 #air pressure for Sac weather station

weather_data <- weather_data %>%
  mutate(Air_Pressure = Air_Pressure - adj_val)

# Add separate columns for Year, Month, and Date
weather_data <- weather_data %>%
  mutate(Year = year(Time), Month = month(Time), Day = day(Time))

# subset data to only include weather for August, September, and October
weather_data_aug_to_oct <- weather_data %>%
  subset(subset = (Month==8|Month==9|Month==10))

# Save to CSV
write_csv(weather_data, "./stat436-BayesTS/WeatherData/weather_data_bzn.csv")
write_csv(weather_data_aug_to_oct, "./stat436-BayesTS/WeatherData/weather_data_bzn_aug_to_oct.csv")





