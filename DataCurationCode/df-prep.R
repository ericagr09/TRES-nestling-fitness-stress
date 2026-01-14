# Data prep file - January 13, 2026

## load packages

pacman::p_load(tidyverse, lubridate, here, dplyr)

## load data

enc_df <- read.csv(here("RawData", "01-13-26_encounters.csv"))
nest_df <- read.csv(here("RawData", "01-13-26_nest.csv"))
weather_df <- read.csv(here("RawData", "ithaca_airport_weather.csv"))

## wrangle encounter data

# filter down to just nestlings
enc_df <- enc_df %>% 
  filter(
    adult_or_nestling %in% c("Nestling")) 

## wrangle nest data

# select only needed variables 
nest_df <- nest_df %>% 
  select(nest_key, clutch_size, egg_loss_num, egg_loss_reason, nest_fate, num_fledged)

## wrangle weather data

# clean weather data
weather_df$date <- as.Date(weather_df$date, format = "%m/%d/%y")
weather_df$yday <- yday(weather_df$date)
weather_df$temp_C <- (weather_df$temp_F - 32) * 5/9
weather_df$year <- year(weather_df$date)
weather_df$yr_day_hr <- paste(weather_df$year, weather_df$yday, weather_df$hour, sep = "_")

# filter weather down only to years / days needed
"weather_df <- weather_df %>%
  filter(year > 2012, yday > 120, yday < 200) %>%
  select(date, hour, yday, precip_inch, temp_C, year, yr_day_hr)"

## combine nest and encounter data

nestling_df <- left_join(enc_df, nest_df, by = "nest_key")

## connect weather data to nestling data

"for(i in 1:nrow(d_enc)){
  # make a subset of temperature data that matches desired range from encounter (morning of capture)
  sub1 <- d_weather %>% filter(yday == d_enc$encounter_doy[i], year == d_enc$exp_year[i],
                               hour >= 6, hour <= 12, is.na(temp_C) == FALSE)
  
  # make another subset for a different time period (today plus two prior days; daytime only)
  sub2 <- d_weather %>% filter(yday >= (d_enc$encounter_doy[i] - 2), yday <= d_enc$encounter_doy[i],
                               year == d_enc$exp_year[i], hour >= 6, hour <= 20, is.na(temp_C) == FALSE)
  
  # use those subsets to assign avg/max/min/range of temperature to encounter as desired
  # note I'm adding a check to make sure there is temperature data available
  if(nrow(sub1) > 0){
    d_enc$avgC_capture_day[i] <- mean(sub1$temp_C)
    d_enc$maxC_capture_day[i] <- max(sub1$temp_C)
  }
  
  if(nrow(sub2) > 0){
    d_enc$avgC_3day[i] <- mean(sub1$temp_C)
  }
}"

## filter down to remove nests that had attrition to see if there is some kind of survival effect ???

"nestling_df2 <- nestling_df %>% 
  filter(
    nest_fate %in% c("Fledged"))" 
