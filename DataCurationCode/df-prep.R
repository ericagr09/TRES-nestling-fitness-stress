# Data prep file - January 23, 2026

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
  select(nest_key, clutch_size, egg_loss_num, egg_loss_reason, nest_fate_doy, nest_fate, num_fledged)

## wrangle weather data

# clean weather data
weather_df$date <- as.Date(weather_df$date, format = "%m/%d/%y")
weather_df$yday <- yday(weather_df$date)
weather_df$temp_C <- (weather_df$temp_F - 32) * 5/9
weather_df$year <- year(weather_df$date)
weather_df$yr_day_hr <- paste(weather_df$year, weather_df$yday, weather_df$hour, sep = "_")

## filter weather down only to years / days needed
weather_df <- weather_df %>%
  filter(yday > 120, yday < 200) %>%
  select(date, hour, yday, precip_inch, temp_C, year, yr_day_hr)

## combine nest and encounter data

nestling_df <- left_join(enc_df, nest_df, by = "nest_key")

## create variable for each development stage: earliest nestling, days ~0-5; mid-nestling, days ~5-12 and; late nestling, days ~12-fledging. 

nestling_df <- nestling_df %>%
  mutate(age = as.numeric(age)) %>%
  mutate(
    nestling_stage = case_when(
    between(age, 0, 4) ~ "early",      # days < 5 early nestling
    between(age, 5, 12) ~ "mid",    # days 5-12 mid nestling
    age > 12 ~ "late" # days > 12 late nestling
  ))

# connect weather data to nestling data - using dplyr to join instead of function b/c of size of data files

## filter to only average daytime temp
weather_df <- weather_df %>% 
  filter(
    hour >= 6,
    hour <= 20,
    !is.na(temp_C)
  )

## get average and mean for capture day
capture_df <- weather_df %>%
  group_by(year, yday) %>%
  summarise(
    avgC_capture_day = mean(temp_C),
    maxC_capture_day = max(temp_C),
    .groups = "drop"
  )

## link to nestling df based on encounter day of year 
nestling_df <- nestling_df %>%
  left_join(
    capture_df,
    by = c(
      "exp_year" = "year",
      "encounter_doy" = "yday"
    )
  )

## get average and mean for fledge day
fledge_df <- weather_df %>%
  group_by(year, yday) %>%
  summarise(
    avgC_fledge_day = mean(temp_C),
    maxC_fledge_day = max(temp_C),
    .groups = "drop"
  )

## link to nestling df based on fledge day of year 
nestling_df <- nestling_df %>%
  left_join(
    fledge_df,
    by = c(
      "exp_year" = "year",
      "nest_fate_doy" = "yday"
    )
  )

# create a binary variable for nest outcome (using functions to make sure it accounts for the messiness of the data)
nestling_df <- nestling_df %>%
  mutate(
    nestling_fate_bin = case_when(
      tolower(trimws(nestling_fate)) == "died"    ~ 0,
      tolower(trimws(nestling_fate)) == "fledged" ~ 1,
      TRUE                                        ~ NA_real_
    )
  )

## filter down to remove nests that had attrition to see if there is some kind of survival effect ???

"nestling_df2 <- nestling_df %>% 
  filter(
    nest_fate %in% c("Fledged"))" 
