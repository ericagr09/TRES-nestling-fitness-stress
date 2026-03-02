# Data prep file - January 23, 2026

## load packages

pacman::p_load(tidyverse, lubridate, here, dplyr)

## load data

enc_df <- read.csv(here("RawData", "01-13-26_encounters.csv"))
nest_df <- read.csv(here("RawData", "01-13-26_nest.csv"))
weather_df <- read.csv(here("RawData", "ithaca_airport_weather.csv"))
exclude_trt <- read.csv(here("RawData", "excluded_treatments.csv")) #df of excluded treatments from Conor

## wrangle encounter data

# filter down to just nestlings
enc_df <- enc_df %>% 
  filter(
    adult_or_nestling %in% c("Nestling")) 

## wrangle nest data

# select only needed variables 
nest_df <- nest_df %>% 
  select(nest_key, nest_treatment, clutch_size, egg_loss_num, egg_loss_reason, nest_fate_doy, nest_fate, num_fledged)

## combine nest and encounter data

nestling_df <- left_join(enc_df, nest_df, by = "nest_key")

## remove treatments - added 02/18/26

exclude_ind <- exclude_trt$treatments[exclude_trt$exclude_female == "yes" |
                                        exclude_trt$exclude_male == "yes" ] 
nestling_df <- nestling_df[!(nestling_df$individual_treatment %in% exclude_ind), ]
nestling_df <- nestling_df[!(nestling_df$nest_treatment %in% exclude_ind), ]

## create variable for each development stage: earliest nestling, days ~0-5; mid-nestling, days ~5-12 and; late nestling, days ~12-fledging. 

nestling_df <- nestling_df %>%
  mutate(age = as.numeric(age)) %>%
  mutate(
    nestling_stage = case_when(
      between(age, 0, 4) ~ "early",      # days < 5 early nestling
      between(age, 5, 12) ~ "mid",    # days 5-12 mid nestling
      age > 12 ~ "late" # days > 12 late nestling
    ))


## wrangle weather data

# clean weather data
weather_df$date <- as.Date(weather_df$date, format = "%m/%d/%y")
weather_df$yday <- yday(weather_df$date)
weather_df$temp_C <- (weather_df$temp_F - 32) * 5/9
weather_df$year <- year(weather_df$date)
weather_df$yr_day_hr <- paste(weather_df$year, weather_df$yday, weather_df$hour, sep = "_")

## filter weather down only to years / days needed
weather_df <- weather_df %>%
  select(date, hour, yday, precip_inch, temp_C, year, yr_day_hr)

## from Conor - added 02/18/26
yr_doy_uni <- nestling_df %>%
  dplyr::group_by(exp_year, encounter_doy) %>%
  summarise(count = n()) %>%
  as.data.frame()

# filter down hourly temperature data to daytime (6am-8pm)   
wh_day <- weather_df[is.na(weather_df$temp_C) == FALSE & weather_df$hour > 5 & weather_df$hour < 21 &
                       weather_df$yday > 110 & weather_df$yday < 250, ]

# for each year/day combo, calculate lagged temperature. Note slight difference with adults
# for day of capture where adults are 6-10am (because captures usually in morning),
# but nestlings are 6am-12pm because measures usually around midday
for(i in 1:nrow(yr_doy_uni)){
  sub1 <- subset(wh_day, wh_day$year == yr_doy_uni$exp_year[i] &
                   wh_day$yday > yr_doy_uni$encounter_doy[i] - 20 &
                   wh_day$yday < yr_doy_uni$encounter_doy[i] + 1)
  sub2 <- sub1[sub1$yday == yr_doy_uni$encounter_doy[i] & sub1$hour < 13, ]
  yr_doy_uni$cap_day_C[i] <- mean(sub2$temp_C)
  yr_doy_uni$prior1_C[i] <- mean(c(sub1$temp_C[sub1$yday > (yr_doy_uni$encounter_doy[i] - 2) &
                                                 sub1$yday < yr_doy_uni$encounter_doy[i]], sub2$temp_C))
  yr_doy_uni$prior2_C[i] <- mean(c(sub1$temp_C[sub1$yday > (yr_doy_uni$encounter_doy[i] - 3) &
                                                 sub1$yday < yr_doy_uni$encounter_doy[i]], sub2$temp_C))
  yr_doy_uni$prior3_C[i] <- mean(c(sub1$temp_C[sub1$yday > (yr_doy_uni$encounter_doy[i] - 4) &
                                                 sub1$yday < yr_doy_uni$encounter_doy[i]], sub2$temp_C))
  yr_doy_uni$prior4_C[i] <- mean(c(sub1$temp_C[sub1$yday > (yr_doy_uni$encounter_doy[i] - 5) &
                                                 sub1$yday < yr_doy_uni$encounter_doy[i]], sub2$temp_C))
  yr_doy_uni$prior5_C[i] <- mean(c(sub1$temp_C[sub1$yday > (yr_doy_uni$encounter_doy[i] - 6) &
                                                 sub1$yday < yr_doy_uni$encounter_doy[i]], sub2$temp_C))
  yr_doy_uni$prior6_C[i] <- mean(c(sub1$temp_C[sub1$yday > (yr_doy_uni$encounter_doy[i] - 7) &
                                                 sub1$yday < yr_doy_uni$encounter_doy[i]], sub2$temp_C))
  yr_doy_uni$prior7_C[i] <- mean(c(sub1$temp_C[sub1$yday > (yr_doy_uni$encounter_doy[i] - 8) &
                                                 sub1$yday < yr_doy_uni$encounter_doy[i]], sub2$temp_C))
}
yr_doy_uni$year_capday <- paste(yr_doy_uni$exp_year, yr_doy_uni$encounter_doy, sep = "_")
nestling_df$year_capday <- paste(nestling_df$exp_year, nestling_df$encounter_doy, sep = "_")
nestling_df<-  left_join(nestling_df,
                         yr_doy_uni,
                         by = "year_capday")


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
    nest_fate %in% c("Fledged"))" l
