# Data Processing ---------------------------------------------------------

# Load Packages -----------------------------------------------------------

library(tidyverse)
library(skimr)
library(janitor)
library(lubridate)
library(magrittr)
library(corrplot)
library(usmap)
library(zoo)

# Set Seed ----------------------------------------------------------------

set.seed(92523)

# Load Data ---------------------------------------------------------------

daily_report <- read_csv("data/unprocessed/csse_covid_19_data/csse_covid_19_daily_reports_us/06-01-2020.csv") %>%
  clean_names()
time_series <- read_csv("data/unprocessed/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv") %>%
  clean_names()
google_mobility <- read_csv("data/unprocessed/Global_Mobility_Report.csv", col_types = list(
  col_character(),
  col_character(),
  col_character(),
  col_character(),
  col_date(format = ""),
  col_double(),
  col_double(),
  col_double(),
  col_double(),
  col_double(),
  col_double())) %>%
  clean_names()
apple_mobility <- read_csv("data/unprocessed/applemobilitytrends.csv") %>%
  clean_names()

# First, group the time series data by state and date:

state_totals <- time_series %>% pivot_longer(
  cols = 13:144,
  names_to = "date",
  values_to = "deaths") %>%
  group_by(province_state, date) %>%
  summarise(total_deaths = sum(deaths, na.rm = TRUE))

state_totals <- state_totals %>%
  mutate(
    date = as_date(date, format = "x%m_%d_%y")
  ) %>%
  arrange(province_state, date)

# Create new variables:

# Function to calculate some lag and change variables:
new_deaths <- function(data){
  data %>%
    mutate(
      change = total_deaths - lag(total_deaths),
      lag1_change = lag(change),
      lag2_change = lag(change, 2),
      lag3_change = lag(change, 3),
      lag4_change = lag(change, 4),
      lag5_change = lag(change, 5),
      lag6_change = lag(change, 6),
      lag7_change = lag(change, 7),
      lag8_change = lag(change, 8),
      lag9_change = lag(change, 9),
      lag10_change = lag(change, 10),
      lag11_change = lag(change, 11),
      lag12_change = lag(change, 12),
      lag13_change = lag(change, 13),
      lag14_change = lag(change, 14)
    )
}

state_totals <- state_totals %>%
  group_by(province_state, add = TRUE) %>%
  group_nest() %>%
  mutate(change = map(data, new_deaths)) %>%
  select(province_state, change) %>%
  unnest(cols = c(change)) %>%
  mutate(change = ifelse(is.na(change), 0, change)) %>%
  rename(state = province_state)

# Join mobility data:
google_dat <- google_mobility %>%
  filter(country_region == "United States", is.na(sub_region_2)) %>%
  filter(!is.na(sub_region_1)) %>%
  select(sub_region_1, date, 6:11) %>%
  rename(state = sub_region_1,
         retail_rec = retail_and_recreation_percent_change_from_baseline,
         grocery_pharm = grocery_and_pharmacy_percent_change_from_baseline,
         transit = transit_stations_percent_change_from_baseline,
         workplace = workplaces_percent_change_from_baseline,
         residential = residential_percent_change_from_baseline,
         parks = parks_percent_change_from_baseline)

state_totals <- state_totals %>% left_join(google_dat, by = c("state", "date"))

apple_mobility <- apple_mobility %>%
  filter(geo_type == "sub-region", region %in% state_totals$state) %>%
  pivot_longer(cols = 7:147, names_to = "date", values_to = "value") %>%
  mutate(date = as_date(date, format = "x%Y_%m_%d")) %>%
  rename(state = region) %>%
  select(state, transportation_type, date, value) %>%
  pivot_wider(names_from = transportation_type, values_from = value)

state_totals <- state_totals %>% left_join(apple_mobility, by = c("state", "date"))

# Now create lag variables using rolling means:
state_totals <- state_totals %>%
  group_by(state) %>%
  mutate(rollmean_retail_rec = rollapply(retail_rec, 7, FUN = function(x) mean(x, na.rm = TRUE), partial = TRUE, fill = NA),
         rollmean_grocery_pharm = rollapply(grocery_pharm, 7, FUN = function(x) mean(x, na.rm = TRUE), partial = TRUE, fill = NA),
         rollmean_parks = rollapply(parks, 7, FUN = function(x) mean(x, na.rm = TRUE), partial = TRUE, fill = NA),
         rollmean_transit = rollapply(transit, 7, FUN = function(x) mean(x, na.rm = TRUE), partial = TRUE, fill = NA),
         rollmean_workplace = rollapply(workplace, 7, FUN = function(x) mean(x, na.rm = TRUE), partial = TRUE, fill = NA),
         rollmean_residential = rollapply(residential, 7, FUN = function(x) mean(x, na.rm = TRUE), partial = TRUE, fill = NA),
         rollmean_driving = rollapply(driving, 7, FUN = function(x) mean(x, na.rm = TRUE), partial = TRUE, fill = NA)) %>% 
  mutate(retail_rec_lag14 = lag(rollmean_retail_rec, 17),
         grocery_pharm_lag14 = lag(rollmean_grocery_pharm, 17),
         parks_lag14 = lag(rollmean_parks, 17),
         transit_lag14 = lag(rollmean_transit, 17),
         workplace_lag14 = lag(rollmean_workplace, 17),
         residential_lag14 = lag(rollmean_residential, 17),
         driving_lag14 = lag(rollmean_driving, 17),
         retail_rec_lag21 = lag(rollmean_retail_rec, 24),
         grocery_pharm_lag21 = lag(rollmean_grocery_pharm, 24),
         parks_lag21 = lag(rollmean_parks, 24),
         transit_lag21 = lag(rollmean_transit, 24),
         workplace_lag21 = lag(rollmean_workplace, 24),
         residential_lag21 = lag(rollmean_residential, 24),
         driving_lag21 = lag(rollmean_driving, 24)) %>%
  ungroup()
 
# Add weekday:
state_totals <- state_totals %>% mutate(
  weekday = weekdays(date)) %>% 
  select(state, date, weekday, everything()) %>%
  mutate(
    weekday = as_factor(weekday))


# Last step: Add indicator for how many days it's been since the first death
first_death <- state_totals %>%
  filter(total_deaths > 0) %>%
  group_by(state) %>%
  summarise(first_death = min(date)) 

state_totals <- state_totals %>%
  left_join(first_death, by = "state") %>%
  mutate(
    day_no = date - first_death) 

# Save dataset:
state_totals %>%
  write_csv("data/processed/modelling_data.csv")

