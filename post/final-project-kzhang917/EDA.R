# Load Packages -----------------------------------------------------------

library(tidyverse)
library(skimr)
library(janitor)
library(lubridate)
library(rworldmap)
library(kernlab)
library(gridExtra)
library(ggdendro)
library(magrittr)
library(corrplot)
library(usmap)


# Set Seed ----------------------------------------------------------------

set.seed(92541445)

# Load Data ---------------------------------------------------------------

# 5/9/20 Daily report:
daily_report <- read_csv("data/unprocessed/csse_covid_19_data/csse_covid_19_daily_reports/05-09-2020.csv") %>%
  clean_names()

# Time series:
confirmed_ts <- read_csv("data/unprocessed/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>%
  clean_names()

deaths_ts <- read_csv("data/unprocessed/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") %>%
  clean_names()

recovered_ts <- read_csv("data/unprocessed/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv") %>%
  clean_names()

# Missing data:
daily_report %>% summarise_all(funs(mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = 1:12, names_to = "var", values_to = "percent_missing") %>%
  ggplot(aes(percent_missing, var)) +
  geom_bar(stat = "identity", aes(fill = var)) +
  labs(title = "Missing data: Daily Report")

confirmed_ts %>% summarise_all(funs(mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = 1:113, names_to = "var", values_to = "percent_missing") %>%
  filter(percent_missing > 0) %>% # There are a lot of variables, so I'm only choosing the ones with any missing data
  ggplot(aes(percent_missing, var)) +
  geom_bar(stat = "identity", aes(fill = var)) +
  labs(title = "Missing data: Confirmed Cases Time Series")

deaths_ts %>% summarise_all(funs(mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = 1:113, names_to = "var", values_to = "percent_missing") %>%
  filter(percent_missing > 0) %>% # There are a lot of variables, so I'm only choosing the ones with any missing data
  ggplot(aes(percent_missing, var)) +
  geom_bar(stat = "identity", aes(fill = var)) +
  labs(title = "Missing data: Deaths Time Series")

recovered_ts %>% summarise_all(funs(mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = 1:113, names_to = "var", values_to = "percent_missing") %>%
  filter(percent_missing > 0) %>% # There are a lot of variables, so I'm only choosing the ones with any missing data
  ggplot(aes(percent_missing, var)) +
  geom_bar(stat = "identity", aes(fill = var)) +
  labs(title = "Missing data: Recovered Time Series")


# Analysis of Response Variables ------------------------------------------


# Response variables - daily reports:

daily_report %>% # Total confirmed cases
  group_by(country_region) %>%
  summarise(total_confirmed = sum(confirmed)) %>%
  ggplot(aes(total_confirmed)) +
  geom_histogram() +
  labs(title = "Distribution of Total Confirmed Cases by Country")

daily_report %>% # Total deaths
  group_by(country_region) %>%
  summarise(total_deaths = sum(deaths)) %>%
  ggplot(aes(total_deaths)) +
  geom_histogram() +
  labs(title = "Distribution of Total Deaths by Country")

daily_report %>% # Total recoveries
  group_by(country_region) %>%
  summarise(total_recovered = sum(recovered)) %>%
  ggplot(aes(total_recovered)) +
  geom_histogram() +
  labs(title = "Distribution of Total Recoveries by Country")

daily_report %>% # All Response
  group_by(country_region) %>%
  summarise(total_confirmed = sum(confirmed),
            total_deaths = sum(deaths),
            total_recovered = sum(recovered)) %>%
  pivot_longer(cols = c(2:4), names_to = "var") %>%
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~var) +
  labs(title = "Distribution of Total Confirmed Cases, Total Deaths, and Total Recoeries by country - Linear Scale")

# Log response variables

daily_report %>% # All response variables on a log-10 scale
  group_by(country_region) %>%
  summarise(log_confirmed = log10(sum(confirmed)),
            log_deaths = log10(sum(deaths)),
            log_recovered = log10(sum(recovered))) %>%
  pivot_longer(cols = 2:4, names_to = "var") %>%
  ggplot(aes((value))) +
  geom_freqpoly(aes(color = var)) +
  labs(title = "Distribution of Confirmed Cases, Deaths, and Recoveries by country - Logarithmic")

# Time series:
confirmed_ts %>% # Total cases over time
  select(-province_state, -lat, -long) %>%
  pivot_longer(cols = c(2:110), names_to = "date", values_to = "cases") %>%
  mutate(date = as_date(date, format = "x%m_%d_%y")) %>% # Convert date to a date variable
  group_by(date) %>%
  summarise(total_cases = sum(cases)) %>%
  ggplot(aes(date, total_cases)) +
  geom_line()

deaths_ts %>% # Total cases over time
  select(-province_state, -lat, -long) %>%
  pivot_longer(cols = c(2:110), names_to = "date", values_to = "deaths") %>%
  mutate(date = as_date(date, format = "x%m_%d_%y")) %>% # Convert date to a date variable
  group_by(date) %>%
  summarise(total_deaths = sum(deaths)) %>%
  ggplot(aes(date, total_deaths)) +
  geom_line()

recovered_ts %>% # Total recovered over time
  select(-province_state, -lat, -long) %>%
  pivot_longer(cols = c(2:110), names_to = "date", values_to = "recovered") %>%
  mutate(date = as_date(date, format = "x%m_%d_%y")) %>% # Convert date to a date variable
  group_by(date) %>%
  summarise(total_recovered = sum(recovered)) %>%
  ggplot(aes(date, total_recovered)) +
  geom_line()

# Putting all time series info into one graphic:
confirmed_all_countries <- confirmed_ts %>% # Total cases over time
  select(-province_state, -lat, -long) %>%
  pivot_longer(cols = c(2:110), names_to = "date", values_to = "cases") %>%
  mutate(date = as_date(date, format = "x%m_%d_%y")) %>% # Convert date to a date variable
  group_by(date) %>%
  summarise(total_cases = sum(cases))

deaths_all_countries <- deaths_ts %>% # Total deaths over time
  select(-province_state, -lat, -long) %>%
  pivot_longer(cols = c(2:110), names_to = "date", values_to = "deaths") %>%
  mutate(date = as_date(date, format = "x%m_%d_%y")) %>% # Convert date to a date variable
  group_by(date) %>%
  summarise(total_deaths = sum(deaths))

recovered_all_countries <- recovered_ts %>% # Total recovered over time
  select(-province_state, -lat, -long) %>%
  pivot_longer(cols = c(2:110), names_to = "date", values_to = "recovered") %>%
  mutate(date = as_date(date, format = "x%m_%d_%y")) %>% # Convert date to a date variable
  group_by(date) %>%
  summarise(total_recovered = sum(recovered))

worldwide_ts <- confirmed_all_countries %>%
  left_join(deaths_all_countries, by = "date") %>%
  left_join(recovered_all_countries, by = "date")

worldwide_ts %>% pivot_longer(cols = 2:4, names_to = "type") %>% # Linear scale
  ggplot(aes(date, value)) +
  geom_line(aes(color = type)) +
  labs(title = "Worldwide cases, deaths, and recoveries over time - Linear")

worldwide_ts %>% pivot_longer(cols = 2:4, names_to = "type") %>% # Log scale
  ggplot(aes(date, log10(value))) +
  geom_line(aes(color = type)) +
  labs(title = "Worldwide cases, deaths and recoveries over time - Logarithmic")

# Time - series for top 10 countries by confirmed cases
top10 <- daily_report %>% group_by(country_region) %>% # 10 countries with most cases
  summarise(total_cases = sum(confirmed)) %>%
  arrange(desc(total_cases)) %>%
  mutate(rank = min_rank(-total_cases)) %>%
  filter(rank <= 10) %>%
  pull(country_region)

confirmed_ts %>% # Total cases over time - top 10 countries - linear scale
  select(-province_state, -lat, -long) %>%
  pivot_longer(cols = c(2:110), names_to = "date", values_to = "cases") %>%
  mutate(date = as_date(date, format = "x%m_%d_%y")) %>% # Convert date to a date variable
  filter(country_region %in% top10) %>%
  group_by(country_region, date) %>%
  summarise(total_cases = sum(cases)) %>%
  ggplot(aes(date, total_cases)) +
  geom_line(aes(color = country_region)) +
  labs(title = "10 Countries with most cases - linear time series")
  

confirmed_ts %>% # Total cases over time - top 10 countries - log scale
  select(-province_state, -lat, -long) %>%
  pivot_longer(cols = c(2:110), names_to = "date", values_to = "cases") %>%
  mutate(date = as_date(date, format = "x%m_%d_%y")) %>% # Convert date to a date variable
  filter(country_region %in% top10) %>%
  group_by(country_region, date) %>%
  summarise(total_cases = sum(cases)) %>%
  ggplot(aes(date, log10(total_cases))) +
  geom_line(aes(color = country_region)) +
  labs(title = "10 countries with most cases - logarithmic time series")

# US Focused analysis
covid_us_report <- read_csv("data/unprocessed/csse_covid_19_data/csse_covid_19_daily_reports_us/05-09-2020.csv") %>%
  clean_names()

us_confirmed_ts <- read_csv("data/unprocessed/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") %>%
  clean_names()

us_deaths_ts <- read_csv("data/unprocessed/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv") %>%
  clean_names()

# Cases, fatalities
us_confirmed_ts <- us_confirmed_ts %>% pivot_longer(cols = 12:120, names_to = "date", values_to = "confirmed") %>%
  mutate(date = as_date(date, format = "x%m_%d_%y")) # Convert to a more usable format

us_deaths_ts <- us_deaths_ts %>% pivot_longer(cols = 13:121, names_to = "date", values_to = "deaths") %>%
  mutate(date = as_date(date, format = "x%m_%d_%y"))

us_ts <- us_confirmed_ts %>%
  group_by(date) %>%
  summarise(total_cases = sum(confirmed)) %>% left_join(
    us_deaths_ts %>%
      
      group_by(date) %>%
      
      summarise(total_deaths = sum(deaths)),
    
    by = "date")

# Plot cumulative cases and deaths:
us_ts %>% # Linear
  pivot_longer(cols = 2:3, names_to = "report", values_to = "num") %>%
  ggplot(aes(date, num)) +
  geom_line(aes(color = report)) +
  labs(title = "US Cases and Deaths - Linear")

us_ts %>% # Log
  pivot_longer(cols = 2:3, names_to = "report", values_to = "num") %>%
  ggplot(aes(date, log10(num))) +
  geom_line(aes(color = report)) +
  labs(title = "US Cases and Deaths - Logarithmic")

# Cases by state:

us_confirmed_ts %>% # Linear
  group_by(date, province_state) %>%
  summarise(total_cases = sum(confirmed)) %>%
  ggplot(aes(date, total_cases)) +
  geom_line(aes(color = province_state))

us_confirmed_ts %>% # Log
  group_by(date, province_state) %>%
  summarise(total_cases = sum(confirmed)) %>%
  ggplot(aes(date, log10(total_cases))) +
  geom_line(aes(color = province_state))


# Creating a percent change variable: -------------------------------------

# Calculates percentage change in deaths for a given date
percent_change_deaths <- function(data){
  data %>%
    group_by(date) %>%
    summarise(deaths = sum(deaths)) %>%
    mutate(pct_change = (deaths - lag(deaths)) / lag(deaths) * 100)
}

# Run percent change function for all states
percent_change_death <- us_deaths_ts %>% group_nest(province_state) %>% 
  mutate(change = map(data, percent_change_deaths)) %>%
  select(province_state, change) %>%
  unnest(cols = c(change)) 

# Change all non-number values to NA
percent_change_death <- percent_change_death %>% 
  mutate(pct_change = ifelse(is.nan(pct_change), NA, pct_change)) %>%
  mutate(pct_change = ifelse(pct_change == Inf, NA, pct_change)) 

# Create variable for earliest death, 30 days after 
deaths_dates <- percent_change_death %>%
  filter(deaths > 0) %>%
  group_by(province_state) %>%
  summarise(first_death = min(date),
            day_30 = first_death + days(30))

# Use earliest death and 30 day period to calculate 30 day avg rate of change:
first_30 <- percent_change_death %>%
  left_join(deaths_dates, by = "province_state") %>%
  filter(date > first_death, date <= day_30) %>%
  group_by(province_state) %>%
  summarise(first30 = mean(pct_change, na.rm = TRUE)) %>%
  arrange(desc(first30))

# Average growth in deaths in past 10 days:
past_10 <- percent_change_death %>%
  filter(date >= as_date("05-01-20", format = "%m-%d-%y")) %>%
  group_by(province_state) %>%
  summarise(past10 = mean(pct_change, na.rm = TRUE)) %>%
  mutate(past10 = ifelse(is.nan(past10), 0, past10)) %>%
  arrange(desc(past10))

# Overall mean growth
overall_change <- percent_change_death %>%
  group_by(province_state) %>%
  summarise(mean_change = mean(pct_change, na.rm = TRUE)) %>%
  mutate(mean_change = ifelse(is.nan(mean_change), 0, mean_change)) %>%
  arrange(desc(mean_change))

# Add summary percent change statistics to the main dataset:
covid_us_report <- covid_us_report %>% 
  left_join(first_30, by = "province_state") %>%
  left_join(past_10, by = "province_state")

# Analysis of Predictor Variables -----------------------------------------

# Get US Data From ACS:


# Population Data ---------------------------------------------------------


acs_population <- read_csv("data/unprocessed/ACS/ACS_Total_Population_-_State.csv") %>%
  clean_names()

covid_data_pop <- covid_us_report %>%
  filter(province_state %in% acs_population$name) %>%
  rename(name = province_state) %>%
  left_join(acs_population, by = "name")

pop_corr <- covid_data_pop %>%
  select(confirmed, deaths, incident_rate, people_tested, people_hospitalized, mortality_rate,
         testing_rate, hospitalization_rate, first30, past10, 25:140) %>%
  cor() %>%
  as_tibble()

pop_corr %>% # Find variables correlated with incident rate
  mutate(names = names(pop_corr)) %>%
  select(names, everything()) %>%
  select(names, incident_rate, mortality_rate) %>%
  arrange(desc(abs(incident_rate)))

pop_corr %>% # Find variables correlated with mortality rate
  mutate(names = names(pop_corr)) %>%
  select(names, everything()) %>%
  select(names, incident_rate, mortality_rate) %>%
  arrange(desc(abs(mortality_rate))) 

pop_corr %>% # Find variables correlated with mean growth in deaths in first month
  mutate(names = names(pop_corr)) %>%
  select(names, everything()) %>%
  select(names, first30, past10) %>%
  arrange(desc(abs(first30))) %>%

pop_corr %>% # Find variables correlated with mean growth in deaths in first month
  mutate(names = names(pop_corr)) %>%
  select(names, everything()) %>%
  select(names, first30, past10) %>%
  arrange(desc(abs(past10))) 

covid_data_pop %>% # Estimate of total female population over age 85 vs first 30 days change in deaths
  ggplot(aes(b01001_049e, first30)) +
  geom_point()

covid_data_pop %>%
  ggplot(aes(b01001_018e, first30)) +
  geom_point()

# Store most correlated variables:
covid_all_vars <- covid_data_pop %>%
  select(1:22, b01001_049e, b01001_042e, b01001_041e, b01001_040e, b01001_018e) 





# Internet Connectivity Data ----------------------------------------------

acs_internet <- read_csv("data/unprocessed/ACS/ACS_Internet_connectivity_-_State.csv") %>%
  clean_names()

covid_data_internet <- covid_us_report %>%
  filter(province_state %in% acs_internet$name) %>%
  rename(name = province_state) %>%
  left_join(acs_internet, by = "name")

internet_corr <- covid_data_internet %>%
  select(confirmed, deaths, incident_rate, people_tested, people_hospitalized, mortality_rate,
         testing_rate, hospitalization_rate, first30, past10, 25:78) %>%
  cor() %>%
  as_tibble()

internet_corr %>%
  mutate(names = names(internet_corr)) %>%
  select(names, everything()) %>%
  select(names, incident_rate, mortality_rate) %>%
  arrange(desc(abs(incident_rate)))

internet_corr %>%
  mutate(names = names(internet_corr)) %>%
  select(names, everything()) %>%
  select(names, incident_rate, mortality_rate) %>%
  arrange(desc(abs(mortality_rate)))

internet_corr %>%
  mutate(names = names(internet_corr)) %>%
  select(names, everything()) %>%
  select(names, first30, past10) %>%
  arrange(desc(abs(first30))) %>% View()

internet_corr %>%
  mutate(names = names(internet_corr)) %>%
  select(names, everything()) %>%
  select(names, first30, past10) %>%
  arrange(desc(abs(past10)))

covid_all_vars <- covid_all_vars %>% bind_cols(
  covid_data_internet %>%
    select(b28001_008e, b28001_011e, b28001_004e, b28002_008e, b28001_010e))

# Income Data -------------------------------------------------------------

acs_income <- read_csv("data/ACS/unprocessed/ACS_Household_Income_Distribution_-_State.csv") %>%
  clean_names()

covid_data_income <- covid_us_report %>%
  filter(province_state %in% acs_income$name) %>%
  rename(name = province_state) %>%
  left_join(acs_income, by = "name")

income_corr <- covid_data_income %>%
  select(confirmed, deaths, incident_rate, people_tested, people_hospitalized, mortality_rate,
         testing_rate, hospitalization_rate, first30, past10, 25:72) %>%
  cor() %>%
  as_tibble()

income_corr %>%
  mutate(names = names(income_corr)) %>%
  select(names, everything()) %>%
  select(names, incident_rate, mortality_rate) %>%
  arrange(desc(abs(incident_rate)))
income_corr %>%
  mutate(names = names(income_corr)) %>%
  select(names, everything()) %>%
  select(names, incident_rate, mortality_rate) %>%
  arrange(desc(abs(mortality_rate)))

income_corr %>%
  mutate(names = names(income_corr)) %>%
  select(names, everything()) %>%
  select(names, first30, past10) %>%
  arrange(desc(abs(first30))) %>%

income_corr %>%
  mutate(names = names(income_corr)) %>%
  select(names, everything()) %>%
  select(names, first30, past10) %>%
  arrange(desc(abs(past10))) 

covid_all_vars <- covid_all_vars %>% bind_cols(
  covid_data_income %>%
    select(b19001_015e, b19001_002e, b19001_014e, b19001_016e, b19001_calc_num_ge100e))


# Insurance Data ----------------------------------------------------------

acs_insurance <- read_csv("data/ACS/unprocessed/ACS_Health_Insurance_Coverage_-_State.csv") %>%
  clean_names()

covid_data_insurance <- covid_us_report %>%
  filter(province_state %in% acs_income$name) %>%
  rename(name = province_state) %>%
  left_join(acs_insurance, by = "name")

insurance_corr <- covid_data_insurance %>%
  select(confirmed, deaths, incident_rate, people_tested, people_hospitalized, mortality_rate,
         testing_rate, hospitalization_rate, first30, past10, 25:93) %>%
  cor() %>%
  as_tibble()

insurance_corr %>%
  mutate(names = names(insurance_corr)) %>%
  select(names, everything()) %>%
  select(names, incident_rate, mortality_rate) %>%
  arrange(desc(abs(incident_rate)))
insurance_corr %>%
  mutate(names = names(insurance_corr)) %>%
  select(names, everything()) %>%
  select(names, incident_rate, mortality_rate) %>%
  arrange(desc(abs(mortality_rate)))

insurance_corr %>%
  mutate(names = names(insurance_corr)) %>%
  select(names, everything()) %>%
  select(names, first30, past10) %>%
  arrange(desc(abs(first30))) 

insurance_corr %>%
  mutate(names = names(insurance_corr)) %>%
  select(names, everything()) %>%
  select(names, first30, past10) %>%
  arrange(desc(abs(past10)))

covid_all_vars <- covid_all_vars %>% bind_cols(
  covid_data_insurance %>%
    select(b27010_036e, b27010_042e, b27010_010e, b27010_026e,
           b27010_004e))


# State Population Data ---------------------------------------------------

state_pop <- read_csv("data/unprocessed/state_population.csv") %>%
  clean_names() %>%
  rename(name = state)

covid_data_pop2 <- covid_us_report %>%
  filter(province_state %in% state_pop$name) %>%
  rename(name = province_state) %>%
  left_join(state_pop, by = "name")

pop2_corr <- covid_data_pop2 %>%
  select(confirmed, deaths, incident_rate, people_tested, people_hospitalized, mortality_rate,
         testing_rate, hospitalization_rate, first30, past10, 22:28) %>%
  cor() %>%
  as_tibble()

pop2_corr %>%
  mutate(names = names(pop2_corr)) %>%
  select(names, everything()) %>%
  select(names, incident_rate, mortality_rate) %>%
  arrange(desc(abs(incident_rate)))

pop2_corr %>%
  mutate(names = names(pop2_corr)) %>%
  select(names, everything()) %>%
  select(names, incident_rate, mortality_rate) %>%
  arrange(desc(abs(mortality_rate)))

pop2_corr %>%
  mutate(names = names(pop2_corr)) %>%
  select(names, everything()) %>%
  select(names, first30, past10) %>%
  arrange(desc(abs(first30))) 

pop2_corr %>%
  mutate(names = names(pop2_corr)) %>%
  select(names, everything()) %>%
  select(names, first30, past10) %>%
  arrange(desc(abs(past10)))

covid_all_vars <- covid_all_vars %>%
  bind_cols(covid_data_pop2 %>% select(pop))

# Change all variables into a percentage of population
covid_all_vars <- covid_all_vars %>% mutate_at(23:42, funs(./pop))

# Looking at all saved variables: -----------------------------------------

covid_all_vars %>%
  select(first30, past10, 23:42) %>%
  cor() %>%
  corrplot()


# Clustering --------------------------------------------------------------

# Helper Functions --------------------------------------------------------

# run_hclust: runs hierarchical clustering with the given method
# args: x - data used for clustering; meth - method of clustering
run_hclust <- function(x, meth){
  return(hclust(dist(x), method = meth)) # hclust runs the clustering algorithm.
}

# cut_hclust: gives desired number of clusters for an hclust() object
# args: hclust_obj - an hclust() object; ncuts - number of clusters desired
cut_hclust <- function(hclust_obj, ncuts){
  return(cutree(hclust_obj, ncuts))
}

# get_within_ss: Get within-cluster SS from a K-means object
# args: kmean_obj - a K-means obj
get_within_ss <- function(kmean_obj){
  return(kmean_obj$tot.withinss)
}

# get_cluster: get cluster labels for data
# args: x - data; clust_obj - a cluster object
get_cluster <- function(x, clust_obj){
  
  if(class(clust_obj) == "kmeans"){
    clust = clust_obj$cluster
  }
  else{
    clust = clust_obj
  }
  
  out = x %>%
    
    mutate(cluster = clust)
  
  return(out)
}


# Hierarchical Clustering -------------------------------------------------


covid_cluster_data <- covid_all_vars %>%
  select(name, 23:42)

scaled_cluster_data <- covid_cluster_data %>%
  select(2:21) %>%
  as.matrix() %>%
  scale() %>%
  as_tibble()

covid_cluster_data_scaled <- 
  covid_cluster_data %>%
  select(name) %>%
  bind_cols(scaled_cluster_data)

covid_hclust <- tibble(
  dat = covid_cluster_data_scaled %>% list()
)

covid_hclust <- covid_hclust %>%
  mutate(
    hcl = map2(dat, "complete", run_hclust), # Create clusters
    dendo = map(hcl, ggdendrogram), # Create dendrogram,
  )


covid_hclust <- covid_hclust %>%
  crossing(ncuts = c(2:6)) %>%
  mutate(
    clusters = map2(hcl, ncuts, cut_hclust),
    clust_dat = map2(dat, clusters, get_cluster)
  )

covid_hclust_3cut <- covid_hclust %>%
  filter(ncuts == 3) %>%
  select(clust_dat) %>%
  unnest(cols = c(clust_dat)) %>%
  rename(state = name) %>%
  mutate(cluster = as_factor(cluster))

# Plot 3 cluster cut on map:
plot_usmap(data = covid_hclust_3cut, values = "cluster", color = "white") +
  scale_fill_discrete(name = "Cluster", na.translate = FALSE)

covid_hclust_4cut <- covid_hclust %>%
  filter(ncuts == 4) %>%
  select(clust_dat) %>%
  unnest(cols = c(clust_dat)) %>%
  rename(state = name) %>%
  mutate(cluster = as_factor(cluster))

# Plot 4 cluster cut on map:
plot_usmap(data = covid_hclust_4cut, values = "cluster", color = "white") +
  scale_fill_discrete(name = "Cluster", na.translate = FALSE)

covid_hclust_5cut <- covid_hclust %>%
  filter(ncuts == 5) %>%
  select(clust_dat) %>%
  unnest(cols = c(clust_dat)) %>%
  rename(state = name) %>%
  mutate(cluster = as_factor(cluster))

# Plot 5 cluster cut on map:
plot_usmap(data = covid_hclust_5cut, values = "cluster", color = "white") +
  scale_fill_discrete(name = "Cluster", na.translate = FALSE) +
  labs(title = "Hierarchical Clustering, 5 clusters")

covid_hclust_6cut <- covid_hclust %>%
  filter(ncuts == 6) %>%
  select(clust_dat) %>%
  unnest(cols = c(clust_dat)) %>%
  rename(state = name) %>%
  mutate(cluster = as_factor(cluster))

# Plot 6 cluster cut on map:
plot_usmap(data = covid_hclust_6cut, values = "cluster", color = "white") +
  scale_fill_discrete(name = "Cluster", na.translate = FALSE) +
  labs(title = "Hierarchical Clustering, 6 clusters")


# K-Means Clustering ------------------------------------------------------

covid_kmeans <- tibble(xmat = list(covid_cluster_data_scaled %>% select(-name))) %>%
  crossing(nclust = 2:6)

covid_kmeans <- covid_kmeans %>% 
  mutate(
    kmean = map2(xmat, nclust, kmeans, nstart = 20),
    within_ss = map_dbl(kmean, get_within_ss),
    clusters = map2(xmat, kmean, get_cluster)
  )

# Plot clusters:
covid_kmeans_3clust <- covid_kmeans %>%
  filter(nclust == 3) %>%
  select(clusters) %>%
  unnest(cols = c(clusters))

covid_kmeans_3clust <- covid_kmeans_3clust %>%
  bind_cols(covid_cluster_data_scaled %>% select(name)) %>%
  select(name, everything()) %>%
  rename(state = name) %>%
  mutate(cluster = as_factor(cluster))

# 3 Clusters
plot_usmap(data = covid_kmeans_3clust, values = "cluster", color = "white") +
  scale_fill_discrete(name = "Cluster", na.translate = FALSE)

covid_kmeans_4clust <- covid_kmeans %>%
  filter(nclust == 4) %>%
  select(clusters) %>%
  unnest(cols = c(clusters))

covid_kmeans_4clust <- covid_kmeans_4clust %>%
  bind_cols(covid_cluster_data_scaled %>% select(name)) %>%
  select(name, everything()) %>%
  rename(state = name) %>%
  mutate(cluster = as_factor(cluster))

# 4 Clusters
plot_usmap(data = covid_kmeans_4clust, values = "cluster", color = "white") +
  scale_fill_discrete(name = "Cluster", na.translate = FALSE)

covid_kmeans_5clust <- covid_kmeans %>%
  filter(nclust == 5) %>%
  select(clusters) %>%
  unnest(cols = c(clusters))

covid_kmeans_5clust <- covid_kmeans_5clust %>%
  bind_cols(covid_cluster_data_scaled %>% select(name)) %>%
  select(name, everything()) %>%
  rename(state = name) %>%
  mutate(cluster = as_factor(cluster))

# 5 Clusters
plot_usmap(data = covid_kmeans_5clust, values = "cluster", color = "white") +
  scale_fill_discrete(name = "Cluster", na.translate = FALSE) +
  labs(title = "K-Means Clustering, 5 clusters")

covid_kmeans_6clust <- covid_kmeans %>%
  filter(nclust == 6) %>%
  select(clusters) %>%
  unnest(cols = c(clusters))

covid_kmeans_6clust <- covid_kmeans_6clust %>%
  bind_cols(covid_cluster_data_scaled %>% select(name)) %>%
  select(name, everything()) %>%
  rename(state = name) %>%
  mutate(cluster = as_factor(cluster))

# 6 Clusters
plot_usmap(data = covid_kmeans_6clust, values = "cluster", color = "white") +
  scale_fill_discrete(name = "Cluster", na.translate = FALSE)


# Spectral Clustering -----------------------------------------------------

covid_spectral <- tibble(data = list(covid_cluster_data_scaled %>% select(-name)))

# Create spectral clusters--nclus = 5
covid_spectral <- covid_spectral %>% mutate(
  spec = map(.x = data,
             .f = function(x) specc(as.matrix(x), centers = 5)),
  spec_data = map2(data, spec, get_cluster)
)

covid_spectral_data <- covid_spectral %>%
  select(spec_data) %>%
  unnest(cols = c(spec_data)) %>%
  bind_cols(covid_cluster_data_scaled %>% select(name)) %>%
  select(name, everything()) %>%
  rename(state = name) %>%
  mutate(cluster = as_factor(as.character(cluster)))

plot_usmap(data = covid_spectral_data, values = "cluster", color = "white") +
  scale_fill_discrete(name = "Cluster", na.translate = FALSE) +
  labs(title = "Spectral Clustering, 5 clusters")


# Analysis of clusters vs COVID-related statistics ------------------------

# Hierarchical
covid_hclust_5cut %>% select(state, cluster) %>%
  rename(province_state = state) %>%
  left_join(covid_us_report, by = "province_state") %>%
  ggplot(aes(log10(confirmed), log10(deaths))) +
  geom_point(aes(color = cluster))

covid_hclust_5cut %>% select(state, cluster) %>%
  rename(province_state = state) %>%
  left_join(covid_us_report, by = "province_state") %>%
  ggplot(aes(first30, past10)) +
  geom_point(aes(color = cluster))

covid_hclust_5cut %>% select(state, cluster) %>%
  rename(province_state = state) %>%
  left_join(covid_us_report, by = "province_state") %>%
  ggplot(aes(incident_rate, mortality_rate)) +
  geom_point(aes(color = cluster))  



# K-Means
covid_kmeans_5clust %>% select(state, cluster) %>%
  rename(province_state = state) %>%
  left_join(covid_us_report, by = "province_state") %>%
  ggplot(aes(log10(confirmed), log10(deaths))) +
  geom_point(aes(color = cluster)) +
  labs(title = "confirmed cases vs deaths by cluster, K-Means")

covid_kmeans_5clust %>% select(state, cluster) %>%
  rename(province_state = state) %>%
  left_join(covid_us_report, by = "province_state") %>%
  ggplot(aes(first30, past10)) +
  geom_point(aes(color = cluster)) +
  labs(title = "first 30 vs past 10 average rates by cluster, K-Means")

covid_kmeans_5clust %>% select(state, cluster) %>%
  rename(province_state = state) %>%
  left_join(covid_us_report, by = "province_state") %>%
  ggplot(aes(incident_rate, mortality_rate)) +
  geom_point(aes(color = cluster)) +
  labs(title = "incident_rate vs mortality rates by cluster, K-Means")

covid_kmeans_5clust %>% select(state, cluster) %>%
  rename(province_state = state) %>%
  left_join(covid_us_report, by = "province_state") %>%
  ggplot(aes(incident_rate, cluster)) +
  geom_boxplot() +
  labs(title = "incident rate by cluster, K-Means")

covid_kmeans_5clust %>% select(state, cluster) %>%
  rename(province_state = state) %>%
  left_join(covid_us_report, by = "province_state") %>%
  ggplot(aes(past10, cluster)) +
  geom_boxplot() +
  labs(title = "past10 by cluster, K-Means")

covid_kmeans_5clust %>% select(state, cluster) %>%
  rename(province_state = state) %>%
  left_join(covid_us_report, by = "province_state") %>%
  ggplot(aes(first30, cluster)) +
  geom_boxplot() +
  labs(title = "first30 by cluster, K-Means")

# Spectral
covid_spectral_data %>% select(state, cluster) %>%
  rename(province_state = state) %>%
  left_join(covid_us_report, by = "province_state") %>%
  ggplot(aes(log10(confirmed), log10(deaths))) +
  geom_point(aes(color = cluster)) +
  labs(title = "confirmed cases vs deaths by cluster, spectral")

covid_spectral_data %>% select(state, cluster) %>%
  rename(province_state = state) %>%
  left_join(covid_us_report, by = "province_state") %>%
  ggplot(aes(first30, past10)) +
  geom_point(aes(color = cluster)) +
  labs(title = "first30 vs past10 by cluster, spectral")

covid_spectral_data %>% select(state, cluster) %>%
  rename(province_state = state) %>%
  left_join(covid_us_report, by = "province_state") %>%
  ggplot(aes(incident_rate, cluster)) +
  geom_boxplot() +
  labs(title = "incident rate vs mortality rate by cluster, spectral")

covid_spectral_data %>% select(state, cluster) %>%
  rename(province_state = state) %>%
  left_join(covid_us_report, by = "province_state") %>%
  ggplot(aes(past10, cluster)) +
  geom_boxplot() +
  labs(title = "past10 by cluster, spectral")

covid_spectral_data %>% select(state, cluster) %>%
  rename(province_state = state) %>%
  left_join(covid_us_report, by = "province_state") %>%
  ggplot(aes(first30, cluster)) +
  geom_boxplot() +
  labs(title = "first30 by cluster, spectral")


# Looking at Lockdown Data ------------------------------------------------

mobility <- read_csv("data/unprocessed/global_mobility_report.csv", col_types = list(
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
  col_double()
)) %>%
  clean_names()

mobility_by_state <- mobility %>%
  filter(country_region == "United States", is.na(sub_region_2)) %>%
  group_by(sub_region_1, date) %>%
  summarise(retail_rec = mean(retail_and_recreation_percent_change_from_baseline, na.rm = TRUE),
            grocery_pharmacy = mean(grocery_and_pharmacy_percent_change_from_baseline, na.rm = TRUE),
            parks = mean(parks_percent_change_from_baseline, na.rm = TRUE),
            transit = mean(transit_stations_percent_change_from_baseline, na.rm = TRUE),
            workplace = mean(workplaces_percent_change_from_baseline, na.rm = TRUE), 
            residential = mean(residential_percent_change_from_baseline, na.rm = TRUE)) %>%
  rename(province_state = sub_region_1)

mobility_us <- mobility %>%
  filter(country_region == "United States", is.na(sub_region_1))

# Overall trend for US:
mobility_us %>%
  pivot_longer(cols = 6:11, names_to = "cat") %>%
  ggplot(aes(date, value)) +
  geom_line(aes(color = cat))

# Analysis by state:
mobility_analysis_states <- percent_change_death %>%
  left_join(mobility_by_state, by = c("province_state", "date"))

# Look at some states:
mobility_analysis_states %>% # Washington
  filter(province_state == "Washington") %>%
  pivot_longer(cols = c(pct_change, transit, residential), names_to = "var") %>%
  ggplot(aes(date, value)) +
  geom_line(aes(color = var)) +
  xlim(as_date("13-02-20", format = "%d-%m-%y"), as_date("07-05-20", format = "%d-%m-%y")) +
  ylim(-80, 50)

mobility_analysis_states %>% # New York
  filter(province_state == "New York") %>%
  pivot_longer(cols = c(pct_change, transit, residential), names_to = "var") %>%
  ggplot(aes(date, value)) +
  geom_line(aes(color = var)) +
  xlim(as_date("13-02-20", format = "%d-%m-%y"), as_date("07-05-20", format = "%d-%m-%y")) +
  ylim(-80, 50)

mobility_analysis_states %>% # Mississippi
  filter(province_state == "Mississippi") %>%
  pivot_longer(cols = c(pct_change, transit, residential), names_to = "var") %>%
  ggplot(aes(date, value)) +
  geom_line(aes(color = var)) +
  xlim(as_date("13-02-20", format = "%d-%m-%y"), as_date("07-05-20", format = "%d-%m-%y")) +
  ylim(-80, 50)

mobility_analysis_states %>% # Illinois
  filter(province_state == "Illinois") %>%
  pivot_longer(cols = c(pct_change, transit, residential), names_to = "var") %>%
  ggplot(aes(date, value)) +
  geom_line(aes(color = var)) +
  xlim(as_date("13-02-20", format = "%d-%m-%y"), as_date("07-05-20", format = "%d-%m-%y")) +
  ylim(-80, 50)


