# Random Forests ----------------------------------------------------------


# Load Packages -----------------------------------------------------------

library(tidyverse)
library(janitor)
library(modelr)
library(rsample)
library(ranger)
library(vip)
library(pdp)

# Set Seed ----------------------------------------------------------------

set.seed(9254853)

# Load Data ---------------------------------------------------------------

data <- read_csv("data/processed/modelling_data.csv") %>%
  clean_names()

# Cut off early dates for which data is not fully available, and filter out states/territories w/ missing data:
data <- data %>% filter(date >= "2020-03-07") %>%
  filter(!(state %in% c("Virgin Islands",
                        "Northern Mariana Islands",
                        "American Samoa",
                        "Diamond Princess",
                        "Grand Princess",
                        "Guam",
                        "Puerto Rico"))) %>%
  mutate(weekday = as.factor(weekday))

data <- data %>%
  select(-driving, -driving_lag14, -driving_lag21, -rollmean_driving, -(20:32), -first_death)

train <- data %>%
  filter(date <= "2020-05-18")

test <- data %>%
  filter(date > "2020-05-18")

# RF ----------------------------------------------------------------------

# helper function to get misclass rate from a ranger object
#' @name mse_ranger
#' @param model ranger object, a fitted random forest
#' @param test tibble/resample object, a test set
#' @param outcome string, indicates the outcome variable in the data
#' @returns MSE of the model on the test set
mse_ranger <- function(model, test, outcome){
  
  # Check if test is a tibble
  if(!is_tibble(test)){
    test <- test %>% as_tibble()
  }
  
  # Make predictions
  preds <- predict(model, test)$predictions
  
  # Compute MSE
  mse <- mean((test[[outcome]] - preds)^2)
  
  return(mse)
}

# Use cross-validation to tune the model:
rf_cv <- train %>% crossv_kfold(k = 5) %>%
  mutate(train = map(train, as_tibble),
         test = map(test, as_tibble)) %>%
  crossing(mtry = seq(1, ncol(train) - 4, length.out = 10),
           num.trees = seq(200, 1000, length.out = 10))


# Do not run this section of code -----------------------------------------
# Cross validation uses 13GB memory - **DO NOT RUN!** - The results are already saved and loaded below.
rf_cv <- rf_cv %>% mutate(
  xg_model = pmap(.l = list(x = train, y = mtry, z = num.trees), 
                  .f = function(x, y, z) ranger(change ~ . - state - date - total_deaths,
                                                mtry = y,
                                                data = x,
                                                num.trees = z,
                                                splitrule = "variance",
                                                importance = "impurity",
                                                verbose = TRUE))
)

rf_cv %>% write_rds("rf/rf_cv.rds")

rf_cv <- read_rds("rf/rf_cv.rds")

rf_cv <- rf_cv %>%
  rename(model = xg_model) %>% 
  mutate(
  train_err = map2(model, train, mse_ranger, outcome = "change"), 
  test_err = map2(model, test, mse_ranger, outcome = "change") 
)

rf_cv_err <- rf_cv %>%
  select(.id, mtry, num.trees, train_err, test_err) %>%
  mutate(
    train_err = unlist(train_err),
    test_err = unlist(test_err) 
  ) %>%
  arrange(test_err)

rf_cv_err %>% write_csv("data/rf_cv_err.csv")

# Run code below ----------------------------------------------------------

rf_cv_err <- read_csv("data/rf_cv_err.csv")

rf_cv_err %>% write_csv("outcomes/rf_cv_err.csv")

# find cv error means:
rf_cv_err <- rf_cv_err %>%
  group_by(mtry, num.trees) %>%
  summarise(
    mean_train_err = mean(train_err),
    mean_test_err = mean(test_err)
  ) %>%
  arrange(mean_test_err)

rf_cv_err %>% 
  ggplot(aes(mtry, num.trees)) +
  geom_tile(aes(fill = mean_test_err)) +
  labs(title = "Heatmap of tuning parameters: Darker shades indicate better test error!")

# Now build the final model based on CV:
rf_final <- ranger(formula = change ~ . - state - date - total_deaths,
                   data = train,
                   mtry = 7,
                   num.trees = 300,
                   splitrule = "variance",
                   importance = "impurity")

predict(rf_final, data = test[3, ])$predictions

# Function to generate predictions for test data:
generate_predictions <- function(data, init_deaths, model){
  current_deaths <- init_deaths
  # Create an empty vector the length of our data to store predicted values
  predicted_change <- rep(0, nrow(data))
  # Create an empty vector the length of our data to store the cumulative number of deaths
  predicted_total <- rep(0, nrow(data))
  for (i in 1:nrow(data)){
    pred <- predict(model, data[i, ])$predictions
    current_deaths <- current_deaths + pred
    predicted_change[i] <- pred
    predicted_total[i] <- current_deaths
    # Fill lag variables in rows further down based on the predicted value
    for (j in 6:19){
      if(i + (j - 5) <= 14)
        data[(i + j), j] <- pred
    }
  }
  tibble(
    pred_change = predicted_change,
    pred_total = predicted_total
  )
}

initial_deaths <- data %>%
  filter(date == "2020-05-18") %>%
  group_by(state) %>%
  summarise(init_deaths = mean(total_deaths))

predictions <- test %>%
  group_by(state, add = TRUE) %>%
  group_nest() %>%
  left_join(initial_deaths, by = "state") %>%
  mutate(
    predictions = map2(data, init_deaths, generate_predictions, model = rf_final)
  )

outcomes <- predictions %>%
  select(state, data, predictions) %>%
  unnest(cols = c(data, predictions)) %>%
  select(state, date, total_deaths, change, pred_change, pred_total) 

# Visualize the predictions
outcomes %>%
  group_by(date) %>%
  summarise(
    pred_change = sum(pred_change),
    pred_total = sum(pred_total),
    total = sum(total_deaths),
    change = sum(change)
  ) %>%
  pivot_longer(cols = c(pred_total, total), names_to = "var") %>%
  ggplot(aes(date, value)) +
  geom_line(aes(color = var)) +
  labs(title = "Predicted vs Actual Death Totals, National")

# Some individual states:
outcomes %>%
  filter(state == "New York") %>%
  pivot_longer(cols = c(pred_total, total_deaths), names_to = "var") %>%
  ggplot(aes(date, value)) +
  geom_line(aes(color = var)) +
  labs(title = "Predicted vs Actual Death Totals, New York")

outcomes %>%
  filter(state == "Illinois") %>%
  pivot_longer(cols = c(pred_total, total_deaths), names_to = "var") %>%
  ggplot(aes(date, value)) +
  geom_line(aes(color = var)) +
  labs(title = "Predicted vs Actual Death Totals, Illinois")

outcomes %>%
  filter(state == "Minnesota") %>%
  pivot_longer(cols = c(pred_total, total_deaths), names_to = "var") %>%
  ggplot(aes(date, value)) +
  geom_line(aes(color = var)) +
  labs(title = "Predicted vs Actual Death Totals, Minnesota")

outcomes %>%
  filter(state == "Montana") %>%
  pivot_longer(cols = c(pred_total, total_deaths), names_to = "var") %>%
  ggplot(aes(date, value)) +
  geom_line(aes(color = var)) +
  labs(title = "Predicted vs Actual Death Totals, Montana")

# MSE when letting the model chronologically generate predictions and updating lag variables with predictions
outcomes %>%
  summarise(mse = mean((change - pred_change)^2))

# MSE nationwide
outcomes %>%
  group_by(date) %>%
  summarise(pred_change = sum(pred_change),
            change = sum(change)) %>%
  summarise(mse = mean((change - pred_change)^2))

outcomes %>%
  write_csv("outcomes/rf.csv")

            