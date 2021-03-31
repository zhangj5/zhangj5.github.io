# Boosting Model ----------------------------------------------------------

# Load Packages -----------------------------------------------------------

library(tidyverse)
library(janitor)
library(modelr)
library(rsample)
library(xgboost)

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

# Dataframes that will be used for model - building:
train_dat <- train %>%
  select(-state, -date, -total_deaths)

test_dat <- train %>%
  select(-state, -date, -total_deaths)

# Helper Functions --------------------------------------------------------

# helper function
#' @name xgb_matrix
#' @param dat tibble, dataset
#' @param outcome string, indicates the outcome variable in the data
#' @param exclude_vars string (or list of strings) to indicate variables to exclude
#' @returns MSE of the model on the test set
xgb_matrix <- function(dat, outcome, exclude_vars){
  
  # Sanitize input: check that dat is a tibble
  if(!is_tibble(dat)){
    
    dat <- as_tibble(dat)
    
  }
  
  # Sanitize input: check that data has factors, not characters
  dat_types <- dat %>% map_chr(class)
  
  outcome_type <- class(dat[[outcome]])
  
  if("character" %in% dat_types){
    
    # If we need to re-code, leave that outside of the function
    print("You must encode characters as factors.")
    return(NULL)
    
  } else {
    
    # If we're doing binary outcomes, they need to be 0-1
    if(outcome_type == "factor" & nlevels(dat[[outcome]]) == 2){
      tmp <- dat %>% select(outcome) %>% onehot::onehot() %>% predict(dat)  
      lab <- tmp[,1]
    } else {
      lab <- dat[[outcome]]
    }
    
    # Make our DMatrix
    mat <- dat %>% dplyr::select(-outcome, -all_of(exclude_vars)) %>% # encode on full boston df
      onehot::onehot() %>% # use onehot to encode variables
      predict(dat) # get OHE matrix
    
    return(xgb.DMatrix(data = mat, 
                       label = lab))
    
  }
  
}

# helper function
#' @name xg_error
#' @param model xgb object, a fitted boosted model
#' @param test_mat DMatrix, a test set
#' @param metric string (either "mse" or "misclass"), indicates the error metric
#' @returns MSE/misclass rate of the model on the test set
xg_error <- function(model, test_mat, metric = "mse"){
  
  # Get predictions and actual values
  preds = predict(model, test_mat)
  vals = getinfo(test_mat, "label")
  
  if(metric == "mse"){
    
    # Compute MSE if that's what we need
    err <- mean((preds - vals)^2)
    
  } else if(metric == "misclass") {
    
    # Otherwise, get the misclass rate
    err <- mean(preds != vals)
    
  }
  
  return(err)
}

# Tuning ------------------------------------------------------------------

# Evaluation function:
eval_mse <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  err <- mean((preds - labels)^2)
  return(list(metric = "mse", value = err))
}

# First CV - tune eta
xgb_cv1 <- train_dat %>% crossv_kfold(k = 5) %>%
  mutate(train = map(train, as_tibble),
         test = map(test, as_tibble)) %>%
  crossing(learn_rate = seq(0.001, 0.015, length.out = 10)) %>%
  mutate(train = map(train, xgb_matrix, outcome = "change", exclude_vars = NULL),
         test = map(test, xgb_matrix, outcome = "change", exclude_vars = NULL))

# Run CV:
xgb_cv1 <- xgb_cv1 %>%
  mutate(
    xg_model = pmap(.l = list(train, test, learn_rate),
                    .f = function(x, y, z) xgb.train(params = list(eta = z,
                                                                   depth = 10,
                                                                   objective = "reg:squarederror"),
                                                     data = x,
                                                     watchlist = list(train = x, test = y),
                                                     nrounds = 10000,
                                                     early_stopping_rounds = 500,
                                                     feval = eval_mse,
                                                     maximize = FALSE,
                                                     print_every_n = FALSE)),
    xg_train_mse = map2(xg_model, train, xg_error, metric = "mse"),
    xg_test_mse = map2(xg_model, test, xg_error, metric = "mse") 
  )

# Find the best learning rate:
xgb_cv1 %>%
  mutate(xg_train_mse = unlist(xg_train_mse),
         xg_test_mse = unlist(xg_test_mse)) %>%
  group_by(learn_rate) %>%
  summarise(mean_train_mse = mean(xg_train_mse),
            mean_test_mse = mean(xg_test_mse)) %>%
  arrange(mean_test_mse)

# Tune max_depth:
xgb_cv2 <- train_dat %>% crossv_kfold(k = 5) %>%
  mutate(train = map(train, as_tibble),
         test = map(test, as_tibble)) %>%
  crossing(max_depth = seq(2, 10, length.out = 9)) %>%
  mutate(train = map(train, xgb_matrix, outcome = "change", exclude_vars = NULL),
         test = map(test, xgb_matrix, outcome = "change", exclude_vars = NULL))

# Run CV:
xgb_cv2 <- xgb_cv2 %>%
  mutate(
    xg_model = pmap(.l = list(train, test, max_depth),
                    .f = function(x, y, z) xgb.train(params = list(eta = 0.005,
                                                                   max_depth = z,
                                                                   objective = "reg:squarederror"),
                                                     data = x,
                                                     watchlist = list(train = x, test = y),
                                                     nrounds = 10000,
                                                     early_stopping_rounds = 500,
                                                     feval = eval_mse,
                                                     maximize = FALSE,
                                                     print_every_n = FALSE)),
    xg_train_mse = map2(xg_model, train, xg_error, metric = "mse"),
    xg_test_mse = map2(xg_model, test, xg_error, metric = "mse") 
  )

# Find best max_depth:
xgb_cv2 %>%
  mutate(xg_train_mse = unlist(xg_train_mse),
         xg_test_mse = unlist(xg_test_mse)) %>%
  group_by(max_depth) %>%
  summarise(mean_train_mse = mean(xg_train_mse),
            mean_test_mse = mean(xg_test_mse)) %>%
  arrange(mean_test_mse)

# Tune min_child_weight:
xgb_cv3 <- train_dat %>% crossv_kfold(k = 5) %>%
  mutate(train = map(train, as_tibble),
         test = map(test, as_tibble)) %>%
  crossing(min_child_weight = seq(1, 9, length.out = 5)) %>%
  mutate(train = map(train, xgb_matrix, outcome = "change", exclude_vars = NULL),
         test = map(test, xgb_matrix, outcome = "change", exclude_vars = NULL))

# Run CV:
xgb_cv3 <- xgb_cv3 %>%
  mutate(
    xg_model = pmap(.l = list(train, test, min_child_weight),
                    .f = function(x, y, z) xgb.train(params = list(eta = 0.005,
                                                                   max_depth = 4,
                                                                   min_child_weight = z,
                                                                   objective = "reg:squarederror"),
                                                     data = x,
                                                     watchlist = list(train = x, test = y),
                                                     nrounds = 10000,
                                                     early_stopping_rounds = 500,
                                                     feval = eval_mse,
                                                     maximize = FALSE,
                                                     print_every_n = FALSE)),
    xg_train_mse = map2(xg_model, train, xg_error, metric = "mse"),
    xg_test_mse = map2(xg_model, test, xg_error, metric = "mse") 
  )

# Find optimal min_child_weight:
xgb_cv3 %>%
  mutate(xg_train_mse = unlist(xg_train_mse),
         xg_test_mse = unlist(xg_test_mse)) %>%
  group_by(min_child_weight) %>%
  summarise(mean_train_mse = mean(xg_train_mse),
            mean_test_mse = mean(xg_test_mse)) %>%
  arrange(mean_test_mse)

# Find optimal gamma:
xgb_cv4 <- train_dat %>% crossv_kfold(k = 5) %>%
  mutate(train = map(train, as_tibble),
         test = map(test, as_tibble)) %>%
  crossing(gamma = seq(0, 0.6, length.out = 7)) %>%
  mutate(train = map(train, xgb_matrix, outcome = "change", exclude_vars = NULL),
         test = map(test, xgb_matrix, outcome = "change", exclude_vars = NULL))

# Run CV:
xgb_cv4 <- xgb_cv4 %>%
  mutate(
    xg_model = pmap(.l = list(train, test, gamma),
                    .f = function(x, y, z) xgb.train(params = list(eta = 0.005,
                                                                   max_depth = 4,
                                                                   min_child_weight = 9,
                                                                   gamma = z,
                                                                   objective = "reg:squarederror"),
                                                     data = x,
                                                     watchlist = list(train = x, test = y),
                                                     nrounds = 10000,
                                                     early_stopping_rounds = 500,
                                                     feval = eval_mse,
                                                     maximize = FALSE,
                                                     print_every_n = FALSE)),
    xg_train_mse = map2(xg_model, train, xg_error, metric = "mse"),
    xg_test_mse = map2(xg_model, test, xg_error, metric = "mse") 
  )

# Find optimal gamma:
xgb_cv4 %>%
  mutate(xg_train_mse = unlist(xg_train_mse),
         xg_test_mse = unlist(xg_test_mse)) %>%
  group_by(gamma) %>%
  summarise(mean_train_mse = mean(xg_train_mse),
            mean_test_mse = mean(xg_test_mse)) %>%
  arrange(mean_test_mse)

# Find optimal colsample_bytree:
xgb_cv5 <- train_dat %>% crossv_kfold(k = 5) %>%
  mutate(train = map(train, as_tibble),
         test = map(test, as_tibble)) %>%
  crossing(colsample_bytree = seq(0.2, 1, length.out = 9)) %>%
  mutate(train = map(train, xgb_matrix, outcome = "change", exclude_vars = NULL),
         test = map(test, xgb_matrix, outcome = "change", exclude_vars = NULL))

# Run CV
xgb_cv5 <- xgb_cv5 %>%
  mutate(
    xg_model = pmap(.l = list(train, test, colsample_bytree),
                    .f = function(x, y, z) xgb.train(params = list(eta = 0.005,
                                                                   max_depth = 4,
                                                                   min_child_weight = 9,
                                                                   gamma = 0,
                                                                   colsample_bytree = z,
                                                                   objective = "reg:squarederror"),
                                                     data = x,
                                                     watchlist = list(train = x, test = y),
                                                     nrounds = 10000,
                                                     early_stopping_rounds = 500,
                                                     feval = eval_mse,
                                                     maximize = FALSE,
                                                     print_every_n = FALSE)),
    xg_train_mse = map2(xg_model, train, xg_error, metric = "mse"),
    xg_test_mse = map2(xg_model, test, xg_error, metric = "mse") 
  )

xgb_cv5 %>%
  mutate(xg_train_mse = unlist(xg_train_mse),
         xg_test_mse = unlist(xg_test_mse)) %>%
  group_by(colsample_bytree) %>%
  summarise(mean_train_mse = mean(xg_train_mse),
            mean_test_mse = mean(xg_test_mse)) %>%
  arrange(mean_test_mse)

# Tune nrounds:
xgb_cv6 <- train_dat %>% crossv_kfold(k = 5) %>%
  mutate(train = map(train, as_tibble),
         test = map(test, as_tibble)) %>%
  crossing(nrounds = seq(600, 1500, length.out = 10)) %>%
  mutate(train = map(train, xgb_matrix, outcome = "change", exclude_vars = NULL),
         test = map(test, xgb_matrix, outcome = "change", exclude_vars = NULL))

xgb_cv6 <- xgb_cv6 %>%
  mutate(
    xg_model = pmap(.l = list(train, test, nrounds),
                    .f = function(x, y, z) xgb.train(params = list(eta = 0.005,
                                                                   max_depth = 4,
                                                                   min_child_weight = 9,
                                                                   gamma = 0,
                                                                   colsample_bytree = 0.4,
                                                                   objective = "reg:squarederror"),
                                                     data = x,
                                                     nrounds = z,
                                                     print_every_n = FALSE)),
    xg_train_mse = map2(xg_model, train, xg_error, metric = "mse"),
    xg_test_mse = map2(xg_model, test, xg_error, metric = "mse") 
  )

xgb_cv6 %>%
  mutate(xg_train_mse = unlist(xg_train_mse),
         xg_test_mse = unlist(xg_test_mse)) %>%
  group_by(nrounds) %>%
  summarise(mean_train_mse = mean(xg_train_mse),
            mean_test_mse = mean(xg_test_mse)) %>%
  arrange(mean_test_mse)

# Build final model:
xgb_final <- xgb.train(params = list(eta = 0.005,
                                     max_depth = 4,
                                     min_child_weight = 9,
                                     gamma = 0,
                                     colsample_bytree = 0.4,
                                     objective = "reg:squarederror"),
                       data = train_dat %>% xgb_matrix(outcome = "change", exclude_vars = NULL),
                       nrounds = 1200)

predict(xgb_final, newdata = test_dat[1, ] %>% xgb_matrix(outcome = "change", exclude_vars = NULL))

# Function to generate predictions for test data:
generate_predictions <- function(data, init_deaths, model){
  current_deaths <- init_deaths
  # Create an empty vector the length of our data to store predicted values
  predicted_change <- rep(0, nrow(data))
  # Create an empty vector the length of our data to store the cumulative number of deaths
  predicted_total <- rep(0, nrow(data))
  for (i in 1:nrow(data)){
    pred <- predict(model, data[i, ] %>% xgb_matrix(outcome = "change", exclude_vars = NULL))
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
  select(-date, -total_deaths) %>%
  group_by(state, add = TRUE) %>%
  group_nest() %>%
  left_join(initial_deaths, by = "state") %>%
  mutate(
    predictions = map2(data, init_deaths, generate_predictions, model = xgb_final)
  )

# Function to get real results:
extract_results <- function(input_state, keep_vars = FALSE){
  if(keep_vars == FALSE) {
    result <- test %>% filter(state == input_state) %>%
      select(date, total_deaths, change)
  }
  else {
    result <- test %>% filter(state == state)
  }
  result
}

outcomes <- predictions %>% mutate(
  actual_results = map(state, extract_results)
) %>% select(state, predictions, actual_results) %>%
  unnest(cols = c(predictions, actual_results))

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
  write_csv("outcomes/boosting.csv")


