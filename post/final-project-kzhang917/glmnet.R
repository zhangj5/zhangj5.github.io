# Ridge Regression --------------------------------------------------------

# Load Packages -----------------------------------------------------------

library(tidyverse)
library(modelr)
library(janitor)
library(skimr)
library(glmnet)
library(glmnetUtils)

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


# Ridge Regression --------------------------------------------------------

# lambda grid to search -- use for ridge regression (400 values)
lambda_grid <- 10^seq(-10, 10, length = 400)

# ridge regression: 10-fold cv
ridge_cv <- train %>% 
  cv.glmnet(
    formula = change ~ . - state - date - total_deaths, 
    data = ., 
    alpha = 0, 
    nfolds = 10,
    lambda = lambda_grid
  )

plot(ridge_cv)

# ridge's best lambdas
ridge_lambda_min <- ridge_cv$lambda.min
ridge_lambda_1se <- ridge_cv$lambda.1se

# Train best models:
final_glmnet <- tibble(
  train = train %>% list(),
  test  = test %>% list()
) %>%
  mutate(
    ridge_min = map(train, ~ glmnet(change ~ . - state - date - total_deaths, data = .x,
                                    alpha = 0, lambda = ridge_lambda_min)),
    ridge_1se = map(train, ~ glmnet(change ~ . - state - date - total_deaths, data = .x,
                                    alpha = 0, lambda = ridge_lambda_1se))) %>%
  pivot_longer(cols = c(-test, -train), names_to = "method", values_to = "fit")

ridge_min_model <- glmnet(change ~ . - state - date - total_deaths, data = train,
                          alpha = 0, lambda = ridge_lambda_min)
ridge_1se_model <- glmnet(change ~ . - state - date - total_deaths, data = train,
                          alpha = 0, lambda = ridge_lambda_1se)

# A look at coefficients
final_glmnet %>% 
  pluck("fit") %>% 
  map( ~ coef(.x) %>% 
         as.matrix() %>% 
         as.data.frame() %>% 
         rownames_to_column("name")) %>%
  reduce(full_join, by = "name") %>% 
  mutate_if(is.double, ~ if_else(. == 0, NA_real_, .)) %>% 
  rename(ridge_min = s0.x,
         ridge_1se = s0.y) %>%
  knitr::kable(digits = 3)


# Function to generate predictions for test data:
generate_predictions <- function(data, init_deaths, model){
  current_deaths <- init_deaths
  # Create an empty vector the length of our data to store predicted values
  predicted_change <- rep(0, nrow(data))
  # Create an empty vector the length of our data to store the cumulative number of deaths
  predicted_total <- rep(0, nrow(data))
  for (i in 1:nrow(data)){
    pred <- predict(model, data[i, ])[1]
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
    predictions = map2(data, init_deaths, generate_predictions, model = ridge_min_model)
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

outcomes %>% write_csv("outcomes/ridge.csv")


# 1se Model ---------------------------------------------------------------

predictions <- test %>%
  group_by(state, add = TRUE) %>%
  group_nest() %>%
  left_join(initial_deaths, by = "state") %>%
  mutate(
    predictions = map2(data, init_deaths, generate_predictions, model = ridge_1se_model)
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



