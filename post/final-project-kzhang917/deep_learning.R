# Model Building ----------------------------------------------------------

# Load Packages -----------------------------------------------------------

library(tidyverse)
library(keras)
library(tensorflow)
library(caret)
library(janitor)

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
                        "Puerto Rico")))

# I don't plan on using the driving variables as of right now, so they will be removed:
data <- data %>%
  select(-driving, -driving_lag14, -driving_lag21, -rollmean_driving)

# Training / testing split: Training will be before May 18th, testing after.
train <- data %>%
  filter(date <= "2020-05-18")

test <- data %>%
  filter(date > "2020-05-18")

# Onehot encoding:
train_dmy <- dummyVars(" ~ .", data = train %>% select(weekday))
train_onehot <- as_tibble(predict(train_dmy, newdata = train))

# datasets will be modified to be used with keras:
train_keras <- train %>%
  select(-weekday) %>%
  bind_cols(train_onehot)

train_keras <- train_keras %>%
  select(-state, -date, -total_deaths, -(19:30), -first_death)

# Scale continuous 
mean <- apply(train_keras %>%
                select(-change, -(29:35)), 2, mean)
std <- apply(train_keras %>%
               select(-change, -(29:35)), 2, sd)
train_keras <- train_keras %>%
  select(-change, -(29:35)) %>%
  scale(center = mean, scale = std) %>%
  as_tibble() %>%
  bind_cols(
    train_keras %>% select(change, 29:35)
  ) %>%
  select(change, everything())

test_dmy <- dummyVars(" ~ .", data = test %>% select(weekday))
test_onehot <- as_tibble(predict(test_dmy, newdata = test))

test_keras <- test %>%
  select(-weekday) %>%
  bind_cols(test_onehot)

test_keras <- test_keras %>%
  select(-state, -date, -total_deaths, -(19:30), -first_death)

# Scale continuous 
test_keras <- test_keras %>%
  select(-change, -(29:35)) %>%
  scale(center = mean, scale = std) %>%
  as_tibble() %>%
  bind_cols(
    test_keras %>% select(change, 29:35)
  ) %>%
  select(change, everything()) %>%
  select(-change)

# Create a validation set:
train_keras <- train_keras %>%
  mutate(id = row_number()) %>%
  select(id, everything())

validation_keras <- train_keras %>%
  sample_frac(0.2)

train_keras <- train_keras %>%
  anti_join(validation_keras, by = "id")

# Convert to matrices:
train_keras_vars <- train_keras %>% select(-change, -id) %>% as.matrix()
train_keras_change <- train_keras %>% select(change) %>% as.matrix()

validation_keras_vars <- validation_keras %>% select(-change, -id) %>% as.matrix()
validation_keras_change <- validation_keras %>% select(change) %>% as.matrix()

# Build neural net - 8 units:
network_keras <- keras_model_sequential() %>%
  layer_dense(units = 8, activation = "relu",
              input_shape = dim(train_keras_vars)[[2]]) %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1)

network_keras %>% compile(
  optimizer = "rmsprop",
  loss = "mse",
  metrics = c("mae")
)

history_keras <- network_keras %>% fit(
  train_keras_vars,
  train_keras_change,
  epochs = 200,
  batch_size = 32,
  validation_data = list(validation_keras_vars,
                         validation_keras_change))

plot(history_keras)


# Second neural net - 16 units:
network_keras_2 <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu",
              input_shape = dim(train_keras_vars)[[2]]) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1)

network_keras_2 %>% compile(
  optimizer = "rmsprop",
  loss = "mse",
  metrics = c("mae")
)

history_keras_2 <- network_keras_2 %>% fit(
  train_keras_vars,
  train_keras_change,
  epochs = 150,
  batch_size = 32,
  validation_data = list(validation_keras_vars,
                         validation_keras_change))

plot(history_keras_2)

# Final model - 16 units and 40 epochs:
network_keras_final <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu",
              input_shape = dim(train_keras_vars)[[2]]) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1)

network_keras_final %>% compile(
  optimizer = "rmsprop",
  loss = "mse",
  metrics = c("mae")
)

history_keras_final <- network_keras_final %>% fit(
  train_keras_vars,
  train_keras_change,
  epochs = 40,
  batch_size = 32,
  validation_data = list(validation_keras_vars,
                         validation_keras_change))

# Use model to make predictions:
results <- network_keras_final %>% evaluate(test_keras %>% as.matrix(),
                                            test %>% select(change) %>% as.matrix())


network_keras_final %>% predict(test_keras[353, ] %>% as.matrix())



scale(predict(network_keras_final, test_keras[30, ] %>% as.matrix()), center = mean[1], scale = std[1])[1]

# Function to generate predictions for test data:
generate_predictions <- function(data, init_deaths, model){
  current_deaths <- init_deaths
  # Create an empty vector the length of our data to store predicted values
  predicted_change <- rep(0, nrow(data))
  # Create an empty vector the length of our data to store the cumulative number of deaths
  predicted_total <- rep(0, nrow(data))
  for (i in 1:nrow(data)){
    pred <- predict(model, data[i, ] %>% as.matrix())[1]
    current_deaths <- current_deaths + pred
    predicted_change[i] <- pred
    predicted_total[i] <- current_deaths
    # Fill lag variables in rows further down based on the predicted value
    for (j in 1:14){
      scaled_pred <- scale(pred, center = mean[j], scale = std[j])[1]
      if(i + j <= 14)
        data[(i + j), j] <- scaled_pred
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

predicted_results <- test_keras %>%
  mutate(state = test$state,
         date = test$date) %>%
  select(state, date, everything()) %>%
  arrange(state, date) %>%
  select(-date) %>%
  group_by(state, add = TRUE) %>%
  group_nest() %>%
  left_join(initial_deaths, by = "state") %>%
  mutate(
    predictions = map2(data, init_deaths, generate_predictions, model = network_keras_final)
  )
 
predicted_results %>% unnest(cols = c(predictions)) %>% filter(state == "New Jersey")

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

predicted_results

outcomes <- predicted_results %>% mutate(
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

# Visualize nationally predicted deaths for the entire duration of the pandemic:
data %>%
  filter(date <= "2020-05-18") %>%
  select(state, date, total_deaths, change) 

outcomes %>% bind_rows(
  data %>%
    filter(date <= "2020-05-18") %>%
    select(state, date, total_deaths, change) 
) %>% group_by(date) %>%
  summarise(pred_total = sum(pred_total),
            total = sum(total_deaths)) %>%
  pivot_longer(cols = 2:3, names_to = "var") %>%
  ggplot(aes(date, value)) +
  geom_line(aes(color = var))

# MSE when letting the model chronologically generate predictions and updating lag variables with predictions
outcomes %>%
  summarise(mse = mean((change - pred_change)^2))

# MSE nationwide
outcomes %>%
  group_by(date) %>%
  summarise(pred_change = sum(pred_change),
            change = sum(change)) %>%
  summarise(mse = mean((change - pred_change)^2))

# Save important results:
history_keras %>% write_rds("deep_learning/history_keras.rds")
history_keras_2 %>% write_rds("deep_learning/history_keras_2.rds")
network_keras_final %>% save_model_tf("deep_learning/network_keras_final")
history_keras_final %>% write_rds("deep_learning/history_keras_final.rds")
outcomes %>% write_csv("outcomes/deep_learning.csv")



