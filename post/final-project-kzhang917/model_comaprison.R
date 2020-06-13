# Model Comparison  -------------------------------------------------------

# Load Packages -----------------------------------------------------------

library(tidyverse)

# Load Data ---------------------------------------------------------------

rf <- read_csv("outcomes/rf.csv") %>%
  mutate(
    model = rep("RF", nrow(.))
  )

deep_learning <- read_csv("outcomes/deep_learning.csv") %>%
  mutate(
    model = rep("Keras", nrow(.))
  )

ridge <- read_csv("outcomes/ridge.csv") %>%
  mutate(
    model = rep("Ridge", nrow(.))
  )

boosting <- read_csv("outcomes/boosting.csv") %>%
  mutate(
    model = rep("xgboost", nrow(.))
  )

real_values <- rf %>%
  mutate(pred_change = change,
         pred_total = total_deaths,
         model = "Reported Deaths")


outcomes <- rf %>%
  bind_rows(deep_learning, ridge, boosting, real_values)

# Create Ensemble model:
ensemble <- outcomes %>%
  filter(model != "Reported Deaths") %>%
  group_by(state, date) %>%
  summarise(pred_change = mean(pred_change),
            pred_total = mean(pred_total)) %>%
  ungroup() %>%
  mutate(total_deaths = rf$total_deaths,
         change = rf$change,
         model = "Ensemble") %>%
  select(state, date, total_deaths, change, pred_change, pred_total, model)


outcomes <- outcomes %>%
  bind_rows(ensemble)
 
outcomes %>%
  group_by(date, model) %>%
  summarise(pred_change = sum(pred_change),
            pred_total = sum(pred_total)) %>%
  ggplot(aes(date, pred_total)) +
  geom_line(aes(color = model)) +
  labs(title = "Model Performance - Predicted National Death Totals")

outcomes %>%
  filter(state == "New York") %>%
  group_by(date, model) %>%
  summarise(pred_change = sum(pred_change),
            pred_total = sum(pred_total)) %>%
  ggplot(aes(date, pred_total)) +
  geom_line(aes(color = model)) +
  labs(title = "Model Performance - New York Death Totals")

outcomes %>%
  filter(state == "Minnesota") %>%
  group_by(date, model) %>%
  summarise(pred_change = sum(pred_change),
            pred_total = sum(pred_total)) %>%
  ggplot(aes(date, pred_total)) +
  geom_line(aes(color = model)) +
  labs(title = "Model Performance - Minnesota Death Totals")

outcomes %>%
  filter(state == "Florida") %>%
  group_by(date, model) %>%
  summarise(pred_change = sum(pred_change),
            pred_total = sum(pred_total)) %>%
  ggplot(aes(date, pred_total)) +
  geom_line(aes(color = model)) +
  labs(title = "Model Performance - Florida Death Totals")

outcomes %>%
  filter(state == "Alabama") %>%
  group_by(date, model) %>%
  summarise(pred_change = sum(pred_change),
            pred_total = sum(pred_total)) %>%
  ggplot(aes(date, pred_total)) +
  geom_line(aes(color = model)) +
  labs(title = "Model Performance - Alabama Death Totals")

outcomes %>%
  filter(state == "California") %>%
  group_by(date, model) %>%
  summarise(pred_change = sum(pred_change),
            pred_total = sum(pred_total)) %>%
  ggplot(aes(date, pred_total)) +
  geom_line(aes(color = model)) +
  labs(title = "Model Performance - California Death Totals")

outcomes %>%
  filter(state == "Montana") %>%
  group_by(date, model) %>%
  summarise(pred_change = sum(pred_change),
            pred_total = sum(pred_total)) %>%
  ggplot(aes(date, pred_total)) +
  geom_line(aes(color = model)) +
  labs(title = "Model Performance - Montana Death Totals")

outcomes %>%
  filter(state == "Kansas") %>%
  group_by(date, model) %>%
  summarise(pred_change = sum(pred_change),
            pred_total = sum(pred_total)) %>%
  ggplot(aes(date, pred_total)) +
  geom_line(aes(color = model)) +
  labs(title = "Model Performance - Kansas Death Totals")

# Calculate national and state level MSE
# National MSE
mean_state_mse <- outcomes %>%
  group_by(model) %>%
  summarise(state_mse = mean((pred_change - change)^2))

national_mse <- outcomes %>%
  group_by(model, date) %>%
  summarise(change = sum(change),
            pred_change = sum(pred_change)) %>%
  ungroup() %>%
  group_by(model) %>%
  summarise(national_mse = mean((pred_change - change) ^2))

national_mse %>%
  left_join(mean_state_mse, by = "model") %>%
  ggplot(aes(national_mse, state_mse)) +
  geom_point(aes(color = model))
