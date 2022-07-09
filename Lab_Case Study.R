# Case Study #1: Predicting Annual Air  Pollution

library(tidyverse)
library(tidymodels)
library(magrittr)
library(skimr)

# Data Import
pm <- readr::read_csv("pm25_data.csv")

# Data Exploration and wrangling
pm %>% 
  dplyr::mutate(across(c(id, fips, zcta), as.factor)) -> pm


# Evaluate Correlation
# select numeric values only and cor chart
pm %>% 
  dplyr::select_if(is.numeric) %>% cor() -> pm_cor

corrplot::corrplot(pm_cor, tl.cex = 0.5)

# Splitting Data
set.seed(1234)
pm_split <- rsample::initial_split(data = pm, prop = 2/3)

train_pm <- rsample::training(pm_split)
test_pm <- rsample::testing(pm_split)

# Making a Receipt
