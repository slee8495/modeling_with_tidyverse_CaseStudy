# Case Study #1: Predicting Annual Air  Pollution

library(tidyverse)
library(tidymodels)
library(magrittr)
library(skimr)

#### Data Import
pm <- readr::read_csv("pm25_data.csv")

#### Data Exploration and wrangling
pm %>% 
  dplyr::mutate(across(c(id, fips, zcta), as.factor)) -> pm


#### Evaluate Correlation
#### select numeric values only and cor chart
pm %>% 
  dplyr::select_if(is.numeric) %>% cor() -> pm_cor

corrplot::corrplot(pm_cor, tl.cex = 0.5)

#### Splitting Data
set.seed(1234)
pm_split <- rsample::initial_split(data = pm, prop = 2/3)

train_pm <- rsample::training(pm_split)
test_pm <- rsample::testing(pm_split)

# Making a Receipt
simple_rec <- train_pm %>% 
  recipes::recipe(value ~ .)


#### recipes for cooking!
simple_rec <- train_pm %>% 
  recipes::recipe(value ~ .) %>% 
  recipes::update_role(id, new_role = "id variable") %>% 
  recipes::update_role("fips", new_role = "country id") %>% 
  recipes::step_dummy(state, county, city, zcta, one_hot = TRUE) %>% 
  recipes::step_corr(all_predictors(), -CMAQ, -aod) %>% 
  recipes::step_nzv(all_predictors(), -CMAQ, -aod)

#### Running Pre processing

prepped_rec <- prep(simple_rec, verbose = TRUE, retain = TRUE)
names(prepped_rec)

preproc_train <- recipes::bake(prepped_rec, new_data = NULL)
glimpse(preproc_train)

# complare with the original data pm
glimpse(pm)


baked_test_pm <- recipes::bake(prepped_rec, new_data = test_pm)
glimpse(baked_test_pm)

# training data and test data possibly don't match, because they are splitted randomly
traincities <- train_pm %>% dplyr::distinct(city)
testcities <- test_pm %>% dplyr::distinct(city)



