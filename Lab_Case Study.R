# Case Study #1: Predicting Annual Air  Pollution

library(tidyverse)
library(tidymodels)
library(magrittr)
library(skimr)
library(vip)

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


# get the number of cities that were different
dim(dplyr::setdiff(traincities, testcities))
# get the number of cities that overlapped
dim(dplyr::intersect(traincities, testcities))

## there are lots of test data that are not in training data


pm %>% 
  dplyr::mutate(city = case_when(city == "Not in a city" ~ "Not in a city",
                                 city != "Not in a city" ~ "In a city")) -> pm


set.seed(1234)
pm_split <- rsample::initial_split(data = pm, prop = 2/3)

train_pm <- rsample::training(pm_split)
test_pm <- rsample::testing(pm_split)

# now we will create a new recipe
novel_rec <- train_pm %>% 
  recipes::recipe() %>% 
  recipes::update_role(everything(), new_role = "predictor") %>% 
  recipes::update_role(value, new_role = "outcome") %>% 
  recipes::update_role(id, new_role = "id variable") %>% 
  recipes::update_role("fips", new_role = "county id") %>% 
  recipes::step_dummy(state, county, city, zcta, one_hot = TRUE) %>% 
  recipes::step_corr(all_numeric()) %>% 
  recipes::step_nzv(all_numeric())

# now we will check the preprocessed data again to see if we still have NA values.
prepped_rec <- recipes::prep(novel_rec, verbose = TRUE, retain = TRUE)
preproc_train <- recipes::bake(prepped_rec, new_data = NULL)
skim(preproc_train)   # now no longer NA

#### Specifying the Model
PM_model <- parsnip::linear_reg()

lm_PM_model <- PM_model %>% parsnip::set_engine("lm")      #"glm" for classification modeling

# here, we aim to predict air pollution
lm_PM_model <-
  PM_model %>% 
  parsnip::set_engine("lm") %>% 
  set_mode("regression")

# here, we combine everything together into a workflow
PM_wflow <- workflows::workflow() %>% 
            workflows::add_recipe(novel_rec) %>% 
            workflows::add_model(lm_PM_model)

PM_wflow


# next, we prepare the recipe and fit the model to our training data all at once
# pringting the output, we can see the coefficients of the model

PM_wflow_fit <- parsnip::fit(PM_wflow, data = train_pm)

#### Assessing the Model Fit

wflowoutput <- PM_wflow_fit %>% 
  workflows::extract_fit_parsnip() %>% 
  broom::tidy()

PM_wflow_fit %>% 
  workflows::pull_workflow_fit() %>% 
  vip::vip(num_features = 10)


# Model Performance: Getting Predicted Values
wf_fit <- PM_wflow_fit %>% 
  workflows::pull_workflow_fit()

wf_fitted_values <- wf_fit$fit$fitted.values
head(wf_fitted_values)

wf_fitted_values <- 
  broom::augment(wf_fit$fit, data = preproc_train) %>% 
  dplyr::select(value, .fitted:.std.resid)

head(wf_fitted_values)

values_pred_train <-
  predict(PM_wflow_fit, train_pm) %>% 
  dplyr::bind_cols(train_pm %>% dplyr::select(value, fips, county, id))

#### Visualizing Model Performance




