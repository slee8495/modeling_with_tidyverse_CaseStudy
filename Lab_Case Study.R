# Case Study #1: Predicting Annual Air  Pollution

library(tidyverse)
library(tidymodels)
library(magrittr)
library(skimr)
library(vip)
library(randomForest)

#########################################################################################################
########################################## Linear Regression ############################################
#########################################################################################################

#########################################################################################################
#### Data Import
pm <- readr::read_csv("pm25_data.csv")

#########################################################################################################
#### Data Exploration and wrangling
pm %>% 
  dplyr::mutate(across(c(id, fips, zcta), as.factor)) -> pm
skim(pm)

#########################################################################################################
#### Evaluate Correlation
#### select numeric values only and cor chart

# First, we need to pick only numeric values
pm %>% 
  dplyr::select_if(is.numeric) %>% cor() -> pm_cor

corrplot::corrplot(pm_cor, tl.cex = 0.5)

#########################################################################################################
#### Splitting Data
set.seed(1234)
pm_split <- rsample::initial_split(data = pm, prop = 2/3)

train_pm <- rsample::training(pm_split)
test_pm <- rsample::testing(pm_split)


#########################################################################################################
#### Making a Receipt
simple_rec <- train_pm %>% 
  recipes::recipe(value ~ .)

#########################################################################################################
#### recipes for cooking!
# We want to id variable not to be included in predictor. it's going to create noises. 
# Also, "fips" variable is not needed.. 
simple_rec <- train_pm %>% 
  recipes::recipe(value ~ .) %>% 
  recipes::update_role(id, new_role = "id variable") %>% 
  recipes::update_role(fips, new_role = "county id")

# And we want to dummy encode our categorical variables, so that they become numeric (for our linear)
simple_rec %>% 
  recipes::step_dummy(state, county, city, zcta, one_hot = TRUE) -> simple_rec


# Now we want to remove variables that appear to be redundant and are highly correlated with others. 
# But, we want to spare some values like CMAQ and aod
simple_rec %>% 
  recipes::step_corr(all_predictors(), - CMAQ, - aod) -> simple_rec

# It is also a good idea to remove variables with near-zero variance
# again, we want to spare CMAQ and aod
simple_rec %>% 
  recipes::step_nzv(all_predictors(), - CMAQ, - aod) -> simple_rec


#########################################################################################################
#### Running Pre processing
prepped_rec <- prep(simple_rec, verbose = TRUE, retain = TRUE)
names(prepped_rec)

# we can take a look at it by using the bake()
preproc_train <- recipes::bake(prepped_rec, new_data = NULL)
glimpse(preproc_train)

# complare with the original data pm
glimpse(pm)


# here, notice how we only have 36 variables now instead of 50. 
# because two of these are our id variables, and we no longer have any categorical variables. 

baked_test_pm <- recipes::bake(prepped_rec, new_data = test_pm)
glimpse(baked_test_pm)

# training data and test data possibly don't match, because they are separated randomly
traincities <- train_pm %>% dplyr::distinct(city)
testcities <- test_pm %>% dplyr::distinct(city)


# get the number of cities that were different
dim(dplyr::setdiff(traincities, testcities))
# get the number of cities that overlapped
dim(dplyr::intersect(traincities, testcities))

## there are lots of test data that are not in training data
# Let's make only two cases for city column. because there are way too many cases. 
pm$city
pm %>% 
  dplyr::mutate(city = case_when(city == "Not in a city" ~ "Not in a city",
                                 city != "Not in a city" ~ "In a city")) -> pm
pm$city


# Let's do split part again since we have the new data with city column edited. 
set.seed(1234)
pm_split <- rsample::initial_split(data = pm, prop = 2/3)

train_pm <- rsample::training(pm_split)
test_pm <- rsample::testing(pm_split)

# now we will create a new recipe (same process as above)
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

#########################################################################################################
#### Specifying the Model
# here in this case study, linear regression
PM_model <- parsnip::linear_reg()

lm_PM_model <- PM_model %>% parsnip::set_engine("lm")      #"glm" for classification modeling

# now let's tell parsnip what we want. 

lm_PM_model <-
  PM_model %>% 
  parsnip::set_engine("lm") %>% 
  set_mode("regression")

# here, we aim to predict air pollution
# here, we combine everything together into a workflow

PM_wflow <- workflows::workflow() %>% 
            workflows::add_recipe(novel_rec) %>% 
            workflows::add_model(lm_PM_model)

PM_wflow


# next, we prepare the recipe and fit the model to our training data all at once
# printing the output, we can see the coefficients of the model

PM_wflow_fit <- parsnip::fit(PM_wflow, data = train_pm)


#########################################################################################################
#### Assessing the Model Fit

# After we fit out model, we can use broom package to look at the output in an easy/tidy way

wflowoutput <- PM_wflow_fit %>% 
  workflows::extract_fit_parsnip() %>% 
  broom::tidy()

# We have fit our model on our training data. which means we have a model to predict values of air pollution. 
# Now, we want to know which variables are the most important. 
# we can explore this using vip()
# This function creates a bar plot. 

# Let's look top 10 important variables. 
PM_wflow_fit %>% 
  workflows::pull_workflow_fit() %>% 
  vip::vip(num_features = 10)



#########################################################################################################
### Model Performance: Getting Predicted Values
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

# now we can compare the predictd outcome values (or fitted values) to the actual outcome values that we observed


wf_fitted_values %>% 
  ggplot2::ggplot(mapping = aes(x = value, y = .fitted)) +
  ggplot2::geom_point() +
  ggplot2::labs(x = "actual outcome values",
                y = "predicted outcome values")

# hmm.. we could do a little better than this..



#### Quantifying Model Performance
# now, let's use different distance functions to assess how far off our predicted outcome and actual outcome values.
# parameter is RMSE (root mean squared error)

yardstick::metrics(wf_fitted_values,
                   truth = value, estimate = .fitted)

#### Assessing Model Performance on v-folds Using tune
set.seed(1234)
vfold_pm <- rsample::vfold_cv(data = train_pm, v = 10)

dplyr::pull(vfold_pm, splits)

# fit the model to our cross validation folds using the fit_resamples() function of the tune package. 

set.seed(122)
resample_fit <- tune::fit_resamples(PM_wflow, vfold_pm)


# we can now take a look at various performance metrics based on the fit of our cross validation "resamples"
resample_fit

workflowsets::collect_metrics(resample_fit)




