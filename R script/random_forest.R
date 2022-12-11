source("data_split_cv.R")

library(tidyverse)
library(tidymodels)

recipe <- 
  recipe(formula = Population_2022 ~ ., data = pop_train) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

set.seed(1234)
rf_spec <- rand_forest(mtry = tune()) %>%
  set_engine("randomForest", importance = TRUE) %>%
  set_mode("regression")

rf_wf <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(rf_spec)

rf_grid <- grid_regular(parameters(rf_spec) %>%
                          update(mtry = mtry(range= c(1, 9))), levels = 9)

rf_tune_res <- tune_grid(
  rf_wf, 
  resamples = pop_folds, 
  grid = rf_grid
)

autoplot(rf_tune_res)

rf_best <- select_best(rf_tune_res, metric = "rmse")

rf_final <- finalize_workflow(rf_wf, rf_best)
rf_final_fit <- fit(rf_final, data = pop_train)
augment(rf_final_fit, new_data = pop_test) %>%
  rsq(truth = Population_2022, estimate = .pred)

augment(rf_final_fit, new_data = data)['.pred']