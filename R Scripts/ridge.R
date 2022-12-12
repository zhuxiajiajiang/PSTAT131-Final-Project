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
ridge_spec <- linear_reg(mixture = 0, penalty = tune()) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

ridge_workflow <- workflow() %>% 
  add_recipe(recipe) %>% 
  add_model(ridge_spec)

ridge_penalty_grid <- grid_regular(penalty(range = c(-10, 10)), levels = 50)

ridge_tune_res <- tune_grid(
  ridge_workflow,
  resamples = pop_folds, 
  grid = ridge_penalty_grid)

autoplot(ridge_tune_res)

ridge_best <- select_best(ridge_tune_res, metric = "rsq")

ridge_final <- finalize_workflow(ridge_workflow, ridge_best)
ridge_final_fit <- fit(ridge_final, data = pop_train)

augment(ridge_final_fit, new_data = pop_test) %>%
  rsq(truth = Population_2022, estimate = .pred)

augment(ridge_final_fit, new_data = data)['.pred']