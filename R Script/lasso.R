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
lasso_spec <- 
  linear_reg(penalty = tune(), mixture = 1) %>% 
  set_mode("regression") %>% 
  set_engine("glmnet") 

lasso_workflow <- workflow() %>% 
  add_recipe(recipe) %>% 
  add_model(lasso_spec)

lasso_penalty_grid <- grid_regular(penalty(range = c(-10, 10)), levels = 50)

lasso_tune_res <- tune_grid(
  ridge_workflow,
  resamples = pop_folds, 
  grid = lasso_penalty_grid)

autoplot(lasso_tune_res)

lasso_best <- select_best(lasso_tune_res, metric = "rsq")

lasso_final <- finalize_workflow(lasso_workflow, lasso_best)
lasso_final_fit <- fit(lasso_final, data = pop_train)
augment(lasso_final_fit, new_data = pop_test) %>%
  rsq(truth = Population_2022, estimate = .pred)

augment(lasso_final_fit, new_data = data)['.pred']