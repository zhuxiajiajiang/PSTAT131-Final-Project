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
reg_tree_spec <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("regression")

reg_tree_wf <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(reg_tree_spec %>% set_args(cost_complexity = tune()))

reg_tree_param_grid <- grid_regular(cost_complexity(range = c(-10, -1)), levels = 50)

reg_tree_tune_res <- tune_grid(
  reg_tree_wf, 
  resamples = pop_folds, 
  grid = reg_tree_param_grid
)

autoplot(reg_tree_tune_res)

reg_tree_best <- select_best(reg_tree_tune_res, metric = "rmse")

reg_tree_final <- finalize_workflow(reg_tree_wf, reg_tree_best)
reg_tree_final_fit <- fit(reg_tree_final, data = pop_train)
#reg_tree_final_fit %>%
#extract_fit_engine() %>%
#rpart.plot()
augment(reg_tree_final_fit, new_data = pop_test) %>%
  rsq(truth = Population_2022, estimate = .pred)

augment(reg_tree_final_fit, new_data = data)['.pred']