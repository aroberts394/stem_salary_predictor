#### salary prediction shiny app project ####

### model salary prediction ###

# load data load and transform script
source("global.R")


#load required libraries
# library(caret)
library(corrplot)
# library(lares)
library(ggplot2)
library(rsample)
library(recipes)
library(parsnip)
library(usemodels)
library(workflows)
library(purrr)
library(broom)
library(yardstick)
library(workflowsets)
library(tune)
library(rules)
library(baguette)
library(finetune)
library(dials)
library(vip)
library(forcats)



#### EXPLORATORY ANALYSIS ----------------------------------------------------------

# plotting correlation plot
#correlations of all numeric variables
# numericVars <- which(sapply(highered, is.numeric)) #index vector of numeric variables
# all_numVars <- highered[, numericVars]
# (cor_numVars <- cor(all_numVars, method = "pearson", use = "complete.obs"))

#correlation plot of numeric variables
# corrplot(cor_numVar, type = "upper", order = "hclust",
#          method = "color",
#          tl.col = "black", tl.srt = 45,
#          addCoef.col = "black", 
#          insig = "blank")

# remove highly correlated field, also REFID field
highered <- highered %>% select(-MR03Y5, -REFID)

# check correlation between categorical variables
# categoricalVars <- names(which(sapply(highered, is.factor))) # derive categorical variables
# all_catVars <- as.data.frame(lapply(highered[categoricalVars], as.character))

# devtools::install_github("laresbernardo/lares") # install package for calculating and plotting correlations
# library(lares)

# calculate and plot correlations
# corr_cross(all_catVars, # name of dataset
#            max_pvalue = 0.05, # display only significant correlations (at 5% level)
#            # top = 10 # display top 10 couples of variables (by correlation coefficient)
#            )

# further removing highly correlated fields from analysis
highered <- highered %>% select(-NMRMEMG, -MRDGRUS, -MRDG, -BTHUS, -CTZUS, -NDGMEMG)

# remove extra df from memory
# rm(all_catVars)
# 
# 
# # check distribution of salary outcome
# ggplot(highered, aes(x = ADJ_SALARY)) + 
#   geom_vline(xintercept = median(highered$ADJ_SALARY), color = "green") +
#   geom_vline(xintercept = mean(highered$ADJ_SALARY), color = "blue") +
#   geom_histogram(bins = 70)
# 
# # check impact of log scale outcome variable
# ggplot(highered, aes(x = ADJ_SALARY)) + 
#   geom_histogram(bins = 50) +
#   geom_vline(xintercept = median(highered$ADJ_SALARY), color = "green") +
#   geom_vline(xintercept = mean(highered$ADJ_SALARY), color = "blue") +
#   scale_x_log10()


#### PREDICTIVE MODELING ------------------------------------------------------------

# splitting data
# Save the split information for an 80/20 split of the data, stratifying by salary
set.seed(123)
highered_split <- initial_split(highered, prop = 0.75, strata = ADJ_SALARY)
highered_train <- training(highered_split)
highered_test <- testing(highered_split)

# resample training set using 5 repeats of 10 fold crosss validation
set.seed(124)
highered_folds <- vfold_cv(highered_train, strata = ADJ_SALARY)


#### CREATE PRE-PROCESSING RECIPE ----------------------------
lm_recipe <- recipe(ADJ_SALARY ~., data = highered_train) %>%
  step_dummy(all_nominal())

pen_lm_recipe <- recipe(ADJ_SALARY ~., data = highered_train) %>%
  step_dummy(all_nominal()) %>%
  step_normalize(all_predictors()) %>%
  step_zv(all_predictors())

  
#### MODEL SPECIFICATIONS --------------------------------------------------------------

# KNN model
nearest_neighbor_kknn_spec <-
  nearest_neighbor(neighbors = tune(), dist_power = tune(), weight_func = tune()) %>%
  set_engine('kknn') %>%
  set_mode('regression')

# xgboost tree regression
boost_tree_xgboost_spec <-
  boost_tree(tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(), 
             min_n = tune(), sample_size = tune(), trees = tune()) %>%
  set_engine('xgboost') %>%
  set_mode('regression')

### random forest model
rand_forest_ranger_spec <-
  rand_forest(mtry = tune(), min_n = tune(), trees = 200) %>%
  set_engine('ranger') %>%
  set_mode('regression')  
  


#### LINEAR REGRESSION WORKFLOW  -------------------------------------------------
# specify the linear regression model
lm_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# fit linear regression model
# create a lm workflow
lm_wflow <- workflow() %>%
  add_model(lm_model) %>%
  add_recipe(lm_recipe)

#fit model
lm_fit <- fit(lm_wflow, data = highered_train)
tidy(lm_fit)

# make predictions
predict(lm_fit, new_data = highered_test)

# add predictions to test set
lm_pred <- highered_test %>%
  select(ADJ_SALARY) %>%
  bind_cols(predict(lm_fit, highered_test)) %>%
  bind_cols(predict(lm_fit, highered_test, type = "conf_int")) # add 95% confidence intervals

# linear regression metrics
lm_metrics <- metric_set(rmse, rsq, mae)
lm_metrics(lm_pred, truth = ADJ_SALARY, estimate = .pred)


### PENALIZED LINEAR REGRESSION WORKFLOW --------------------------------------
# specify penalized linear regression model
linear_reg_spec <- 
  linear_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")

# create workflow
pen_lm_wflow <- workflow() %>%
  add_recipe(pen_lm_recipe)

# tuning lasso parameters
# create set of bootstrap resamples
set(1234)
highered_boot <- bootstraps(highered_train)
lr_reg_grid <- grid_regular(penalty(), levels = 50)

# run tuning resamples
doParallel::registerDoParallel()
set.seed(2020)
lasso_grid <- tune_grid(
  pen_lm_wflow %>% add_model(linear_reg_spec), resamples = highered_boot,
  grid = lr_reg_grid
  )

# check tuning grid results
lowest_rmse <- lasso_grid %>%
  select_best("rmse")

final_lasso <- finalize_workflow(
  pen_lm_wflow %>% add_model(linear_reg_spec), lowest_rmse
)

# get variable importance
lasso_var_imp <- final_lasso %>%
  fit(highered_train) %>%
  pull_workflow_fit() %>%
  vi(lambda = lowest_rmse$penalty) %>%
  mutate(Importance = abs(Importance),
         Variable = fct_reorder(Variable, Importance))

# plot variable importance
lasso_var_imp %>%
  top_n(n = 20, wt = Importance) %>%
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL)

# check lass model metrics
last_fit(final_lasso, highered_split) %>%
  collect_metrics()

final_pen_lm <- fit(final_lasso, data = highered_train)


# add predictions to test set
pen_lm_pred <- highered_test %>%
  select(ADJ_SALARY) %>%
  bind_cols(predict(final_pen_lm, highered_test))

# get lasso metrics
lm_metrics(pen_lm_pred, truth = ADJ_SALARY, estimate = .pred)


#### REGRESSION TREES -----------------------------------------------------------
# specify regression tree
# adding default values now, will work on tuning later
reg_tree_spec <- 
  decision_tree(min_n = 2, cost_complexity = 0.0000000001, tree_depth = 15) %>%
  set_engine("rpart") %>%
  set_mode("regression")

# create workflow
reg_tree_wflow <- workflow() %>%
  add_model(reg_tree_spec) %>%
  add_recipe(lm_recipe) # using simple linear regression recipe

# set tuning parameters for regression trees
# tree_grid <- grid_regular(cost_complexity(), tree_depth(), min_n(), levels = 2)
# tree_grid

# run tuning resamples
# doParallel::registerDoParallel()
# set.seed(2020)
# 
# tree_rs <- tune_grid(
#   reg_tree_wflow, resamples = highered_folds, grid = tree_grid,
#   metrics = metric_set(rmse, rsq, mae)
# )

# fit regression trees
reg_tree_fit <- fit(reg_tree_wflow, data = highered_train)


# check variable importance
reg_tree_fit %>% 
  pull_workflow_fit() %>%
  vip(geom = "col", aesthetics = list(fill = "steelblue", alpha = 0.8)) +
  scale_y_continuous(expand = c(0, 0))

# add predictions to test set
reg_pred <- highered_test %>%
  select(ADJ_SALARY) %>%
  bind_cols(predict(reg_tree_fit, highered_test))

# regression tree metrics
reg_metrics <- metric_set(rmse, rsq, mae)
reg_metrics(reg_pred, truth = ADJ_SALARY, estimate = .pred)

















