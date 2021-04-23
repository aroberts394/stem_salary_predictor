# ======================= #
# Author: Tony Roberts
# Course: PUBH 7462
# Project: STEM Professional Salary Predictor
# Description: Predictive model building for STEM Salary shiny app
# ======================= #


# load data load and transform script
source("global.R")


# load required libraries
# library(caret)
library(corrplot)
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
library(ggfortify)
library(future)
library(parallel)



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


# remove highly correlated field, also REFID ID field
highered <- highered %>% select(-MR03Y5, -REFID)

# check correlation between categorical variables
# categoricalVars <- names(which(sapply(highered, is.factor))) # derive categorical variables
# all_catVars <- as.data.frame(lapply(highered[categoricalVars], as.character))

# devtools::install_github("laresbernardo/lares") # install package for calculating and plotting correlations
# library(lares)

# calculate and plot correlations
# lares::corr_cross(highered, # name of dataset
#            max_pvalue = 0.05, # display only significant correlations (at 5% level)
#            # top = 10 # display top 10 couples of variables (by correlation coefficient)
#            )

# correlations of variables against adj_salary variable
# lares::corr_var(highered, 
#                 var = ADJ_SALARY # name of variable to focus on
#                 )

# further removing highly correlated fields from analysis, 
# removing JOBSATIS for weak relationship/interpretability for use case
highered <- highered %>% select(-NMRMEMG, -MRDGRUS, -MRDG, -BTHUS, -CTZUS, -NDGMEMG, -JOBSATIS)


# simplified highered data frame
highered_simple <- highered %>% select(AGE, GENDER, RACETH, CHTOT, HRSWKGR, EMSIZE, EMSEC, 
                                      NOCPRMG, YEARS_SINCE_GRAD, ADJ_SALARY)


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

# SPLITTING INTO TRAINING AND TEST DATA 
# Save the split information for an 80/20 split of the data, stratifying by salary
set.seed(123)
highered_split <- initial_split(highered, prop = 0.75, strata = ADJ_SALARY)
highered_train <- training(highered_split)
highered_test <- testing(highered_split)

# resample training set using 5 repeats of 10 fold crosss validation
set.seed(124)
highered_folds <- vfold_cv(highered_train, strata = ADJ_SALARY)



#### LINEAR REGRESSION WORKFLOW  -------------------------------------------------
# create linear regression recipe
lm_recipe <- recipe(ADJ_SALARY ~., data = highered_train) %>%
  step_dummy(all_nominal()) %>%
  step_zv(all_predictors()) %>%
  step_corr(all_predictors())

# specify the linear regression model
lm_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# fit linear regression model
# create a lm workflow
lm_wflow <- workflow() %>%
  add_model(lm_model) %>%
  add_recipe(lm_recipe)

lm_simple_wflow <- workflow() %>%
  add_model(lm_model) %>%
  add_recipe(lm_simple_recipe)

#fit model
lm_fit <- fit(lm_wflow, data = highered_train)
lm_summary <- tidy(lm_fit)
lm_summary

# # check model diagnostics
# lm_fit_reg <- lm(ADJ_SALARY ~ sqrt(AGE) + ., data = highered)
# autoplot(lm_fit_reg, which = 1:6)
# car::powerTransform(lm_fit_reg)

# add predictions to test set
lm_pred <- highered_test %>%
  select(ADJ_SALARY) %>%
  bind_cols(predict(lm_fit, highered_test)) %>%
  bind_cols(predict(lm_fit, highered_test, type = "conf_int")) # add 95% confidence intervals

# check a few predictions 
lm_pred %>% head(10)

# linear regression metrics
lm_metrics <- metric_set(rmse, rsq, mae)

lm_metrics_output <- lm_metrics(lm_pred, truth = ADJ_SALARY, estimate = .pred)
lm_metrics_output

#variable importance
lm_fit %>% pull_workflow_fit() %>% vip()



### SIMPLIFIED LINEAR REGRESSION ------------------------------------------------
# split into train/test
set.seed(123)
highered_simple_split <- initial_split(highered_simple, prop = 0.75, strata = ADJ_SALARY)
highered_simple_train <- training(highered_simple_split)
highered_simple_test <- testing(highered_simple_split)

# create recipe
lm_simple_recipe <- recipe(ADJ_SALARY ~., data = highered_simple) %>%
  step_dummy(all_nominal()) %>%
  step_zv(all_predictors()) %>%
  step_corr(all_predictors())

# create workflow
lm_simple_wflow <- workflow() %>%
  add_model(lm_model) %>%
  add_recipe(lm_simple_recipe)

# simplified linear regression model
lm_simple_fit <- fit(lm_simple_wflow, data = highered_simple)

# add predictions to test set
lm_simple_pred <- highered_simple_test %>%
  select(ADJ_SALARY) %>%
  bind_cols(predict(lm_simple_fit, highered_simple_test)) %>%
  bind_cols(predict(lm_simple_fit, highered_simple_test, type = "conf_int")) # add 95% confidence intervals
lm_simple_pred


# linear regression metrics
lm_simple_metrics_output <- lm_metrics(lm_simple_pred, truth = ADJ_SALARY, estimate = .pred)
lm_simple_metrics_output

#variable importance
lm_simple_fit %>% pull_workflow_fit() %>% vip()

### PENALIZED LINEAR REGRESSION WORKFLOW --------------------------------------
# create lasso regression recipe
pen_lm_recipe <- recipe(ADJ_SALARY ~., data = highered_train) %>%
  step_dummy(all_nominal()) %>%
  step_normalize(all_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_corr(all_predictors())

# specify penalized linear regression model
linear_reg_spec <- 
  linear_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")

# create workflow
pen_lm_wflow <- workflow() %>%
  add_recipe(pen_lm_recipe)

# tuning lasso parameters
# create set of bootstrap resamples
set.seed(1234)
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

# pen_lm_metrics <- lasso_grid %>% 
#   collect_metrics() %>% 
#   group_by(.config, .metric)

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

pen_lm_fit <- last_fit(final_lasso, highered_split)
final_pen_lm <- fit(final_lasso, data = highered_train)

# check lasso model metrics
pen_lm_fit %>% collect_metrics()

pen_lm_summary <- tidy(final_pen_lm)
pen_lm_summary


# add predictions to test set
pen_lm_pred <- highered_test %>%
  select(ADJ_SALARY) %>%
  bind_cols(predict(final_pen_lm, highered_test))

# get lasso metrics
lasso_metrics <- lm_metrics(pen_lm_pred, truth = ADJ_SALARY, estimate = .pred)


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
reg_metrics <- lm_metrics(reg_pred, truth = ADJ_SALARY, estimate = .pred)
reg_metrics

#### RANDOM FOREST REGRESSION WORKFLOW -------------------------------------------
# specify random forest model 
rand_forest_ranger_spec <-
  rand_forest() %>%
  set_engine('ranger', num.threads = parallel::detectCores(), importance = "permutation",
             verbose = TRUE) %>%
  set_mode('regression') %>%
  set_args(trees = 200)


# create random forest workflow
rf_wflow <- 
  workflow() %>%
  add_model(rand_forest_ranger_spec) %>%
  add_recipe(lm_recipe)


# fit model
set.seed(100)
plan(multisession)

rf_fit <- 
  fit_resamples(
    rf_wflow,
    highered_folds,
    metrics = metric_set(rmse, rsq, mae),
    control = control_resamples(verbose = TRUE,
                                save_pred = TRUE,
                                extract = function(x) x)
  )

# extract roots
rf_tree_roots <- function(x) {
  map_chr(1:200, ~ranger::treeInfo(x, tree = .)[1, "splitvarName"])
}

rf_roots <- function(x) {
  x %>% 
    select(.extracts) %>%
    unnest(cols = c(.extracts)) %>%
    mutate(fit = map(.extracts, ~.x$fit$fit$fit),
           oob_rmse = map_dbl(fit, ~sqrt(.x$prediction.error)),
           roots = map(fit, ~rf_tree_roots(.))) %>%
    select(roots) %>%
    unnest(cols = c(roots))
}

#plot
rf_roots(rf_fit) %>%
  group_by(roots) %>%
  count() %>%
  arrange(desc(n)) %>%
  filter(n > 75) %>%
  ggplot(aes(fct_reorder(roots, n), n)) +
  geom_col() +
  coord_flip() +
  labs(x = "root", y = "count")



#### BOOSTED TREES WORKFLOW --------------------------------------------------------
# specify xgboost tree regression
boost_tree_xgboost_spec <-
  boost_tree() %>%
  set_engine('xgboost', nthreads = parallel::detectCores()) %>%
  set_mode('regression')

# create workflow
boost_wflow <- workflow() %>%
  add_recipe(lm_recipe) %>%
  add_model(boost_tree_xgboost_spec)

# fit model
set.seed(100)
plan(multisession)

boost_fit <- 
  fit_resamples(
    boost_wflow,
    highered_folds,
    metrics = metric_set(rmse, rsq, mae),
    control = control_resamples(verbose = TRUE,
                                save_pred = TRUE)
  )

# evaluate metrics from random forest, xgboost and other models
collect_metrics(rf_fit) %>%
  bind_rows(collect_metrics(boost_fit)) %>%
  filter(.metric == "rmse") %>%
  mutate(model = c("rf", "boost")) %>%
  select(model, everything())


# out of sample performance
# random forest oos performance
rf_final_fit <- last_fit(rf_wflow, split = highered_split)

# xgboost performance
boost_final_fit <- last_fit(boost_wflow, split = highered_split)


# evaluate performance differences for all models
collect_metrics(rf_final_fit) %>%
  bind_rows(collect_metrics(boost_final_fit)) %>%
  bind_rows(lm_metrics_output) %>%
  bind_rows(reg_metrics) %>%
  filter(.metric == "rmse") %>%
  mutate(model = c("rf", "boost", "lm", "reg_tree")) %>%
  select(model, everything())


# random forest final model
rf_model_fit <- fit(rf_wflow, highered_train) %>% pull_workflow_fit()
rf_model_pred <- (predict(rf_model_fit, highered_test)) # add 95% confidence intervals


# save simple linear model
saveRDS(lm_simple_fit, "./lm_simple_model.rds")
