# Yuxin: 
# models are as the following:
# Random Forest, XG-Boost
# visualizations are as the following: 
# scatter plot

###################################################################
# Random Forest

# load necessary packages
library(randomForest)
library(parsnip)
library(workflows)
library(parallel)
library(doParallel)
library(tidymodels)
library(yardstick)

####Result of the following code, up until line 62 has been saved under###
####file = "RandomForest_Prediction.rds". Run line 65 to obtain result.###

# set the seed
set.seed(123457)
# define parsnip, set random forest with mtry, trees, and min_n hyperparameters
# as tuning. To be tuned later.
parsnip_RF <- rand_forest(mtry = tune("mtry"),
                          trees = tune("trees"),
                          min_n = tune("min_n")) %>%
  set_engine('randomForest') %>%
  set_mode('regression') #set random forest to regression

# define workflow
workflow_RF <- workflow() %>% 
  add_model(parsnip_RF) %>%
  add_formula(Disaster_Frequency ~ .)

# define hyperparameter tuning grid to be experimented
hyperparam_tune_grid <- crossing(min_n = seq(10, 50, by = 20),
                               mtry = seq(10, 50, by = 20),
                               trees = c(100, 500, 1000))
# define cross validation
rf_cv <- train %>% vfold_cv(v = 10, times = 2)
# define metrics to be used
RF_metrics <- metric_set(rmse, mae, rsq_trad)

# assigning resources
cl <- makePSOCKcluster(6)
registerDoParallel(cl)
time1 <- Sys.time() #save current system time

#run the 10 fold cross validation for each combination of hyperparameters
rf_tuning <- workflow_RF %>% 
  tune_grid(resamples = rf_cv, 
            grid = hyperparam_tune_grid,
            metrics = RF_metrics) %>%
  collect_metrics()

time2 <- Sys.time() #save current system time

(diff <- time2 - time1) #obtain total run time

stopCluster(cl)
##################################################################
save(rf_tuning, file = "RandomForest_Prediction.rds")
RF_prediction <- load("RandomForest_Prediction.rds")

Best_result1 <- rf_tuning %>% 
  filter(!(.metric == "rsq_trad")) %>%
  group_by(.metric) %>% 
  slice_min(mean)
Best_result2 <- rf_tuning %>% 
  filter(.metric == "rsq_trad") %>%
  group_by(.metric) %>% 
  slice_max(mean)
Best_result <- Best_result1 %>% rbind(Best_result2)
Best_result

