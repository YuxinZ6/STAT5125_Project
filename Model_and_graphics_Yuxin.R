# Yuxin: 
# models are as the following:
# Random Forest, XG-Boost
# visualizations are as the following: 
# scatter plot

#Code from class, run when connection failed
unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

unregister_dopar()

###################################################################
#####Random Forest#####

# load necessary packages
library(randomForest)
library(parsnip)
library(workflows)
library(parallel)
library(doParallel)
library(tidymodels)
library(yardstick)
library(ggplot2)
library(xgboost)
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

# define random forest recipe, code all nominal predictors dummy variables and 
# normalize all numeric predictors.
RF_recipe <- 
  recipe(formula = Disaster_Frequency ~ ., data = train) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors())

# define workflow
workflow_RF <- workflow() %>% 
  add_model(parsnip_RF) %>%
  add_recipe(RF_recipe)

# define hyperparameter tuning grid to be experimented
hyperparam_tune_grid <- crossing(min_n = seq(10, 50, by = 20),
                                 mtry = seq(10, 50, by = 20),
                                 trees = c(100, 500, 1000))
# define cross validation
rf_cv <- train %>% vfold_cv(v = 10, times = 2)
# define metrics to be used
RF_metrics <- metric_set(rmse, mae, rsq_trad)
################################
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
################################
save(rf_tuning, file = "RandomForest_Prediction.rds")
RF_prediction <- load("RandomForest_Prediction.rds")
rf_tuning 

# selecting the best hyperparameter combination
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
#################################
# Fitting the train data with best hyperparameter combination
# Since both rmse and rsq_trad supports mtry = 50, trees = 1000, 
# and min_n = 10, we fit that to the train set. 
parsnip_RF_train <- rand_forest(mtry = 50,
                                trees = 1000,
                                min_n = 10) %>%
  set_engine('randomForest') %>%
  set_mode('regression')

# define workflow
workflow_RF_train <- workflow() %>% 
  add_model(parsnip_RF_train) %>%
  add_recipe(RF_recipe)

# fit to the train data
RF_fit <- workflow_RF_train %>% fit(data = train) 
RF_fit 

RF_fit %>% predict(test) %>% mutate(truth = train$Disaster_Frequency, 
                                    estimate = .pred)

###################################################################
#####XG-Boost#####
# 
# # XG-boost requires train object and test object column names and column
# # order to be exactly the same. 
# train_sorted <- train[, order(names(train))]
# test_sorted <- test[, order(names(test))]

# set the seed
set.seed(123457)
# define parsnip, set random forest with mtry, trees, and min_n hyperparameters
# as tuning. To be tuned later.
parsnip_boost <- boost_tree(tree_depth = tune("tree_depth"), 
                            learn_rate = tune("learn_rate"), 
                            min_n = tune("min_n")) %>%
  set_engine('xgboost') %>%
  set_mode('regression')

# define recipe for boosting, one hot encode dummy variable for nominal predictors
# and normalize all numeric predictors. 
boost_recipe <- 
  recipe(formula = Disaster_Frequency ~ ., data = train) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors())


# define workflow
workflow_boost <- workflow() %>% 
  add_model(parsnip_boost) %>%
  add_recipe(boost_recipe)

# define hyperparameter tuning grid to be experimented
hyperparam_tune_grid_boost <- crossing(min_n = seq(10, 50, by = 20),
                                       tree_depth = c(5, 10, 15),
                                       learn_rate = c(0.01, 0.05, 0.1))
# define cross validation
boost_cv <- train %>% vfold_cv(v = 10, times = 2)
# define metrics to be used
boost_metrics <- metric_set(rmse, mae, rsq_trad)
################################
# assigning resources
cl <- makePSOCKcluster(6)
registerDoParallel(cl)
time1 <- Sys.time() #save current system time

#run the 10 fold cross validation for each combination of hyperparameters
boost_tuning <- workflow_boost %>% 
  tune_grid(resamples = boost_cv, 
            grid = hyperparam_tune_grid_boost,
            metrics = boost_metrics) %>%
  collect_metrics()

time2 <- Sys.time() #save current system time

(diff <- time2 - time1) #obtain total run time

stopCluster(cl)

#################################
save(boost_tuning, file = "XGBoost_Prediction.rds")
Boost_prediction <- load("XGBoost_Prediction.rds")
boost_tuning 

Best_result3 <- boost_tuning %>% 
  filter(!(.metric == "rsq_trad")) %>%
  group_by(.metric) %>% 
  slice_min(mean)
Best_result4 <- boost_tuning %>% 
  filter(.metric == "rsq_trad") %>%
  group_by(.metric) %>% 
  slice_max(mean)
Best_result5 <- Best_result3 %>% rbind(Best_result4)
Best_result5
#################################
# Fitting the train data with best hyperparameter combination
# all metrics support the combination of min_n = 10, 
# tree_depth = 10, and learn_rate = 0.1, we use it as our model for train set. 
parsnip_Boost_train <- boost_tree(tree_depth = 10, 
                                  learn_rate = 0.1, 
                                  min_n = 10) %>%
  set_engine('xgboost') %>%
  set_mode('regression')

# define workflow
workflow_Boost_train <- workflow() %>% 
  add_model(parsnip_Boost_train) %>%
  add_recipe(boost_recipe)

# fit to the train data
boost_fit <- workflow_Boost_train %>% fit(data = train) 
boost_fit 

###################################################################
######Visualization#####
# optimize data for scatterplot
scatterplot_df <- df_long %>% group_by(ISO3, Year) %>%
  reframe(emission = sum(`Value`),
          Disaster_Frequency = first(Disaster_Frequency)) %>%
  glimpse()

# convert country (IOS3) as factor
scatterplot_df$ISO3 <- as.factor(scatterplot_df$ISO3)

# plot the scatter plot
scatterplot_df %>% ggplot(aes(x = log(emission + 0.0001), 
                              y = Disaster_Frequency)) + 
  geom_point() + 
  geom_point(data = scatterplot_df[scatterplot_df$Disaster_Frequency>10, ],
             aes(x = log(emission + 0.0001), 
                 y = Disaster_Frequency, 
                 color = ISO3)) +
  facet_wrap(~Year) +
  ylab("Disaster Count") + 
  xlab("Log of Emission (Log of Metric Tons of CO2)") +
  ggtitle("Emission versus Disaster Count for Each Country")

