

##########Must run knn_prediction_code.R before running this R script file#########

# Out of sample metric: sMAPE
# sMAPE = 1/n * (Σ|( Actual — Predicted)|/(Actual + Predicted/2))
# sMAPE is useful when actual value contains zero, which in our case that 
# disaster frequency is often 0.
# using MAPE with actual value of 0 will result in infinity error because value is
# not divisible by 0.
# an alternative to this is sMAPE. 
# sMAPE lower bound is 0% and upper bound is 200%.

# calculate sMAPE for each models on both the train set and test set
# random forest
RF_train_sMAPE <- RF_train_predict %>% 
  smape(truth = train_sub$Disaster_Frequency, estimate = .pred) %>%
  select(.estimate) %>% pull()
RF_test_sMAPE <- RF_test_predict %>% 
  smape(truth = test_sub$Disaster_Frequency, estimate = .pred) %>%
  select(.estimate) %>% pull()
# xg-boost
boost_train_sMAPE <- boost_train_predict %>% 
  smape(truth = train_sub$Disaster_Frequency, estimate = .pred) %>%
  select(.estimate) %>% pull()
boost_test_sMAPE <- boost_test_predict %>% 
  smape(truth = test_sub$Disaster_Frequency, estimate = .pred) %>%
  select(.estimate) %>% pull()
# poisson 
poi_train_sMAPE <- poi_fitted %>% predict(train_sub) %>% 
  smape(truth = train_sub$Disaster_Frequency, estimate = .pred) %>%
  select(.estimate) %>% pull()
poi_test_sMAPE <- poi_fitted %>% predict(test_sub) %>% 
  smape(truth = test_sub$Disaster_Frequency, estimate = .pred) %>%
  select(.estimate) %>% pull()
# knn
knn5_train_sMAPE <- knn_5 %>% predict(train_sub) %>% 
  smape(truth = train_sub$Disaster_Frequency, estimate = .pred) %>%
  select(.estimate) %>% pull()
knn5_test_sMAPE <- knn_5t %>% 
  smape(truth = test_sub$Disaster_Frequency, estimate = .pred) %>%
  select(.estimate) %>% pull()
knn10_train_sMAPE <- knn_10 %>% predict(train_sub) %>% 
  smape(truth = train_sub$Disaster_Frequency, estimate = .pred) %>%
  select(.estimate) %>% pull()
knn10_test_sMAPE <- knn_10t %>% 
  smape(truth = test_sub$Disaster_Frequency, estimate = .pred) %>%
  select(.estimate) %>% pull()
knn20_train_sMAPE <- knn_20 %>% predict(train_sub) %>% 
  smape(truth = train_sub$Disaster_Frequency, estimate = .pred) %>%
  select(.estimate) %>% pull()
knn20_test_sMAPE <- knn_20t %>% 
  smape(truth = test_sub$Disaster_Frequency, estimate = .pred) %>%
  select(.estimate) %>% pull()

# bind_rows(RF_train_sMAPE, RF_test_sMAPE, boost_train_sMAPE,
#           boost_test_sMAPE, poi_train_sMAPE, poi_test_sMAPE,
#           knn5_train_sMAPE, knn5_test_sMAPE, knn10_train_sMAPE, 
#           knn10_test_sMAPE, knn20_train_sMAPE, knn20_test_sMAPE)

# organize results into one dataframe
model_selection_result <- 
  data.frame(prediction = c("RF_train", "RF_test", "Boost_train", "Boost_test", 
                            "Poisson_train", "Poisson_test", "knn5_train", 
                            "knn5_test", "knn10_train", "knn10_test",
                            "knn20_train", "knn20_test"),
             value = c(RF_train_sMAPE, RF_test_sMAPE, boost_train_sMAPE,
                       boost_test_sMAPE, poi_train_sMAPE, poi_test_sMAPE,
                       knn5_train_sMAPE, knn5_test_sMAPE, knn10_train_sMAPE, 
                       knn10_test_sMAPE, knn20_train_sMAPE, knn20_test_sMAPE))
model_selection_result
