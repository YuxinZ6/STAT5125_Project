#Jen: K-Nearest and Visualizations
library(kknn)
library(ggplot2)
# Goal:
# K-Nearest Neighbors Regression and cross validation to determine the best k
# Visuals: 
# World map and co2 emission (optional)
#     aggregating the co2 emission across all years, and rank by country.
#     ranking is going to be done by color. (gradient color, red is worse, while green is better)
# CO2 emission by country (do first, if time then do map)
# Bar graphs of *df_long* category column, x_axis is year. 
#    facet wrap by category


##### Model #####
# Set seed
set.seed(123457)

# k=5 with standardization
knn_parsnip_5 <- nearest_neighbor() %>% 
  set_mode("regression") %>%
  set_engine("kknn", neighbors = 5)

knn_recipe_5 <- recipe(Disaster_Frequency ~ .,
                       data = train) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

knn_workflow_5 <- workflow() %>%
  add_model(knn_parsnip_5) %>%
  add_recipe(knn_recipe_5)

knn_5s <- knn_workflow_5 %>% fit(train)


# k=10 with standardization
knn_parsnip_10 <- nearest_neighbor() %>% 
  set_mode("regression") %>%
  set_engine("kknn", neighbors = 10)

knn_recipe_10 <- recipe(Disaster_Frequency ~ .,
                       data = train) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

knn_workflow_10 <- workflow() %>%
  add_model(knn_parsnip_10) %>%
  add_recipe(knn_recipe_10)

knn_10 <- knn_workflow_10 %>% fit(train)


# k=20 with standardization
knn_parsnip_20 <- nearest_neighbor() %>% 
  set_mode("regression") %>%
  set_engine("kknn", neighbors = 20)

knn_recipe_20 <- recipe(Disaster_Frequency ~ .,
                        data = train) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

knn_workflow_20 <- workflow() %>%
  add_model(knn_parsnip_20) %>%
  add_recipe(knn_recipe_20)

knn_20 <- knn_workflow_20 %>% fit(train)


# Cross validation
# k=5
knn_val1<- knn_workflow_5 %>%
  fit_resamples(
    resamples = vfold_cv(train, v = 10),
    metrics = metric_set(rmse, rsq, mae,rsq_trad),
  )
knn_val1 %>% collect_metrics()
# k=10
knn_val2 <- knn_workflow_10 %>%
  fit_resamples(
    resamples = vfold_cv(train, v = 10),
    metrics = metric_set(rmse, rsq, mae,rsq_trad),
     )
knn_val2 %>% collect_metrics()

# k=20
knn_val3 <- knn_workflow_10 %>%
  fit_resamples(
    resamples = vfold_cv(train, v = 10),
    metrics = metric_set(rmse, rsq, mae,rsq_trad),
  )
knn_val3 %>% collect_metrics()


metrics_knn <- bind_rows(
  knn_val1 %>% collect_metrics(),
  knn_val2 %>% collect_metrics(),
  knn_val3 %>% collect_metrics(),
  .id = "Model"
)
metrics_knn


##### Visuals #####
# Map maybe 

# aggregating the co2 emission across all years, and rank by country.
df_agg <- df_long %>% 
  group_by(Year, ISO3) %>% arrange(desc(Value))  


test_df <- df_long %>% group_by(ISO3, Year) %>%
  reframe(emission = sum(`Value`),
          Disaster_Frequency = first(Disaster_Frequency))

# CO2 by Country
ggplot(df_long,aes(x=ISO3, y= emission)) + 
  geom_line()  +
  facet_wrap(~Category)+
  labs(
    title = "CO2 emissions by Country",
    x = "Country", 
    y = "CO2 emissions (Units?)"
  )


# Bar of bar graphs of *df_long* category column, x_axis is year. 
ggplot(df_long,aes(x=Year, fill=ISO3)) + 
  geom_bar() +
  facet_wrap(~Category) +
  labs(
    title = "CO2 emissions by Year ",
    x = "Year", 
    y = "CO2 emissions"
  )





