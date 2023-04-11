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
# aggregating the co2 emission across all years and all sectors, 
# and rank by category.
df_agg <- df_long %>% 
  group_by(ISO3, Category) %>%
  reframe(emission = sum(`Value`)) %>%
  mutate_if(is.character, as.factor) %>%
  group_by(Category) %>%
  arrange(desc(emission)) %>%
  slice(1:10)

# CO2 by Country
ggplot(df_agg,aes(x = ISO3, y = emission, fill = Category)) + 
  geom_bar(stat = "identity")  +
  # facet_wrap(~Category)+
  labs(
    title = "CO2 emissions by Category of Enterprise for Each Country",
    x = "Country", 
    y = "Metric Tons of CO2 Emission"
  ) 

# map
library(maps)

world <- map_data("world")

bind <- df_long %>% left_join(Country_label, by="ISO3") %>% 
  select(c(Country,Value)) %>% 
  group_by(Country) %>% reframe(Value = sum(Value)) %>% 
  # group_by(Country) %>% first(Value) %>% 
  glimpse()
world <- world %>% full_join(bind, by = c("region" ="Country"))
world <- world %>% mutate_all(~ ifelse(is.na(.), 0, .))

ggplot(world, aes(long, lat, group=group, fill = log(Value+1))) +
  geom_polygon(color="blue") +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed() + 
  ggtitle("Map of CO2 Emission Recorded in Dataset")




