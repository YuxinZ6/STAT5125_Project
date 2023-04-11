
library(ggplot2)
library(poissonreg)

#### MODELS ####

# subsetting train dataset. excluding ISO3 and Year
train_sub <- train %>% select(-c(ISO3))

# Poisson Generalized Linear Regression Model with Lasso Penalty
poi_recipe <- recipe(Disaster_Frequency ~., data = train_sub) %>%
  setp_dummy(all_factor_predictors()) %>%
  step_normalize(all_predictors())
# defining model using poisson_reg()
poi_parsnip <-  poisson_reg(penalty = "lasso") %>%
  set_mode("regression") %>%
  set_engine("glm")
# defining workflow with model and recipe
poi_workflow <- workflow() %>%
  add_model(poi_parsnip) %>%
  add_recipe(poi_recipe)
# Cross validation on train dataset
poi_result <- poi_workflow %>%
  fit_resamples(
    resamples = vfold_cv(train_sub, v = 10),
    metrics = metric_set(rmse, rsq, rsq_trad),
    
  )
# showing results 
poi_result %>% collect_metrics()

poi_fitted <- poi_workflow %>% fit(train_sub)
poi_fitted

#  Poisson Generalized Linear Model with Elastic Net Penalty
# defining recipe
poi_recipe_enp <- recipe(Disaster_Frequency ~., data = train_sub) %>%
  step_normalize(all_predictors())
# defining model using poisson_reg with elastic net penalty
poi_parsnip_enp <-  poisson_reg(penalty = "elastic_net") %>%
  set_mode("regression") %>%
  set_engine("glm")
# defining workflow with model and recipe
poi_workflow_enp <- workflow() %>%
  add_model(poi_parsnip_enp) %>%
  add_recipe(poi_recipe_enp)
# cross validation on train dataset
poi_result_enp <- poi_workflow_enp %>%
  fit_resamples(
    resamples = vfold_cv(train_sub, v = 10),
    metrics = metric_set(rmse, rsq, rsq_trad),
    
  )
# showing results
poi_result_enp %>% collect_metrics()


#### GRAPHS ####

# line graph, count of disasters by Year,  color code by country (t.s. plot)
# creating aggregate dataset, grouping by Year and ISO3, summary of total disaster
df_aggregate_1 <- df_long %>% 
  group_by(Year, ISO3) %>% 
  summarise(total_disaster = sum(Disaster_Frequency)) %>%
  filter(total_disaster > 500)

# convert Year to numeric
df_aggregate_1$Year <- as.numeric(as.character(df_aggregate_1$Year))

# ggplot using aggregate dataset
ggplot(df_aggregate_1, aes(x = Year, y = total_disaster, color = ISO3)) +
  geom_line() +
  geom_point() +
  labs(title = "Count of Disaster by Year (disasters > 500)",
       x = "Year", 
       y = "Sum of Disaster",
       color = "Country") 

#line graph of aggregated count of disasters across years 
# using df_wide to create aggregate dataset, group by Year, summary of disaster
df_aggregate_2 <- df_wide %>% 
  group_by(Year) %>% 
  summarize(total_disaster_frequency = sum(Disaster_Frequency))

# converting Year to numeric
df_aggregate_2$Year <- as.numeric(as.character(df_aggregate_2$Year))

# ggplot using aggregate dataset
ggplot(df_aggregate_2, aes(x = Year, y = total_disaster_frequency)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = " Disaster Frequency by Year ",
       x = "Year",
       y = "Sum Disaster Frequency")


