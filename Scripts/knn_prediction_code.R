knn_5t <- knn_5 %>% predict(test_sub)
knn_10t <- knn_10 %>% predict(test_sub)
knn_20t <- knn_20 %>% predict(test_sub)


rsq_manual <- function (x, y){
  cor(x, y) ^ 2
}
knn_5t_rsq <- rsq_manual(test_sub$Disaster_Frequency, knn_5t$.pred)
knn10t_rsq <- rsq_manual(test_sub$Disaster_Frequency, knn_10t$.pred)
knn20t_rsq <- rsq_manual(test_sub$Disaster_Frequency, knn_20t$.pred)
knn_5t_mae <- calculate_mae(test_sub$Disaster_Frequency, knn_5t$.pred)
knn_10t_mae <- calculate_mae(test_sub$Disaster_Frequency, knn_10t$.pred)
knn_20t_mae <- calculate_mae(test_sub$Disaster_Frequency, knn_20t$.pred)
knn_5t_rmse <- calculate_rmse(test_sub$Disaster_Frequency, knn_5t$.pred)
knn_10t_rmse <- calculate_rmse(test_sub$Disaster_Frequency, knn_10t$.pred)
knn_20t_rmse <- calculate_rmse(test_sub$Disaster_Frequency, knn_20t$.pred)
data.frame(models = c("knn5", "knn10", "knn20"),
           rsq = c(knn_5t_rsq, knn10t_rsq, knn20t_rsq), 
           mae = c(knn_5t_mae, knn_10t_mae, knn_20t_mae),
           rmse = c(knn_5t_rmse, knn_10t_rmse, knn_20t_rmse))