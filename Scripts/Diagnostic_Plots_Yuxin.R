# load necessary packages
library(car)
library(caret)
#######################
# Diagnostic Plots

# setting up plot environment
par(mfrow = c(2,2), mar = c(4,4,7,2) + 0.1)
# Scatter plot of Actual versus predicted
plot(train_sub$Disaster_Frequency, RF_train_predict$.pred,
     main = "Actual versus Predicted",
     ylab = "Prediction Result",
     xlab = "Actual Result")
#qq plot
qqPlot(RF_train_predict$.pred, main = "QQ-norm Plot")
# fitted versus residual plot
train_sub$Disaster_Frequency - RF_train_predict$.pred
fitted_resid_plot <- plot(RF_train_predict$.pred, resid,
                          ylab = "Residuals",
                          xlab = "Fitted Values",
                          main = "Fitted versus Residuals")
abline(h = 0, col = 'blue', lty = 2)
#calcualte standard residuals
stand_resid <- (resid - mean(resid))/sd(resid)
# fitted versus standard residual plot
plot(RF_train_predict$.pred, stand_resid,
     ylab = "Standard Residuals",
     xlab = "Fitted Values",
     main = "Fitted versus Standard Residuals")
abline(h = 0, col = 'blue', lty = 2)
# setting a common title
mtext("Diagnostic Plots", side = 3, line = -2, cex = 1.5, outer = TRUE)