# Possible scenarios:
# 1. A lot of wind -> coal power stations become marginal -> price drops and is
#    related to coal
# 2. Strong consumption together with weak production and difficulties to import
#    -> risk of blackout -> price of electricity consumption reduction becomes
#    marginal -> price increases abruptly
# 3. 

# data preprocessing
source("src/data_preprocessing.R")
library(xgboost)
library(Metrics)
library(ggplot2)
library(Ckmeans.1d.dp)

set.seed(2023)

# Bin the variables and map to the desired values
bin_and_map <- function(x, breaks = 5) {
  bins <- cut(x, breaks = breaks, labels = c(-2, -1, 0, 1, 2), include.lowest = TRUE)
  return(as.numeric(as.character(bins)))
}

# Apply the function to all numerical columns
binned <- as.data.frame(lapply(rbind(X_train, X_test), bin_and_map))
X_train <- head(binned, nrow(X_train))
X_test <- tail(binned, nrow(X_test))

# convert training data to DMatrix
dtrain_FR <- xgb.DMatrix(data = as.matrix(X_train[mask_train_FR, ]), label = y_train[mask_train_FR, "TARGET"])
dtrain_DE <- xgb.DMatrix(data = as.matrix(X_train[!mask_train_FR, ]), label = y_train[!mask_train_FR, "TARGET"])

# cross-validation
params_xgb <- list(max_depth = 2, eta = 0.01, nthread = 1, min_child_weight = 10,
                   objective = "reg:squarederror")
cv_FR <- xgb.cv(params = params_xgb, data = dtrain_FR, nrounds = 1000, nfold = 5, print_every_n = 10, early_stopping_rounds = 50)
cv_DE <- xgb.cv(params = params_xgb, data = dtrain_DE, nrounds = 1000, nfold = 5, print_every_n = 10, early_stopping_rounds = 50)

# CV with corr metric
evalcorr <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  corr <- cor(preds, labels, method = "spearman")
  corr <- ifelse(is.na(corr), 0, corr)
  return(list(metric = "corr", value = corr))
}
set.seed(2023)
xgb.cv(params = params_xgb, data = dtrain_FR, nrounds = cv_FR$best_iteration, nfold = 5, feval = evalcorr, maximize = TRUE, print_every_n = 10, early_stopping_rounds = NULL)
set.seed(2023)
xgb.cv(params = params_xgb, data = dtrain_DE, nrounds = cv_DE$best_iteration, nfold = 5, feval = evalcorr, maximize = TRUE, print_every_n = 10, early_stopping_rounds = NULL)

# train model
model_FR <- xgb.train(params = params_xgb, data = dtrain_FR, nrounds = cv_FR$best_iteration)
model_DE <- xgb.train(params = params_xgb, data = dtrain_DE, nrounds = cv_DE$best_iteration)

# predictions
y_hat_train <- vector(mode="numeric", length=nrow(X_train_raw))
y_hat_test <- vector(mode="numeric", length=nrow(X_test_raw))

y_hat_train[mask_train_FR] <- predict(model_FR, as.matrix(X_train[mask_train_FR, ]))
y_hat_train[!mask_train_FR] <- predict(model_DE, as.matrix(X_train[!mask_train_FR, ]))

y_hat_test[mask_test_FR] <- predict(model_FR, as.matrix(X_test[mask_test_FR, ]))
y_hat_test[!mask_test_FR] <- predict(model_DE, as.matrix(X_test[!mask_test_FR, ]))

# RMSE
rmse_train_FR <- rmse(y_train[mask_train_FR, "TARGET"], y_hat_train[mask_train_FR]) %>% round(., 3)
rmse_train_DE <- rmse(y_train[!mask_train_FR, "TARGET"], y_hat_train[!mask_train_FR]) %>% round(., 3)
rmse_train <- rmse(y_train$TARGET, y_hat_train) %>% round(., 3)
print(paste("RMSE on training set for FR: ", rmse_train_FR))
print(paste("RMSE on training set for DE: ", rmse_train_DE))
print(paste("RMSE on training set: ", rmse_train))

# Spearman correlation for the predictions on the training set
cor_train_FR <- cor(y_train[mask_train_FR, "TARGET"], y_hat_train[mask_train_FR], method = "spearman") %>% round(., 3)
cor_train_DE <- cor(y_train[!mask_train_FR, "TARGET"], y_hat_train[!mask_train_FR], method = "spearman") %>% round(., 3)
cor_train <- cor(y_train$TARGET, y_hat_train, method = "spearman") %>% round(., 3)
print(paste("Spearman corr on training set for FR: ", cor_train_FR))
print(paste("Spearman corr on training set for DE: ", cor_train_DE))
print(paste("Spearman corr on training set: ", cor_train))

# export predictions on the test set
to_export <- tibble(ID = X_test_raw$ID, TARGET = y_hat_test)
# set EOL to CRLF to avoid conflicts with git
write_csv(to_export, "./data/y_hat_test.csv", eol = "\r\n")
