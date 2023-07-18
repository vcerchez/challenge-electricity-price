# data preprocessing
source("./src/data_preprocessing.R")
library(Metrics)

# fit OLS
model_FR <- lm(y_train[mask_train_FR, "TARGET"] ~ ., data = X_train[mask_train_FR, ])
model_DE <- lm(y_train[!mask_train_FR, "TARGET"] ~ ., data = X_train[!mask_train_FR, ])

# predict
y_hat_train <- vector(mode="numeric", length=nrow(X_train_raw))
y_hat_test <- vector(mode="numeric", length=nrow(X_test_raw))

y_hat_train[mask_train_FR] <- predict(model_FR, X_train[mask_train_FR, ])
y_hat_train[!mask_train_FR] <- predict(model_DE, X_train[!mask_train_FR, ])

y_hat_test[mask_test_FR] <- predict(model_FR, X_test[mask_test_FR, ])
y_hat_test[!mask_test_FR] <- predict(model_DE, X_test[!mask_test_FR, ])

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
