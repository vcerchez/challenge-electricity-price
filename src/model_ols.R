# data preprocessing
source("./src/data_preprocessing.R")

# fit OLS
model <- lm(y_train$TARGET ~ ., data = X_train)

# predict
y_hat_train <- predict(model, X_train)
y_hat_test <- predict(model, X_test)

# Spearman correlation for the predictions on the training set
cor_train <- cor(y_train$TARGET, y_hat_train, method = "spearman") %>% 
  round(., 3)
print(paste("Spearman corr on training set: ", cor_train))

# export predictions on the test set
to_export <- tibble(ID = X_test_raw$ID, TARGET = y_hat_test)
# set EOL to CRLF to avoid conflicts with git
write_csv(to_export, "./data/y_hat_test.csv", eol = "\r\n")
