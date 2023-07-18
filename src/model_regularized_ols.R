# data preprocessing
source("./src/data_preprocessing.R")

library(glmnet)
library(caret)

set.seed(2023)

df <- data.frame(X_train)
df$TARGET <- y_train$TARGET

# split data for FR and DE
df_FR <- df[mask_train_FR, ]
df_DE <- df[!mask_train_FR, ]

model_FR = train(
 form = TARGET ~ .,
 data = df_FR,
 trControl = trainControl(method = "cv", number = 5),
 method = "glmnet",
 tuneGrid = expand.grid(lambda = seq(0.001, 10.3, length.out = 100),
                        alpha = seq(0.0, 1, length.out = 6))
)

model_FR

ggplot(model_FR)

model_DE = train(
  form = TARGET ~ .,
  data = df_DE,
  trControl = trainControl(method = "cv", number = 5),
  method = "glmnet",
  tuneGrid = expand.grid(lambda = seq(0.001, 1, length.out = 100),
                         alpha = seq(0.2, 1, length.out = 6))
)

model_DE

ggplot(model_DE)

# predict
y_hat_train <- vector(mode="numeric", length=nrow(X_train_raw))
y_hat_test <- vector(mode="numeric", length=nrow(X_test_raw))

y_hat_train[mask_train_FR] <- predict(model_FR, X_train[mask_train_FR, ])
y_hat_train[!mask_train_FR] <- predict(model_DE, X_train[!mask_train_FR, ])

y_hat_test[mask_test_FR] <- predict(model_FR, X_test[mask_test_FR, ])
y_hat_test[!mask_test_FR] <- predict(model_DE, X_test[!mask_test_FR, ])

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
