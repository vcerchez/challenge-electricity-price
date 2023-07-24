library(readr)
library(dplyr)

# Load data
X_train_raw <- read.csv("./data/X_train.csv")
X_test_raw <- read.csv("./data/X_test.csv")
y_train <- read.csv("./data/y_train.csv")

# mask data for FR and DE
mask_train_FR <- X_train_raw$COUNTRY == "FR"
mask_test_FR <- X_test_raw$COUNTRY == "FR"

# Drop unused cols
columns_to_drop <- c("ID", "DAY_ID", "COUNTRY", "DE_FR_EXCHANGE", "FR_NET_IMPORT", "DE_NET_IMPORT")

X_train <- X_train_raw %>% select(!one_of(columns_to_drop))
X_test <- X_test_raw %>% select(!one_of(columns_to_drop))

# Standartize vars
mean_ <- sapply(X_train, mean, na.rm = TRUE)
sd_ <- sapply(X_train, sd, na.rm = TRUE)

X_train <- as.data.frame(scale(X_train, center = mean_, scale = sd_))
X_test <- as.data.frame(scale(X_test, center = mean_, scale = sd_))

# Fill-in NAs
X_train <- X_train %>% replace(is.na(.), 0)
X_test <- X_test %>% replace(is.na(.), 0)
