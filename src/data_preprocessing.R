library(readr)
library(dplyr)

# Load data
X_train_raw <- read.csv("./data/X_train.csv")
X_test_raw <- read.csv("./data/X_test.csv")
y_train <- read.csv("./data/y_train.csv")

# Fill-in NAs
X_train <- X_train_raw %>% replace(is.na(.), 0)
X_test <- X_test_raw %>% replace(is.na(.), 0)

# Drop unused cols
columns_to_drop <- c("ID", "DAY_ID", "COUNTRY", "DE_FR_EXCHANGE", "FR_NET_IMPORT", "DE_NET_IMPORT")
X_train <- X_train %>% select(!one_of(columns_to_drop))
X_test <- X_test %>% select(!one_of(columns_to_drop))
