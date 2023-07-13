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
X_train <- X_train %>% select(-COUNTRY)
X_test <- X_test %>% select(-COUNTRY)
