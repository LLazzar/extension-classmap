# Install the necessary packages
# install.packages("xgboost")
# install.packages("caret")
# install.packages("mlbench")


# Load the required libraries
library(xgboost)
library(caret) #for confusion matrix
library(mlbench) #for digits

# Load the dataset
data("Ionosphere")
data=Glass

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(nrow(data), nrow(data) * 0.7)  # 70% for training
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Prepare the datas matrix and label for XGBoost
train_matrix <- as.matrix(train_data[, -10])  # Exclude the target column
train_label <- train_data[, 10]  # Target column (response variable)
test_matrix <- as.matrix(test_data[, -10])
test_label <- test_data[,10]
levels(train_label)


# Train the XGBoost model
num_class=length(unique(train_label))
train_label_num <- as.integer(train_label) - 1 #Xgboost in multiclass wants integer starting from 0
test_label_num <- as.integer(test_label) - 1 #Xgboost in multiclass wants integer starting from 0
xgb_model <- xgboost(data = train_matrix, label = train_label_num, nrounds = 100, objective="multi:softmax",
                     num_class=num_class)
xgb_model

#model evaluation on train
train_label_pred_num <- predict(xgb_model, train_matrix)
train_label_pred <- levels(train_label)[train_label_pred_num + 1]
train_label_pred <- factor(train_label_pred, levels = levels(train_label))
train_label_pred
confusionMatrix(train_label_pred, train_label)

train_probabilities <- predict(xgb_model, train_matrix, type = "prob")

#model evaluation on test
test_label_pred_num <- predict(xgb_model, test_matrix)
test_label_pred <- levels(test_label)[test_label_pred_num + 1]
test_label_pred <- factor(test_label_pred, levels = levels(test_label))
test_label_pred
confusionMatrix(test_label_pred, test_label)
