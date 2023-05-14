# Install the necessary packages
# install.packages("xgboost")
# install.packages("caret")
# install.packages("mlbench")


# Load the required libraries
library(xgboost)
library(caret) #for confusion matrix
library(mlbench) #for digits
install.packages("palmerpenguins")
library(palmerpenguins)
data(penguins)
library(palmerpenguins)
library(dplyr)
library(tidyverse)
library(xgboost)
library(caret)

# Load the dataset
data("DNA")
penguins

# Remove missing values
penguins <- na.omit(penguins)

# Convert species to numeric
penguins$species

# Convert island and sex to numeric using one-hot encoding
library(fastDummies)
penguins <- dummy_cols(penguins, select_columns = c("island", "sex")) # Create the dummy variable data frame
penguins <- penguins %>% select(-island, -sex)

data=penguins

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(nrow(data), nrow(data) * 0.7)  # 70% for training
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]


# Prepare the datas matrix and label for XGBoost
train_matrix <- as.matrix(data.frame(lapply(train_data[, -1], function(x) as.numeric(as.character(x))))) # Exclude the target column
train_label <- train_data$species  # Target column (response variable)
test_matrix <-  as.matrix(data.frame(lapply(test_data[, -1], function(x) as.numeric(as.character(x)))))
test_label <- test_data$species
levels(train_label)


# Train the XGBoost model
num_class=length(unique(train_label))
train_label_num <- as.integer(train_label) - 1 #Xgboost in multiclass wants integer starting from 0
test_label_num <- as.integer(test_label) - 1 #Xgboost in multiclass wants integer starting from 0
xgb_model <- xgboost(data = train_matrix, label = train_label_num, nrounds = 100, objective="multi:softprob",
                     num_class=num_class)
xgb_model$pred

#
source("feature_code/R/vcr_custom.R")
library(classmap)

#model evaluation on train
train_label_pred_num <- predict(xgb_model, train_matrix)
train_label_pred <- levels(train_label)[train_label_pred_num + 1]
train_label_pred <- factor(train_label_pred, levels = levels(train_label))
train_label_pred
confusionMatrix(train_label_pred, train_label)
train_probabilities <- predict(xgb_model, train_matrix, outputmargin = FALSE)

train_posteriors = predict(xgb_model,train_matrix,reshape=T)
train_posteriors = as.data.frame(train_posteriors)
colnames(train_posteriors) = levels(train_label)
train_label_pred = apply(train_posteriors,1,function(x) colnames(train_posteriors)[which.max(x)])
confusionMatrix(as.factor(train_label_pred), train_label)

vcr.out=vcr.custom.train(Xnew=train_matrix, ynew=train_label, train_posteriors)
silplot(vcr.out)


#model evaluation on test
test_label_pred_num <- predict(xgb_model, test_matrix)
test_label_pred <- levels(test_label)[test_label_pred_num + 1]
test_label_pred <- factor(test_label_pred, levels = levels(test_label))
test_label_pred
confusionMatrix(test_label_pred, test_label)

test_posteriors = predict(xgb_model,test_matrix,reshape=T)
test_posteriors = as.data.frame(test_posteriors)
colnames(test_posteriors) = levels(test_label)
test_label_pred = apply(test_posteriors,1,function(x) colnames(test_posteriors)[which.max(x)])
confusionMatrix(as.factor(test_label_pred), test_label)

vcr.out.test=vcr.custom.newdata(Xnew=test_matrix, ynew=test_label, probs=test_posteriors, vcr.custom.train.out = vcr.out)
silplot(vcr.out.test)


