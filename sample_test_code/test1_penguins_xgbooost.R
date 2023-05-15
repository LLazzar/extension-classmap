# Install the necessary packages
# install.packages("xgboost")
# install.packages("caret")
# install.packages("mlbench")
# install.packages("palmerpenguins")

# Load the required libraries
library(xgboost)
library(caret) #for confusion matrix
library(palmerpenguins)
library(dplyr)
library(tidyverse)
library(xgboost)


# Load the dataset
data(penguins)
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


#
source("feature_code/R/vcr_custom.R")
library(classmap)

#model evaluation on train
#train_label_pred_num <- predict(xgb_model, train_matrix)
#train_label_pred <- levels(train_label)[train_label_pred_num + 1]
#train_label_pred <- factor(train_label_pred, levels = levels(train_label))
#train_label_pred
#confusionMatrix(train_label_pred, train_label)
#train_probabilities <- predict(xgb_model, train_matrix, outputmargin = FALSE)

train_posteriors = predict(xgb_model,train_matrix,reshape=T)
train_posteriors = as.data.frame(train_posteriors)
colnames(train_posteriors) = levels(train_label)
train_label_pred = apply(train_posteriors,1,function(x) colnames(train_posteriors)[which.max(x)])
confusionMatrix(as.factor(train_label_pred), train_label)

vcr.out=vcr.custom.train(y=train_label, train_posteriors)
silplot(vcr.out)

set.seed(123)

# Generate the matrix
distance_matrix <- matrix(runif(233 * 3), nrow = 233, ncol = 3)
vcr.out.withdist=vcr.custom.train(y=train_label, train_posteriors, distToClasses = distance_matrix)
classmap(vcr.out.withdist, whichclass = 3)

#model evaluation on test
#test_label_pred_num <- predict(xgb_model, test_matrix)
#test_label_pred <- levels(test_label)[test_label_pred_num + 1]
#test_label_pred <- factor(test_label_pred, levels = levels(test_label))
#test_label_pred
#confusionMatrix(test_label_pred, test_label)

test_posteriors = predict(xgb_model,test_matrix,reshape=T)
test_posteriors = as.data.frame(test_posteriors)
colnames(test_posteriors) = levels(test_label)
test_label_pred = apply(test_posteriors,1,function(x) colnames(test_posteriors)[which.max(x)])
confusionMatrix(as.factor(test_label_pred), test_label)

vcr.out.test=vcr.custom.newdata(ynew=test_label, probs=test_posteriors, vcr.custom.train.out = vcr.out)
silplot(vcr.out.test)

distance_matrix_test <- matrix(runif(100 * 3), nrow = 100, ncol = 3)
vcr.out.withdisttest=vcr.custom.newdata(ynew = test_label, test_posteriors, newDistToClasses = distance_matrix_test, vcr.custom.train.out = vcr.out.withdist)
classmap(vcr.out.withdisttest, whichclass = 3)


