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
xgb_model <- xgboost(data = train_matrix, label = train_label_num, nrounds = 25, objective="multi:softprob",
                     num_class=num_class, max_depth=1)
xgb.importance(model=xgb_model)


source("feature_code/R/VCR_custom.R")
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

###################################################
#computing pairwise weighted dissimilarities
library(cluster)
importanceraw=xgb.importance(model=xgb_model)
importanceweight=rep(0,ncol(train_matrix))
names(importanceweight)=colnames(train_matrix)
importanceweight["bill_length_mm"]

for (i in 1:nrow(importanceraw)) {
  importanceweight[as.character(importanceraw[i,1])]=as.numeric(importanceraw[i,2])
}

trainpairwisedis=daisy(train_matrix, type = list(symm=6:10), weights = importanceweight)

# daisy_vcr skips the variables with zero weight.
dismat  <- as.matrix(trainpairwisedis)
meandis <- mean(as.vector(dismat), na.rm = TRUE)
dismat[which(is.na(dismat))] <- meandis
#
# Compute neighbors by sorting dissimilarities:
sortNgb <- t(apply(dismat, 1, order))[, -1] #contains indexes of nearest points for each row (row=observation)
sortDis <- t(apply(dismat, 1, sort))[, -1] #contains dimmilarieties instead of indexes
#
# Compute initial fig[i, g] from cases to classes:
#
k=5
yintv=as.numeric(train_label)

distToClass <- matrix(rep(NA, nrow(train_matrix) * 3), ncol = 3)
for (i in seq_len(nrow(train_matrix))) { # loop over all cases in indsv
  for (g in seq_len(3)) { # loop over classes
    ngbg <- which(yintv[sortNgb[i, ]] == g) #getting indexes of all in the same class
    if (length(ngbg) > k) {ngbg <- ngbg[seq_len(k)]} #getting the k nearer
    distToClass[i, g] <- median(sortDis[i, ngbg]) #take the median of the k nearer
  }
}

#to compute for test set, compute dissimilarities with each member of training

newDistToClass <- matrix(rep(NA, nrow(test_matrix) * 3), ncol = 3)

for (itest in 1:nrow(test_matrix)){
  temptrainplusone=rbind(test_matrix[itest,], train_matrix)
  yintv=rbind(as.numeric(test_label)[itest],as.numeric(train_label)) #nb if a label is not in test
  testpairwisedis=daisy(temptrainplusone, type = list(symm=6:10), weights = importanceweight)
  dismat=as.matrix(testpairwisedis)
  sortNgb <- t(apply(dismat, 1, order))[1, -1] #contains indexes of nearest points for each row (row=observation)
  sortDis <- t(apply(dismat, 1, sort))[1, -1] #contains dimmilarieties instead of indexes
  
  for (g in seq_len(3)) { # loop over classes
    ngbg <- which(yintv[sortNgb] == g) #getting indexes of all in the considered
    if (length(ngbg) > k) {ngbg <- ngbg[seq_len(k)]} #getting the k nearer
    newDistToClass[itest,g] <- median(sortDis[ngbg]) #take the median of the k nearer
  }
  }

testpairwisediss=daisy(test_matrix, type = list(symm=6:10), weights = importanceweight)
  
#############################################################

vcr.out.withdist=vcr.custom.train(y=train_label, train_posteriors, distToClasses = distToClass)

classmap(vcr.out.withdist, whichclass = 3)

source("feature_code/R/VCR_visualization.R")
source("feature_code/R/VCR_plotly.R")
mdsColorscale(vcrout = vcr.out, diss=trainpairwisedis)

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
vcr.out.withdisttest=vcr.custom.newdata(ynew = test_label, test_posteriors, newDistToClasses = newDistToClass, vcr.custom.train.out = vcr.out.withdist)
classmap(vcr.out.withdisttest, whichclass = 3)

mdsColorscale(vcrout = vcr.out.test, diss=testpairwisediss, classCols = c("darkgreen","red","darkblue"))




