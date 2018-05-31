#!/bin/R

#this script executes random forest

library(randomForest)
args = commandArgs(trailingOnly=TRUE); #accept arguments, first = data file (needs to be oriented s.t. rows are patients and columns are features, including 1 col for labels) second file is list of cluster ranking

### Read Data / Preprocess ###
training = read.table(args[1], header = T, row.names = 1); #use for tsv output from calculateClusterMeans.R script
training = subset(training, select = -c(cancer_type)); #remove cancer type (only applicable for tables accidentally with this col)
#training = t(training); #transpose data (flip rows and cols)
response = substr(rownames(training),1,3);
training = cbind.data.frame(training,response);

#read in cluster ranking
modules = scan(file=args[2], what="list")
modules = gsub("X", "", modules)

#Read in validation data
validation = read.table(args[3], header = T, row.names = 1); #use for tsv output from calculateClusterMeans.R script
#validation = t(validation); #transpose data (flip rows and cols)
response = substr(rownames(validation),1,3);
validation = cbind.data.frame(validation,response);

for(i in 1:204) {
#pull informative clusters
features = modules[1:i]; #6 #50
features = c(features, "response");
training_new = training[names(training) %in% features];
names(training_new) = make.names(names(training_new)); #convert feature names to be legal
validation_new = validation[names(validation) %in% features];
names(validation_new) = make.names(names(validation_new)); #convert feature names to be legal

#set seed reproducibility
set.seed(2);
#use all training data to create final model
random_forest = randomForest(training_new$response ~ ., data = training_new, ntree = 10000, importance = T, mtry=4);
#use final model to predict response on external validation dataset
pred_response = predict(random_forest, validation_new, type = "response" );
#ex-validation accuracy
accuracy = sum(pred_response == validation_new$response)/nrow(validation_new);
print(i);
print(accuracy);
table(pred = pred_response, true = validation_new$response);
}

