#!/bin/R

#this script executes random forest
library(pROC)
library(PRROC)
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

#pull informative clusters
features = modules[1:32]; #6 #50
features = c(features, "response");
training_new = training[names(training) %in% features];
names(training_new) = make.names(names(training_new)); #convert feature names to be legal
validation_new = validation[names(validation) %in% features];
names(validation_new) = make.names(names(validation_new)); #convert feature names to be legal

#set seed reproducibility
set.seed(2);
#use all training data to create final model
random_forest = randomForest(training_new$response ~ ., data = training_new, ntree = 10000, importance = T, mtry=4);
#use final model to predict response on external validation dataset (combo drugs)
pred_response = predict(random_forest, validation_new, type = "response" );
#ex-validation accuracy
accuracy = sum(pred_response == validation_new$response)/nrow(validation_new);
print(accuracy);
table(pred = pred_response, true = validation_new$response);

print(nrow(training_new))
a = rep(1,15); #remove +1 if nrow is divisible by 25
b = rep(2,15);
c = rep(3,16);
d = rep(4,16);

all_run = data.frame(row.names=1:62)
#if(FALSE) {
for(i in 1:200) {
	random_vec = c(a,b,c,d); #random vec has 25% ones 25% twos 25% threes 25% fours
	random_vec = sample(random_vec); #randomize positions

	#run1
	training_1 = training_new[random_vec %in% c(2,3,4),]; #select 75% of data for training
	testing_1 = training_new[random_vec==1,]; #select 25% of data for testing
	#use 75% training data to create model
	rf = randomForest(training_1$response ~ ., data = training_1, ntree = 10000, importance = T, mtry=4);
	pred = predict(rf, testing_1, type = "prob" );
	#accuracy = sum(pred == testing_1$response)/nrow(testing_1);
	#print(accuracy);
	#table(pred = pred, true = testing_1$response);

	#run 2
	training_1 = training_new[random_vec %in% c(1,3,4),]; #select 75% of data for training
	testing_1 = training_new[random_vec==2,]; #select 25% of data for testing
	rf = randomForest(training_1$response ~ ., data = training_1, ntree = 10000, importance = T, mtry=4);
	pred2 = predict(rf, testing_1, type = "prob");

	#run 3
	training_1 = training_new[random_vec %in% c(1,2,4),]; #select 75% of data for training
	testing_1 = training_new[random_vec==3,]; #select 25% of data for testing
	rf = randomForest(training_1$response ~ ., data = training_1, ntree = 10000, importance = T, mtry=4);
	pred3 = predict(rf, testing_1, type = "prob");

	#run 4
	training_1 = training_new[random_vec %in% c(1,2,3),]; #select 75% of data for training
	testing_1 = training_new[random_vec==4,]; #select 25% of data for testing
	rf = randomForest(training_1$response ~ ., data = training_1, ntree = 10000, importance = T, mtry=4);
	pred4 = predict(rf, testing_1, type = "prob");

	all_prob = rbind(pred, pred2, pred3, pred4)
	all_prob <- all_prob[ order(row.names(all_prob)), ] #sort df by rownames before cbind
	all_run = cbind(all_run,all_prob)
	row.names(all_run) = row.names(all_prob) #add sorted rownames to new df
}
#head(all_run)
all_prob = all_run[,seq(2,400,2)]
all_prob$mean = apply(all_prob, 1, function(x) { mean(x, na.rm=TRUE) })
#write.table(all_prob, file="5fu_probabilities.tsv", row.names=T);
#} #IF FALSE
all_prob = read.table("5fu_probabilities.tsv", header=TRUE, sep=" ", row.names=1);
#q()

#pROC
rf_prob = predict(random_forest, validation_new, type = "prob" );
write.table(rf_prob[,2], file="5fu_val_probabilities.tsv", row.names=T)
#train_prob = predict(random_forest, training_new, type = "prob" );
#head(train_prob)
#q()

