#!/bin/R
#this script performs feature selection via random forest and outputs rank of all features once the top n converge
#module load R
#run as: Rscript rf_feature_selection.R input_clusters_means.tsv

library(randomForest)
args = commandArgs(trailingOnly=TRUE); #accept arguments, first = data file (needs to be oriented s.t. rows are patients and columns are features, including 1 col for labels)

### Read Data / Preprocess ###

#data = read.csv(args[1], header = T); #use for csv input without rownames
#rownames(data) = c(1:nrow(data)); #use for csv input without rownames
data = read.table(args[1], header = T, row.names = 1); #use for tsv output from python script

data = subset(data, select = -c(cancer_type)); #remove cancer type (only applicable for tables accidentally with this col)
#data = t(data); #transpose data (flip rows and cols)

response = substr(rownames(data),1,3); #parse response from sample name (rownames)
data = cbind.data.frame(data,response); #add column with response to end of data frame

#add for demo
#for(i in 1:15) { data[,i] = as.numeric(as.character(data[,i])); }
#data$age_at_diagnosis = as.numeric(as.character(data$age_at_diagnosis));
#demo

### Make Formula ###
#use all of the data for feature selection

names(data) = make.names(names(data)); #convert feature names to be legal (this adds X in front of cluster number)

## BUILD RF ##
#run rf once to get values for initialization
random_forest = randomForest(data$response ~ .,data = data, ntree = 10000, importance = T); #default mtry = sqrt number of features
#initialize vars_importance data frame
vars_importance = as.data.frame(importance(random_forest));

#get MeanDecreaseGIni Score for all clusters (variables)
vars_importance = as.data.frame(vars_importance[,c("MeanDecreaseGini"), drop=FALSE]);
vars_rank1 = c();
vars_rank2 = c();
vars_rank3 = c();
x = 0;

#one iteration takes 7.5 seconds, scales linearly
#repeatedly run rf, each time ranking clusters on importance (most informative for prediction)
repeat {
	x = x + 1;
	#push back old predictions in "queue"
	vars_rank3 = vars_rank2;
	vars_rank2 = vars_rank1;
	#make prediction
	random_forest = randomForest(data$response ~ .,data = data, ntree = 10000, importance = T); #default mtry = sqrt number of features
	imp = as.data.frame(importance(random_forest));
	#imp = imp[order(imp$MeanDecreaseGini, decreasing = T),, drop=FALSE] #sort variables from most to least important
	#get MeanDecreaseGini for each cluster to rank importance
	imp = as.data.frame(imp[,c("MeanDecreaseGini"), drop=FALSE]);
	#add score to vars_importance dataframe
	vars_importance = cbind(vars_importance, imp);
	#Calculate mean Gini Score for all variables
	vars_means = as.data.frame(apply(vars_importance, 1, function(x) {mean(x)}))
	#Rank variables (sort by highest Mean Gini Score)
	vars_means = vars_means[order(vars_means, decreasing = TRUE),, drop=FALSE];
	vars_rank = rownames(vars_means);
	vars_rank1 = head(vars_rank,200); #save top n features for checking convergence

	#exit loop once feature ranking converges 3 consecutive times for the top n features
	if( identical(vars_rank1,vars_rank2) && identical(vars_rank2,vars_rank3) ) {
		print("Converged");
		print(x);
                break;	
	}
	else if(x == 46080) { #set exit on x iterations to prevent infinite loop
		print("Force Quit");
		print(x);
		break;
	}
	else if(x %% 100 == 0 ) { #print every 100 iterations ~12.5 mins
		print(x);
	}
}

#Print ranking of clusters from RF feature selection
tmp=paste("converge_top200_030118_",x,"x_",args[1],sep="");
capture.output(cat(vars_rank), file=tmp)

