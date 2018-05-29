#Try Logistic Regresssion on Selected Clustered Genomoic Data 

setwd("C:/Users/test/Desktop/BioInfProject/Demo_Cleaned")
library('glmnet')
library(boot)

#Load 5FU

fu_cluster_means = read.csv("5fu_multi_5k_clara_204_cluster_means.txt", sep = "\t", header = T ) #take transpose
fu_cluster_ranking = scan("converge_top200_120617_484x_5fu_multi_5k_clara_204_cluster_means.txt", what = "list")
fu_cluster_ranking = gsub("X", "", fu_cluster_ranking)

#5FU: Top 15 clusters 
fu_cluster_means_opt = fu_cluster_means[fu_cluster_means$Group.1 %in%  fu_cluster_ranking[1:15],] #numeric

#Logistic Regression
FU_xtrain = as.matrix(t(fu_cluster_means_opt[,-1])) #rememebr predictors need to be in matrix form 
colnames(FU_xtrain) = as.character(fu_cluster_means_opt[,1])

FU_response = as.factor(substring(rownames(FU_xtrain),1,3))
FU_ytrain = as.matrix(as.numeric(FU_response)-1)

FU.glm = glm(FU_ytrain~FU_xtrain, family = 'binomial')
summary(FU.glm)


FU.predict = data.frame(predict.glm(FU.glm,data.frame(FU_xtrain), type = "response"))

FU.predict[FU.predict>0.5] = 1
FU.predict[FU.predict<0.5] = 0
accuracy = 1 - (sum(abs(FU_ytrain-FU.predict))/length(FU_ytrain))  

#Accuracy with 10fold validation
FU_data = data.frame(cbind(FU_ytrain, FU_xtrain))
FU.acc = c()

smp_size = floor(0.8 * nrow(FU_data))

for (i in 1:10){
  FU_train_ind <- sample(seq_len(nrow(FU_data)), size = smp_size)
  FU_traindata <- FU_data[FU_train_ind, ]
  FU_testdata <- FU_data[-FU_train_ind, ]
  tmp = glm(V1~., family = 'binomial', data = FU_traindata)
  tmp.predict = predict.glm(tmp, newdata = FU_testdata, type = "response" )
  tmp.predict[tmp.predict>0.5] = 1
  tmp.predict[tmp.predict<0.5] = 0
  a = 1 - (sum(abs(FU_testdata$V1-tmp.predict))/length(FU_testdata))  
  FU.acc = c(FU.acc,a)
  } 

mean(FU.acc) #77%


###############################################################################################################
#Load Gem
gem_demo = read.csv("gem_multi_demographic_cleaned.txt", sep = "\t", header = T )
gem_cluster_means = read.csv("gem_multi_5k_clara_192_cluster_means.txt", sep = "\t", header = T ) #take transpose
gem_cluster_ranking = scan("converge_top200_120617_466x_gem_multi_5k_clara_192_cluster_means.txt", what = "list")
gem_cluster_ranking = gsub("X", "", gem_cluster_ranking)

#Gem: Top 27 clusters
gem_cluster_means_opt = gem_cluster_means[gem_cluster_means$Group.1 %in% gem_cluster_ranking[1:27],]

#Logistic Regression
gem_xtrain = as.matrix(t(gem_cluster_means_opt[,-1])) #rememebr predictors need to be in matrix form 
colnames(gem_xtrain) = as.character(gem_cluster_means_opt[,1])

gem_response = as.factor(substring(rownames(gem_xtrain),1,3))
gem_ytrain = as.matrix(as.numeric(gem_response)-1)

gem.glm = glm(gem_ytrain~gem_xtrain, family = 'binomial')
summary(gem.glm)


gem.predict = data.frame(predict.glm(gem.glm,data.frame(gem_xtrain), type = "response"))

gem.predict[gem.predict>0.5] = 1
gem.predict[gem.predict<0.5] = 0
gem_accuracy = 1 - (sum(abs(gem_ytrain-gem.predict))/length(gem_ytrain))  

#Accuracy with 10fold validation
gem_data = data.frame(cbind(gem_ytrain, gem_xtrain))
gem.acc = c()

smp_size = floor(0.8 * nrow(gem_data))

for (i in 1:10){
  gem_train_ind <- sample(seq_len(nrow(gem_data)), size = smp_size)
  gem_traindata <- gem_data[gem_train_ind, ]
  gem_testdata <- gem_data[-gem_train_ind, ]
  tmp = glm(V1~., family = 'binomial', data = gem_traindata)
  tmp.predict = predict.glm(tmp, newdata = gem_testdata, type = "response" )
  tmp.predict[tmp.predict>0.5] = 1
  tmp.predict[tmp.predict<0.5] = 0
  a = 1 - (sum(abs(gem_testdata$V1-tmp.predict))/length(gem_testdata))  
  gem.acc = c(gem.acc,a)
} 

mean(gem.acc) #73%
