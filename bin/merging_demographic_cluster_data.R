#Merge demographic data

setwd("C:/Users/test/Desktop/BioInfProject/Demo_Cleaned")

#Load 5FU

fu_demo = read.csv("5fu_multi_demographic_cleaned.txt", sep = "\t", header = T )
fu_cluster_means = read.csv("5fu_multi_5k_clara_204_cluster_means.txt", sep = "\t", header = T ) #take transpose
fu_cluster_ranking = scan("converge_top200_120617_484x_5fu_multi_5k_clara_204_cluster_means.txt", what = "list")
fu_cluster_ranking = gsub("X", "", fu_cluster_ranking)


#5FU: Top 15 clusters 
fu_cluster_means_opt = fu_cluster_means[fu_cluster_means$Group.1 %in%  fu_cluster_ranking[1:15],] #numeric

#Load Gem
gem_demo = read.csv("gem_multi_demographic_cleaned.txt", sep = "\t", header = T )
gem_cluster_means = read.csv("gem_multi_5k_clara_192_cluster_means.txt", sep = "\t", header = T ) #take transpose
gem_cluster_ranking = scan("converge_top200_120617_466x_gem_multi_5k_clara_192_cluster_means.txt", what = "list")
gem_cluster_ranking = gsub("X", "", gem_cluster_ranking)

#Gem: Top 27 clusters
gem_cluster_means_opt = gem_cluster_means[gem_cluster_means$Group.1 %in% gem_cluster_ranking[1:27],]

#Join tables & Prep for classification
fu_cluster_data = as.data.frame(t(fu_cluster_means_opt)) #numeric
fu_cluster_data = fu_cluster_data[2:nrow(fu_cluster_data),]
fu_cluster_data$id = substring(rownames(fu_cluster_data), 10)
#fu_cluster_data$full_id = rownames(fu_cluster_data)
fu_cluster_data$response = substring(rownames(fu_cluster_data),1,3)
fu_cluster_data$response = as.factor(as.numeric(as.factor(fu_cluster_data$response))-1)

full_fu_data = merge(fu_cluster_data, fu_demo , by = "id") #numeric

write.table(full_fu_data, "full_fu_data_demo.txt", sep =  "\t", quote = F)

gem_cluster_data = as.data.frame(t(gem_cluster_means_opt))
gem_cluster_data = gem_cluster_data[2:nrow(gem_cluster_data),]
gem_cluster_data$id = substring(rownames(gem_cluster_data), 10)
#gem_cluster_data$full_id = rownames(gem_cluster_data)
gem_cluster_data$response = substring(rownames(gem_cluster_data),1,3)
gem_cluster_data$response = as.factor(as.numeric(as.factor(gem_cluster_data$response))-1)

full_gem_data = merge(gem_cluster_data, gem_demo , by = "id")

write.table(full_gem_data, "full_gem_data_demo.txt", sep =  "\t", quote = F)
