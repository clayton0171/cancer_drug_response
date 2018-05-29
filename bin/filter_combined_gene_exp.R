#/usr/bin/R
#this script removes genes with low variance and outputs top 6000 using SD & Percent Unique 

args = commandArgs(trailingOnly=TRUE);

cancer_drop = read.table(args[1], header = TRUE, sep = " ") #genes to be dropped
cancer = read.table(args[2], header = TRUE, sep = " ") #log and normalized of expression data 

#delete rows that have low population variance
cancer_multi = subset(cancer, !(cancer[,1] %in% cancer_drop[,1]))

#Plot Unique values vs sd
cancer_unique = as.data.frame(apply(data.matrix(cancer[,-1]), 1, function(X) {(length(unique(X)))/length(X)}))
rownames(cancer_unique) = cancer$Gene[1:nrow(cancer_unique)]

cancer_sd = as.data.frame(apply(data.matrix(cancer[,-1]), 1, function(X) {sd(X)})) 
rownames(cancer_sd) = cancer$Gene[1:nrow(cancer_sd)]

#Plot SD vs Unique%
cancer_plot_data = cbind.data.frame(cancer_sd,cancer_unique)
colnames(cancer_plot_data) = c("SD", "Unique_Percent")
#plot(cancer_plot_data, type = "p" , xlab = "Standard Deviation", ylab = "Percent Unique", main = "cancer %Unique vs SD")  

#create 3rd col with variable of product of SD*Unique%
cancer_plot_data = cbind.data.frame(cancer_plot_data, (cancer_plot_data$SD*cancer_plot_data$Unique_Percent))
cancer_plot_data_filtered = cancer_plot_data[complete.cases(cancer_plot_data),] #remove NA values, 3 removed

#Get top 6000 genes
cancer_genes = cancer_plot_data_filtered[order(cancer_plot_data_filtered[,3], decreasing = TRUE),] #sort genes to keep 
cancer_genes_keep = cancer_genes[1:7500,] 

#Final Table with 6k genes
cancer_final = subset(cancer, cancer$Gene %in% rownames(cancer_genes_keep))

write.table(cancer_final, file = paste(args[2],"_filtered_7500.tsv",sep = ""), row.names=F, quote=FALSE, sep = "\t")




#######################################################################################################################

#FU multiple cancer file
#fum_drop = read.table("FU_drop_cols", header = TRUE, sep = " ") #genes to be dropped
#fum = read.table("combined_5fu_multi_log", header = TRUE, sep = " ") #log and normalized of expression data 

#delete rows that have low population variance
#fu_multi = subset(fum, !(fum[,1] %in% fum_drop[,1]))

#Plot Unique values vs sd
#fu_multi_unique = as.data.frame(apply(data.matrix(fu_multi[,-1]), 1, function(X) {(length(unique(X)))/length(X)}))
#rownames(fu_multi_unique) = fu_multi$Gene[1:nrow(fu_multi_unique)]

#fu_multi_sd = as.data.frame(apply(data.matrix(fu_multi[,-1]), 1, function(X) {sd(X)})) 
#rownames(fu_multi_sd) = fu_multi$Gene[1:nrow(fu_multi_sd)]

#Plot SD vs Unique%
#FU_plot_data = cbind.data.frame(fu_multi_sd,fu_multi_unique)
#colnames(FU_plot_data) = c("SD", "Unique_Percent")
#plot(FU_plot_data, type = "p" , xlab = "Standard Deviation", ylab = "Percent Unique", main = "FU_Multi %Unique vs SD")  

#create 3rd col with variable of product of SD*Unique%
#FU_plot_data = cbind.data.frame(FU_plot_data, (FU_plot_data$SD*FU_plot_data$Unique_Percent))
#FU_plot_data_filtered = FU_plot_data[complete.cases(FU_plot_data),] #remove NA values, 3 removed

#Get top 6000 genes
#FU_multi_genes = FU_plot_data_filtered[order(FU_plot_data_filtered[,3], decreasing = TRUE),] #sort genes to keep 
#FU_multi_genes_keep = FU_multi_genes[1:6000,] #a


#Final Table with 5k genes
#fum_final = subset(fu_multi, fu_multi$Gene %in% rownames(FU_multi_genes_keep))

#write.table(fum_final, file ="5FU_Multi_Filtered", sep = "\t")
