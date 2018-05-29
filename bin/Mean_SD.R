setwd("C:/Users/test/Desktop/SP2017/BME8813/Cancer")
#Mean_SD_Script.R

cancer= read.table("chol_matrix", header = FALSE, sep = " ", stringsAsFactors = FALSE) #load data
rownames(cancer) = cancer[,1]
cancer = cancer[,-1]

#Step 1 Find cols of each cancer type

coad_cols = (which(cancer == "coad"))%%(dim(cancer)[1]) 
esca_cols = (which(cancer == "esca"))%%(dim(cancer)[1]) 
paad_cols = (which(cancer == "paad"))%%(dim(cancer)[1]) 
read_cols = (which(cancer == "read"))%%(dim(cancer)[1]) 
stad_cols = (which(cancer == "stad"))%%(dim(cancer)[1]) 
blca_cols = (which(cancer == "blca"))%%(dim(cancer)[1]) 
brca_cols = (which(cancer == "brca"))%%(dim(cancer)[1]) 
cesc_cols = (which(cancer == "cesc"))%%(dim(cancer)[1]) 
chol_cols = (which(cancer == "chol"))%%(dim(cancer)[1]) 
hnsc_cols = (which(cancer == "hnsc"))%%(dim(cancer)[1]) 
lihc_cols = (which(cancer == "lihc"))%%(dim(cancer)[1]) 
luad_cols = (which(cancer == "luad"))%%(dim(cancer)[1]) 
lusc_cols = (which(cancer == "lusc"))%%(dim(cancer)[1]) 
pcpg_cols = (which(cancer == "pcpg"))%%(dim(cancer)[1]) 
sarc_cols = (which(cancer == "sarc"))%%(dim(cancer)[1]) 
skcm_cols = (which(cancer == "skcm"))%%(dim(cancer)[1]) 
tgct_cols = (which(cancer == "tgct"))%%(dim(cancer)[1]) 
ucec_cols = (which(cancer == "ucec"))%%(dim(cancer)[1]) 

#Step 2: Calculate means and SD for each cancer type
cancer_num = data.matrix(cancer[-1,]) #converts to numeric mode & removes row of cancer names  

coad_mean <- apply(cancer_num[,coad_cols], 1,function(x) { mean(x) }) 
esca_mean <- apply(cancer_num[,esca_cols], 1,function(x) { mean(x) })  
paad_mean <- apply(cancer_num[,paad_cols], 1,function(x) { mean(x) })
read_mean <- apply(cancer_num[,read_cols], 1,function(x) { mean(x) }) 

blca_mean <- apply(cancer_num[,blca_cols], 1,function(x) { mean(x) }) 
brca_mean <- apply(cancer_num[,brca_cols], 1,function(x) { mean(x) }) 
cesc_mean <- apply(cancer_num[,cecs_cols], 1,function(x) { mean(x) }) 
chol_mean <- apply(cancer_num[,chol_cols], 1,function(x) { mean(x) }) 
hnsc_mean <- apply(cancer_num[,hnsc_cols], 1,function(x) { mean(x) }) 
lihc_mean <- apply(cancer_num[,lihc_cols], 1,function(x) { mean(x) }) 
luad_mean <- apply(cancer_num[,luad_cols], 1,function(x) { mean(x) }) 
lusc_mean <- apply(cancer_num[,lusc_cols], 1,function(x) { mean(x) }) 
pcpg_mean <- apply(cancer_num[,pcpg_cols], 1,function(x) { mean(x) }) 
sarc_mean <- apply(cancer_num[,sarc_cols], 1,function(x) { mean(x) }) 
skcm_mean <- apply(cancer_num[,skcm_cols], 1,function(x) { mean(x) }) 
stad_mean <- apply(cancer_num[,stad_cols], 1,function(x) { mean(x) }) 
tgct_mean <- apply(cancer_num[,tgct_cols], 1,function(x) { mean(x) }) 
ucec_mean <- apply(cancer_num[,ucec_cols], 1,function(x) { mean(x) }) 

coad_sd <- apply(cancer_num[,coad_cols], 1,function(x) { sd(x) }) 
esca_sd <- apply(cancer_num[,esca_cols], 1,function(x) { sd(x) })  
paad_sd <- apply(cancer_num[,paad_cols], 1,function(x) { sd(x) })
read_sd <- apply(cancer_num[,read_cols], 1,function(x) { sd(x) }) 
stad_sd <- apply(cancer_num[,stad_cols], 1,function(x) { sd(x) }) 
blca_sd <- apply(cancer_num[,blca_cols], 1,function(x) { sd(x) }) 
brca_sd <- apply(cancer_num[,brca_cols], 1,function(x) { sd(x) }) 
cesc_sd <- apply(cancer_num[,cecs_cols], 1,function(x) { sd(x) }) 
chol_sd <- apply(cancer_num[,chol_cols], 1,function(x) { sd(x) }) 
hnsc_sd <- apply(cancer_num[,hnsc_cols], 1,function(x) { sd(x) }) 
lihc_sd <- apply(cancer_num[,lihc_cols], 1,function(x) { sd(x) }) 
luad_sd <- apply(cancer_num[,luad_cols], 1,function(x) { sd(x) }) 
lusc_sd <- apply(cancer_num[,lusc_cols], 1,function(x) { sd(x) }) 
pcpg_sd <- apply(cancer_num[,pcpg_cols], 1,function(x) { sd(x) }) 
sarc_sd <- apply(cancer_num[,sarc_cols], 1,function(x) { sd(x) }) 
skcm_sd <- apply(cancer_num[,skcm_cols], 1,function(x) { sd(x) }) 
tgct_sd <- apply(cancer_num[,tgct_cols], 1,function(x) { sd(x) }) 
ucec_sd <- apply(cancer_num[,ucec_cols], 1,function(x) { sd(x) }) 

#Can put into one large table if desired 

#Step 4:Make Drug Tables 
FU_table = cancer[,c(coad_cols, esca_cols,paad_cols, read_cols, stad_cols)] 
Gem_table =cancer[,c(blca_cols, brca_cols, cesc_cols, chol_cols, hnsc_cols, lihc_cols, luad_cols, lusc_cols, paad_cols,
                     pcpg_cols, sarc_cols, skcm_cols, tgct_cols, ucec_cols)]


#Step 5: Remove rows that have less than 25% unique values in each table, NOTE this creates new table with desired rows 
FU_drop = FU_table[apply(FU_table[2:nrow(FU_table),],1, 
                                 function(PC) {(length(unique(as.numeric(PC)))/length(as.numeric(PC))) < 0.2}),]

Gem_drop = Gem_table[apply(Gem_table[2:nrow(FU_table),],1, 
                          function(PC) {(length(unique(as.numeric(PC)))/length(as.numeric(PC))) < 0.2}),]
#Step 6: Normalize Data

#load data of 486 patients of interest





