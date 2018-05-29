#Opticluster

#rm(list=ls())

#library('randomForest')
library("optCluster")
library("kohonen")
library("mclust")
library("org.Hs.eg.db")
source("https://bioconductor.org/biocLite.R")
library("Biobase")
library("AnnotationDbi")
source("bitr.R")
library(GOSemSim)
library(magrittr)
#5000 genes and 93 patients
#log and normalized data
gem = read.table("combined_gem_multi_log_filtered_7500.tsv", header = TRUE, sep = "\t")

#bitr <- function(geneID, fromType, toType, OrgDb, drop=TRUE) { }


#library("annotate")
#library("clusterProfiler")
#biocLite("annotate");
#biocLite("GO.db",lib = "C:/Program Files/R/R-3.4.0/library" )
#library("GO.db")
#k2 =  keys(org.Hs.eg.db) #Shows keys
#View(select(org.Hs.eg.db, keys =k2[1], columns = c("ENSEMBL","GO", "GOALL"))) #shows columns of annotation table
gList = as.data.frame(as.character(gem$Gene)) 
#Cut decimal of GO ID
gList2 = apply(gList[1], 1,function(x) { substring(x, 1,15) });
#gList2 = as.data.frame(as.character(gList2)) # 
gem2 = gem
rownames(gem2) = gList2
gem2 = gem2[,-1]
ids = bitr(gList2, fromType="ENSEMBL", toType=c("GO", "ONTOLOGY"), OrgDb="org.Hs.eg.db")
#Creates list of GO ID assignments
#clMethods = c("agnes", "clara", "diana", "hierarchical", "kmeans", "model", "pam", "som", "sota"),
#only include GOID
gem_cluster_final = optCluster(gem2,seq(190,210,2), clMethods = c("agnes", "clara", "diana", "hierarchical", "kmeans", "model", "pam", "sota"), countData = FALSE, validation = "biological", annotation =ids[,1:2])
#OPTIONAL 
summary(gem_cluster_final)
optMethod = strsplit(topMethod(gem_cluster_final), "-", TRUE)
optMethod = unlist(optMethod)
optMethod = optMethod[1] 
clusterResults(gem_cluster_final, method=optMethod)
topMethod(gem_cluster_final)

#Check mapped and unmapped genes 
#gList_mapped = as.data.frame(unique(ids[,1]))
#gList_unmapped = as.data.frame(subset(gList2, !(gList2 %in% gList_mapped[,1])));

