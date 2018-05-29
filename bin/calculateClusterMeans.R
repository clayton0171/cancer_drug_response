#!/usr/bin/R
#This script combines the optcluster assignments and takes mean of gene expression values by cluster by patient

args = commandArgs(trailingOnly=TRUE);

#Import 7 files
#args[1]: gene expression matrix
#args[2]:args[n]: optcluster assignment files

#Load expression file
gene_exp = read.table(args[1], header = TRUE, sep = "\t")

#Drop decimals in expression file
gene_exp$gene_short = apply(gene_exp[1], 1,function(x) { substring(x, 1,15) })

#Take means and write tables
for (i in 2:length(args)){
  if (grepl('sota', args[i])){
    index = nchar(args[i]) -nchar('assignments.txt')
    type = substring(args[i],1,index)
    y = read.table(args[i], header = F, sep = "", stringsAsFactors = F)
    MT = cbind(gene_exp,y$V2)
    colnames(MT)[ncol(MT)] = 'assignment'
    means_by_cluster = aggregate(subset(MT, select = -c(Gene,gene_short,assignment)),
                                 list(MT$assignment), data = MT, mean)
    write.table(means_by_cluster, file = paste(type,'means.txt' ,sep =""), sep = "\t", quote = F, row.names = F)
  }
  else {
  index = nchar(args[i]) -nchar('assignments.txt')
  type = substring(args[i],1,index)
  y = read.table(args[i], header = F, sep = "", stringsAsFactors = F)
  colnames(y) = c('gene_short','assignment')
  MT = merge(gene_exp,y, by = 'gene_short')
  drop = c('Gene')
  means_by_cluster = aggregate(subset(MT, select = -c(Gene,gene_short,assignment)),
                                      list(MT$assignment), data = MT, mean)
  write.table(means_by_cluster, file = paste(type,'means.txt' ,sep =""), sep = "\t", quote = F, row.names = F)
  } 
}






