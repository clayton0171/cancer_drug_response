#/usr/bin/R
#This script calculates the mean for all the pathways for a drug  model

args = commandArgs(trailingOnly=TRUE);

#setwd("C:/Users/test/Desktop/HP_Desktop/BioInfProject/Paper_Files")


#Import 3 files 
#args[1] gene expression file 
#args[2] genes included in the final model 
#args[3] file of genes for each pathway  

#Load files 
gene_exp = read.table(args[1], header = TRUE, sep = "\t")
#gene_exp = read.table('combined_5fu_multi_log_filtered_5000.tsv', header = TRUE, sep = "\t")

gene_exp$gene_short = substring(gene_exp$Gene,1,15)
type = substring(args[2], 1,3)

gene_list = read.table(args[2], header = TRUE, sep = "\t")
#gene_list = read.table('5fu_top32clust_genes',header = TRUE, sep = "\t" )
gene_list = apply(gene_list,1,function(x) {as.character(x)})
type = substring(args[2], 1,3)

gene_exp_top = subset(gene_exp, gene_exp$gene_short %in% gene_list) #Table of genes in top clusters

#Table of genes in pathways
pathway = read.table(args[3], header = TRUE, sep = "\t")
#pathway = read.table('5fu_pantherGeneList_parsed_ensgID_fixed_names.txt',header = TRUE, sep = "\t")
pathway_cleaned = subset(pathway, pathway$gene_short %in% gene_exp_top$gene_short)

gene_pathway = subset(gene_exp_top, gene_exp_top$gene_short %in% pathway$gene_short) #df for all genes in all pathways for model
responders = subset(gene_pathway, 
                    select = substring(colnames(gene_pathway),1,3) %in% c('res', 'gen'))

nonresponders = subset(gene_pathway, 
                       select = substring(colnames(gene_pathway),1,3) %in% c('non', 'gen'))


path_names = unique(pathway_cleaned$pathway_name)


tStat = data.frame(gene_short_res = character(),
                   gene_short_nonres = character(),
                   pathway_name = character(),
                   pathway_id = character(),
                   num_genes_in_pathway = double(),
                   tStat = double(),
                   responder_mean = double(),
                   nonresponder_mean = double()
                   )

for(i in 1:length(path_names)) { #loop through each gene pathway
  df_path = subset(pathway_cleaned,pathway_cleaned$pathway_name %in% path_names[i])
  df_res = subset(responders, responders$gene_short %in% df_path$gene_short) #df of pathway i
  df_nonres = subset(nonresponders, nonresponders$gene_short %in% df_path$gene_short) #df of pathway i
  
  for(j in 1:nrow(df_path)){ #loop through each gene for the pathway 
    
   result = t.test(df_res[j,1:ncol(df_res)-1],df_nonres[j,1:ncol(df_nonres)-1])
   out = data.frame(df_res$gene_short[j],df_nonres$gene_short[j], path_names[i], df_path$pathway_id[j], nrow(df_path),
                      result$statistic,result$estimate[1], result$estimate[2])
   names(out) = names(tStat)
   tStat = rbind(tStat, out)
    }
  
}
library('dplyr')

tStat_grouped = tStat %>% group_by(pathway_name, pathway_id, num_genes_in_pathway)
tStat_means_by_pathway = tStat_grouped %>% summarize(pathway_tStat_mean = mean(tStat))

p_value <- function(tStat, df) {
  if (df == 1) {
    p = 2*pt(tStat,df,lower= FALSE)
  } else {
    p = 2*pt(tStat,df-1,lower= FALSE)
  } 
  return(p)
}

tStat_means_by_pathway$p_value <- 
  mapply(function(x,y) p_value(x,y), abs(tStat_means_by_pathway$pathway_tStat_mean),
         tStat_means_by_pathway$num_genes_in_pathway)


#View(tStat)  

write.table(tStat, file = paste(type,'tStat_by_pathway.txt' ,sep ="_"), sep = "\t", quote = F, row.names = F)
write.table(tStat_means_by_pathway,file = paste(type,'tStat_means_by_pathway' ,sep ="_"), sep = "\t", quote = F, row.names = F )
                         