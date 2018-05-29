#this script normalizes a patient's gene expression (-mean/sd) by their cancer type

args = commandArgs(trailingOnly=TRUE);

cancer_norm = read.table(args[1], header = TRUE, sep = "\t")
patient_norm = read.table(gzfile(args[2]), header = FALSE, sep = "\t")

patient_norm_mat = data.matrix(patient_norm[,2]);
patient_norm_mat = log10(patient_norm_mat + 1);

norm_table = (patient_norm_mat[,1] - cancer_norm[,1])/cancer_norm[,2]

file_norm = cbind.data.frame(patient_norm[,1],norm_table)

write.table(file_norm, file = paste(args[2],"_norm.tsv",sep=""),quote=FALSE,row.names=FALSE,col.names=c("Gene",args[2]))
