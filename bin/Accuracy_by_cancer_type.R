#setwd("C:/Users/test/Desktop/HP_Desktop/BioInfProject/Paper_Files")

args = commandArgs(trailingOnly=TRUE);

#data = read.table('gem_probabilities_200x_means.tsv', header = TRUE, sep = "")

data = read.table(args[1], header = TRUE, sep = "")
type = substring(args[1], 1,3)

data$class = as.numeric(data$meanProb>0.5)
data$response = substring(data[,1],1,3)
data$response[data$response=='non'] = 'Non-responder'
data$response[data$response=='res'] = 'Responder'

data$cancer_type = substring(data[,1],5,8)
write.table(table(data$class, data$response), file = paste(type,'contingency_table' ,sep ="_"), sep = "\t", quote = F, row.names = F )

cancers <- unique(data$cancer_type)    
accuracy_by_cancer = data.frame(cancer_type = character(),
                      accuracy = double(),
                      count = double())

for(i in 1:length(cancers)){
   A =  data[data$cancer_type== cancers[i],'class'] 
   B = data[data$cancer_type== cancers[i],'response']
   tmp = table(A,B)
   out = data.frame(cancers[i], sum(diag(prop.table(tmp))), length(A)) 
   names(out) = names(accuracy_by_cancer)
   accuracy_by_cancer = rbind(accuracy_by_cancer, out)
}


write.table(accuracy_by_cancer,file = paste(type,'accuracy_by_cancer_type' ,sep ="_"), sep = "\t", quote = F, row.names = F )

#Line chart
#plot(data$meanProb, y = rep(0, nrow(data)), col= data$color)

#Violin plot with dot plot
library('ggplot2')
plot_data <- data[,c('meanProb','response')]

p <- ggplot(plot_data, aes(x=response, y = meanProb, color = response, fill = response)) + 
              labs(y = 'Average Probability') + ylim(0,1) +  geom_violin(color = 'black')
p + geom_jitter(shape = 16, position = position_jitter(0.1), color = 'black', size = 2.5) +
  scale_fill_brewer(palette = "Set1") + theme(text = element_text(size = 18))
