theme_shashwat <- function(base_size_value = 14) 
{
  theme_bw(base_size = base_size_value, base_family = "Helvetica") %+replace%
    theme(panel.grid = element_blank(),
          panel.border = element_blank(),
          plot.background = element_blank(),
          axis.text = element_text(color = "#000000", family = "Helvetica"),
          axis.title = element_text(color = "#000000", size = base_size_value + 2),
          plot.title = element_text(color = "#000000", hjust = 0.5),
          axis.line.y = element_line(color = "#000000"),
          axis.line.x = element_line(color = "#000000")
    )
}

inputData <- read.table('5fu_probabilities_200x_means.tsv', sep = " ", header = T)
response <- substr(inputData$res.cancer.patient, 1, 3)

trainingData <- data.frame(cbind(response, inputData$meanProb))
colnames(trainingData) <- c("response", "meanProb")

trainingData$response <- as.character(trainingData$response)
trainingData[trainingData$response == "non",]$response <- 0
trainingData[trainingData$response == "res",]$response <- 1
trainingData$response <- as.integer(trainingData$response)
trainingData$meanProb <- as.double(as.character(trainingData$meanProb))

rocObj <- roc(response~meanProb, trainingData)
rocDF_fuTrain <- data.frame(cbind(rocObj$thresholds, rocObj$sensitivities, rocObj$specificities))
colnames(rocDF_fuTrain) <- c("Thresholds", "Sensitivity", "Specificity")


inputData <- read.table('5fu_val_probabilities_052518.tsv', sep = " ", header = T)
response <- substr(inputData$res.cancer.patient, 1, 3)

validationData <- data.frame(cbind(response, inputData$meanProb))
colnames(validationData) <- c("response", "meanProb")

validationData$response <- as.character(validationData$response)
validationData[validationData$response == "non",]$response <- 1 # To coorrect for directionalty bias in roc()
validationData[validationData$response == "res",]$response <- 0
validationData$response <- as.integer(validationData$response)
validationData$meanProb <- as.double(as.character(validationData$meanProb))

rocObj <- roc(response~meanProb, validationData)
rocDF_fuVal <- data.frame(cbind(rocObj$thresholds, rocObj$sensitivities, rocObj$specificities))
colnames(rocDF_fuVal) <- c("Thresholds", "Sensitivity", "Specificity")


fuPlot <- ggplot() + geom_roc(data = trainingData, aes(d = response, m = meanProb), n.cuts = F,color = "#0000FF") +
          geom_roc(data = validationData, aes(d = response, m = meanProb), n.cuts = F, color = "#FF0000") +
          theme_shashwat() + labs(x="1 - Specificity", y = "Sensitivity") + geom_abline(intercept = 0, slope = 1, color = "#808080", alpha = 0.75)


#
inputData <- read.table('gem_probabilities_200x_means.tsv', sep = " ", header = T)
response <- substr(inputData$res.cancer.patient, 1, 3)

trainingData <- data.frame(cbind(response, inputData$meanProb))
colnames(trainingData) <- c("response", "meanProb")

trainingData$response <- as.character(trainingData$response)
trainingData[trainingData$response == "non",]$response <- 0
trainingData[trainingData$response == "res",]$response <- 1
trainingData$response <- as.integer(trainingData$response)
trainingData$meanProb <- as.double(as.character(trainingData$meanProb))



inputData <- read.table('gem_val_probabilities.tsv', sep = " ", header = T)
response <- substr(inputData$res.cancer.patient, 1, 3)

validationData <- data.frame(cbind(response, inputData$meanProb))
colnames(validationData) <- c("response", "meanProb")

validationData$response <- as.character(validationData$response)
validationData[validationData$response == "non",]$response <- 0
validationData[validationData$response == "res",]$response <- 1
validationData$response <- as.integer(validationData$response)
validationData$meanProb <- as.double(as.character(validationData$meanProb))

gemPlot <- ggplot() + geom_roc(data = trainingData, aes(d = response, m = meanProb), n.cuts = F, color = "#0000FF") +
  geom_roc(data = validationData, aes(d = response, m = meanProb), n.cuts = F, color = "#FF0000") +
  theme_shashwat() + labs(x="1 - Specificity", y = "Sensitivity") + geom_abline(intercept = 0, slope = 1, color = "#808080", alpha = 0.75)


# gemPlot <- ggplot() + geom_roc(data = trainingData, aes(d = response, m = meanProb), n.cuts = F, color = "#0000FF") +
#   geom_roc(data = validationData, aes(d = response, m = meanProb), n.cuts = F, color = "#FF0000") +
#   theme_shashwat() + labs(x="1 - Specificity", y = "Sensitivity") + geom_abline(intercept = 0, slope = 1)


gemDots <- ggplot(validationData, aes(x = response, y = meanProb)) + geom_point()

#

rocObj <- roc(response~meanProb, trainingData)
rocDF <- data.frame(cbind(rocObj$thresholds, rocObj$sensitivities, rocObj$specificities))
colnames(rocDF) <- c("Thresholds", "Sensitivity", "Specificity")





######################### CODE FOR VIOLINS #####################################

#setwd("C:/Users/test/Desktop/HP_Desktop/BioInfProject/Paper_Files")

args = c('5fu_probabilities_200x_means.tsv', 'gem_probabilities_200x_means.tsv')

#data = read.table('gem_probabilities_200x_means.tsv', header = TRUE, sep = "")
for (i in args){
  data = read.table(i, header = TRUE, sep = " ")
  type = substring(i, 1,3)
  
  data$class = as.numeric(data$meanProb>0.5)
  data$response = substring(data[,1],1,3)
  data$response[data$response=='non'] = 'Non-responder'
  data$response[data$response=='res'] = 'Responder'
  
  data$cancer_type = substring(data[,1],5,8)
  write.table(table(data$class, data$response), file = paste(type,'contingency_table' ,sep ="_"), sep = "\t", quote = F, row.names = F )
  
  cancers <- unique(data$cancer_type)    
  # accuracy_by_cancer = data.frame(cancer_type = character(),
  #                                 accuracy = double(),
  #                                 count = double())
  # 
  # for(i in 1:length(cancers)){
  #   A =  data[data$cancer_type== cancers[i],'class'] 
  #   B = data[data$cancer_type== cancers[i],'response']
  #   tmp = table(A,B)
  #   out = data.frame(cancers[i], sum(diag(prop.table(tmp))), length(A)) 
  #   names(out) = names(accuracy_by_cancer)
  #   accuracy_by_cancer = rbind(accuracy_by_cancer, out)
  # }
  # 
  # 
  # write.table(accuracy_by_cancer,file = paste(type,'accuracy_by_cancer_type' ,sep ="_"), sep = "\t", quote = F, row.names = F )
  # 
  #Line chart
  #plot(data$meanProb, y = rep(0, nrow(data)), col= data$color)
  
  #Violin plot with dot plot
  library('ggplot2')
  plot_data <- data[,c('meanProb','response')]
  
  # p <- ggplot(plot_data, aes(x=response, y = meanProb, color = response, fill = response)) + 
  #   labs(y = 'Average Probability') + ylim(0,1) +
  #   geom_violin(color = 'black')
  # p <- p + geom_jitter(shape = 16, position = position_jitter(0.05), size = 2.5) +
  #   scale_fill_brewer(palette = "Set1") + theme(text = element_text(size = 18)) + theme_shashwat()
  
  p <- ggplot() +
    geom_violin(data = plot_data, aes(response, meanProb, fill = response)) +
    geom_jitter(data = plot_data, aes(response, meanProb), pch = 21, size = 3.5, stroke = 2, position = position_jitter(0.06), fill = "#FFFFFF") +
    ylim(c(0,1)) +
    scale_fill_manual(values = c("#0D47A1", "#CDDC39")) +
    theme_shashwat()
  
  assign(paste("file_", i, sep = ""), p)
}



ggarrange(ncol = 2, nrow = 2, fuPlot, gemPlot, file_5fu_probabilities_200x_means.tsv, file_gem_probabilities_200x_means.tsv, labels = c("a", "b", "c", "d"))








