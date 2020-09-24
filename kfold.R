library("FactoMineR")
library("factoextra")
library("rattle")
library("rpart")
library("rpart.plot")
library("RColorBrewer")
library("tidyverse")
library("caret")
library("tm")
library("e1071")
library("neuralnet")
source("function_support.R")
library("pROC")

dataset = read.csv("xAPI-Edu-Data.csv", header = TRUE)
dataset.processed = process_data(dataset)

folds <- cut(seq(1, nrow(dataset)), breaks=10, labels = FALSE)

accuracy <- matrix(nrow = 10, ncol = 4)
rownames(accuracy) <- c("fold1", "fold2", "fold3", "fold4", "fold5", "fold6", "fold7", "fold8", "fold9", "fold10")
colnames(accuracy) <- c("decision_tree", "pruned_decision_tree", "naive_bayes", "neural_network")

for(i in 1:10){
  print(paste("START FOLD", i))
  # DIVIDE TRAIN TEST FOR KFOLD
  testIndexes <- which(folds == i, arr.ind = TRUE)
  testData <- dataset[testIndexes, ]
  trainData <- dataset[-testIndexes, ]
  testData.processed <- dataset.processed[testIndexes, ]
  trainData.processed <- dataset.processed[-testIndexes, ]
  # Align factor of predicted and target value
  testData.processed$prediction = testData.processed$Class
  #trainData.processed.attributes = data.frame(trainData.processed[,1:4], trainData.processed[,6:73])
  trainData.processed.attributes = data.frame(trainData.processed[,1:3], trainData.processed[, 5:10])
  

  ### DECISION TREE ###
  decisionTree = rpart(Class ~ raisedhands + VisITedResources + Discussion + StudentAbsenceDays +  
                         ParentAnsweringSurvey + Relation, data=trainData, method="class")
  #fancyRpartPlot(decisionTree)
  testData$prediction <- predict(decisionTree, testData, type = "class")
  DT.perf <- confusionMatrix(testData$prediction, testData$Class, mode = "prec_recall")
  #DT.roc = multiclass.roc(factor(testData$Class, ordered = TRUE), factor(testData$prediction, ordered = TRUE))
  #rs <- DT.roc[['rocs']]
  #plot.roc(rs[[1]])
  #sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))
  
  # Save performance in csv
  write.table(round(DT.perf$byClass, 2), paste("result/decision_tree_perf_", i, ".txt", sep = ""), sep = "\t", col.names = NA)
  # Create accuracy table
  accuracy[i, 1] = DT.perf$overall[1]
  #plotcp(decisionTree)
  
  ### PRUNENED DECISION TREE ###
  prunedDecisionTree = prune(decisionTree, cp= 0.017)
  #fancyRpartPlot(prunedDecisionTree)
  #plotcp(prunedDecisionTree)
  testData$prediction <- predict(prunedDecisionTree, testData, type = "class")
  DTP.perf <- confusionMatrix(testData$prediction, testData$Class, mode = "prec_recall")
  #DTP.roc = multiclass.roc(factor(testData$Class, ordered = TRUE), factor(testData$prediction, ordered = TRUE))
  #rs <- DTP.roc[['rocs']]
  #plot.roc(rs[[1]])
  #sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))
  # Save performance in csv
  write.table(round(DTP.perf$byClass, 2), paste("result/pruned_decision_tree_perf_", i, ".txt", sep = ""),  sep = "\t", col.names = NA)
  # Create accuracy table
  accuracy[i, 2] = DTP.perf$overall[1]

  ### NAIVE BAYES ###
  NBclassfier = naiveBayes(as.factor(Class) ~ raisedhands + VisITedResources + Discussion + StudentAbsenceDays +  
                             ParentAnsweringSurvey + Relation, data=trainData)
  testData$prediction = predict(NBclassfier, testData, type = "class")
  NB.perf <- confusionMatrix(testData$prediction, testData$Class, mode = "prec_recall")

  # Save performance in csv
  write.table(round(NB.perf$byClass, 2), paste("result/naive_bayes_perf_", i, ".txt", sep = ""),  sep = "\t", col.names = NA)
  # Create accuracy table
  accuracy[i, 3] = NB.perf$overall[1]
  
  #NB.roc = multiclass.roc(factor(testData$Class, ordered = TRUE), factor(testData$prediction, ordered = TRUE))
  #rs <- NB.roc[['rocs']]
  #plot.roc(rs[[1]])
  #sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))
  
  ### NEURAL NETWORKS ###
  network = neuralnet(trainData.processed$highAndament + trainData.processed$mediumAndament + trainData.processed$lowAndament ~ .,
                      data = trainData.processed.attributes, hidden = 3, stepmax = 1e6)
  net.predict = compute(network, testData.processed)$net.result
  testData.processed$prediction = c("H", "L", "M")[apply(net.predict, 1, which.max)]
  # NN.perf <- confusionMatrix(factor(testData.processed$prediction, ordered = TRUE), factor(testData.processed$Class, ordered = TRUE), mode = "prec_recall")

  # Save performance in csv
  # write.table(round(NN.perf$byClass, 2), paste("result/neural_network_perf_", i, ".txt", sep = ""),  sep = "\t", col.names = NA)
  # Create accuracy table
  confusion.matrix = table(testData.processed$prediction, testData.processed$Class)
  accuracy[i, 4] = sum(diag(confusion.matrix))/sum(confusion.matrix)
  print(sum(diag(confusion.matrix))/sum(confusion.matrix))
  
  #NN.roc = multiclass.roc(factor(testData.processed$Class, ordered = TRUE), factor(testData.processed$prediction, ordered = TRUE))
  #rs <- NN.roc[['rocs']]
  #plot.roc(rs[[1]])
  #sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))
  
  print(paste("END FOLD", i))
}

write.table(round(accuracy, 2), "result/accuracy.txt", sep = "\t", col.names = NA)
