#install.packages("xgboost")
library(xgboost)

#Loading the data-set
DataSet<- read.csv(file.choose(),header=TRUE,sep=",",colClasses = c("numeric"))

TrainingDataSet <- DataSet[1:60000,]

TestingDataSet <- DataSet[60001:97009,]

TrainingDataSetA <- TrainingDataSet[,c(9,13,16,17,18,19,20,21,22,23,24)]
labelA=TrainingDataSet$A
modelA <- xgboost(data = as.matrix(TrainingDataSetA),label=labelA,nrounds =70,objective="multi:softmax",num_class=3,eval_metric="merror")
predictedA <- predict(modelA,as.matrix(TestingDataSet))
countA=0
for(i in 1:37009)
{
  if(predictedA[i]==TestingDataSet[i,15])
  {
    countA=countA+1
  }
}


TrainingDataSetB <- TrainingDataSet[,c(9,10,11,13,15,19,20,22,23,24,25)]
labelB=TrainingDataSet$B
modelB <- xgboost(data = as.matrix(TrainingDataSetB),label=labelB,nrounds =70,objective="multi:softmax",num_class=2,eval_metric="merror")
predictedB <- predict(modelB,as.matrix(TestingDataSet))
countB=0
for(i in 1:37009)
{
  if(predictedB[i]==TestingDataSet[i,16])
  {
    countB=countB+1
  }
}

TrainingDataSetC <- TrainingDataSet[,c(6,7,10,11,12,13,14,15,18,19,21,23)]
labelC=TrainingDataSet$C
modelC <- xgboost(data = as.matrix(TrainingDataSetC),label=labelC,nrounds =150,objective="multi:softmax",num_class=5,eval_metric="merror")
predictedC <- predict(modelC,as.matrix(TestingDataSet))
countC=0
for(i in 1:37009)
{
  if(predictedC[i]==TestingDataSet[i,17])
  {
    countC=countC+1
  }
}

TrainingDataSetD <- TrainingDataSet[,c(7,10,11,12,13,14,15,17,19,22,23)]
labelD=TrainingDataSet$D
modelD <- xgboost(data = as.matrix(TrainingDataSetD),label=labelD,nrounds =70,objective="multi:softmax",num_class=4,eval_metric="merror")
predictedD <- predict(modelD,as.matrix(TestingDataSet))
countD=0
for(i in 1:37009)
{
  if(predictedD[i]==TestingDataSet[i,18])
  {
    countD=countD+1
  }
}

TrainingDataSetE <- TrainingDataSet[,c(7,10,11,13,14,15,16,17,18,20,21)]
labelE=TrainingDataSet$E
modelE <- xgboost(data = as.matrix(TrainingDataSetE),label=labelE,nrounds =70,objective="multi:softmax",num_class=2,eval_metric="merror")
predictedE <- predict(modelE,as.matrix(TestingDataSet))
countE=0
for(i in 1:37009)
{
  if(predictedE[i]==TestingDataSet[i,19])
  {
    countE=countE+1
  }
}

TrainingDataSetF <- TrainingDataSet[,c(8,13,14,15,16,19,21,22,23,24,25)]
labelF=TrainingDataSet$F
modelF <- xgboost(data = as.matrix(TrainingDataSetF),label=labelC,nrounds =200,objective="multi:softmax",num_class=5,eval_metric="merror")
predictedF <- predict(modelF,as.matrix(TestingDataSet))
countF=0
for(i in 1:37009)
{
  if(predictedF[i]==TestingDataSet[i,20])
  {
    countF=countF+1
  }
}

TrainingDataSetG <- TrainingDataSet[,c(7,8,10,11,13,14,17,18,19,20,22,23)]
labelG=TrainingDataSet$G
modelG <- xgboost(data = as.matrix(TrainingDataSetG),label=labelG,nrounds =200,objective="multi:softmax",num_class=5,eval_metric="merror")
predictedG <- predict(modelG,as.matrix(TestingDataSet))
countG=0
for(i in 1:37009)
{
  if(predictedG[i]==TestingDataSet[i,21])
  {
    countG=countG+1
  }
}

count=0

for(i in 1:37009)
{
  if((TestingDataSet[i,15]==predictedA[i])&&(TestingDataSet[i,16]==predictedB[i])&&(TestingDataSet[i,17]==predictedC[i])&&(TestingDataSet[i,18]==predictedD[i])&&(TestingDataSet[i,19]==predictedE[i])&&(TestingDataSet[i,20]==predictedF[i])&&(TestingDataSet[i,21]==predictedG[i]))
  {
    count=count+1
  }
}