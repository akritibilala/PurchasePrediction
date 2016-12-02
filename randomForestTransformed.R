#install.packages("randomForest")
library(randomForest)

#Loading the data-set
DataSet<- read.csv(file.choose(),header=TRUE,sep=",",colClasses = c("A"="factor","B"="factor","C"="factor","D"="factor","E"="factor","F"="factor","G"="factor"))
print("a")
TrainingDataSet <- DataSet[1:60000,]
print("b")
TrainingDataSet <- TrainingDataSet[,-c(1,2,3,4,5,6,7)]
print("c")
TestingDataSet <- DataSet[60001:97009,]
print("d")
modelA <- randomForest(A~.,TrainingDataSet,ntree=700)
print("e")
predictedA <- predict(modelA,TestingDataSet)
print("f")

countA=0
for(i in 1:37009)
{
  if(predictedA[i]==TestingDataSet[i,17])
  {
    countA=countA+1
  }
}

modelB <- randomForest(B~.,TrainingDataSet,ntree=700)
predictedB <- predict(modelB,TestingDataSet)

countB=0
for(i in 1:37009)
{
  if(predictedB[i]==TestingDataSet[i,18])
  {
    countB=countB+1
  }
}

modelC <- randomForest(C~.,TrainingDataSet,ntree=700)
predictedC <- predict(modelC,TestingDataSet)

countC=0
for(i in 1:37009)
{
  if(predictedC[i]==TestingDataSet[i,19])
  {
    countC=countC+1
  }
}

modelD <- randomForest(D~.,TrainingDataSet,ntree=700)
predictedD <- predict(modelD,TestingDataSet)

countD=0
for(i in 1:37009)
{
  if(predictedD[i]==TestingDataSet[i,20])
  {
    countD=countD+1
  }
}

modelE <- randomForest(E~.,TrainingDataSet,ntree=700)
predictedE <- predict(modelE,TestingDataSet)

countE=0
for(i in 1:37009)
{
  if(predictedE[i]==TestingDataSet[i,21])
  {
    countE=countE+1
  }
}

modelF <- randomForest(F~.,TrainingDataSet,ntree=700)
predictedF <- predict(modelF,TestingDataSet)

countF=0
for(i in 1:37009)
{
  if(predictedF[i]==TestingDataSet[i,22])
  {
    countF=countF+1
  }
}

modelG <- randomForest(G~.,TrainingDataSet,ntree=700)
predictedG <- predict(modelG,TestingDataSet)

countG=0
for(i in 1:37009)
{
  if(predictedG[i]==TestingDataSet[i,23])
  {
    countG=countG+1
  }
}

count=0

for(i in 1:37009)
{
  if((TestingDataSet[i,17]==predictedA[i])&&(TestingDataSet[i,18]==predictedB[i])&&(TestingDataSet[i,19]==predictedC[i])&&(TestingDataSet[i,20]==predictedD[i])&&(TestingDataSet[i,21]==predictedE[i])&&(TestingDataSet[i,22]==predictedF[i])&&(TestingDataSet[i,23]==predictedG[i]))
  {
    count=count+1
  }
}