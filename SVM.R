
#install.packages("e1071")
library(e1071)
DataSet<- read.csv(file.choose(),header=TRUE,sep=",")
#TrainingDataSet<- read.csv(file.choose(),header=TRUE,sep=",")
#TestingDataSet<- read.csv(file.choose(),header=TRUE,sep=",")
#col< c("group_size","homeowner","risk_factor")
#trdata <- TrainingDataSet[,col]
TrainingDataSet <- DataSet[1:30000,]

TestingDataSet <- DataSet[30001:97009,]
svm.fit1 <- svm(A~C_previous*duration_previous*minimum_cost,data=TrainingDataSet, kernel="linear", cost=0.01, scale=FALSE, type="C-classification")
svm.fit2 <- svm(B~car_age*risk_factor*cost,data=TrainingDataSet, kernel="linear", cost=0.01, scale=FALSE, type="C-classification")
svm.fit3 <- svm(C~car_age*risk_factor*cost,data=TrainingDataSet, kernel="linear", cost=0.01, scale=FALSE, type="C-classification")
svm.fit4 <- svm(D~car_age*risk_factor*cost,data=TrainingDataSet, kernel="linear", cost=0.01, scale=FALSE, type="C-classification")
svm.fit5 <- svm(E~car_age*risk_factor*cost,data=TrainingDataSet, kernel="linear", cost=0.01, scale=FALSE, type="C-classification")
svm.fit6 <- svm(F~car_age*risk_factor*cost,data=TrainingDataSet, kernel="linear", cost=0.01, scale=FALSE, type="C-classification")
svm.fit7 <- svm(G~car_age*risk_factor*cost,data=TrainingDataSet, kernel="linear", cost=0.01, scale=FALSE, type="C-classification")
#tesdata <- TestingDataSet[,col]
svm.PredictedA <- predict(svm.fit1,TestingDataSet)
svm.PredictedB <- predict(svm.fit2,TestingDataSet)
svm.PredictedC <- predict(svm.fit3,TestingDataSet)
svm.PredictedD <- predict(svm.fit4,TestingDataSet)
svm.PredictedE <- predict(svm.fit5,TestingDataSet)
svm.PredictedF <- predict(svm.fit6,TestingDataSet)
svm.PredictedG <- predict(svm.fit7,TestingDataSet)
countA=0

for(i in 1:67009)
{
  if(svm.PredictedA[i]==TestingDataSet[i,17])
  {
    countA=countA+1
  }
}
print(countA)
countB=0

for(i in 1:67009)
{
  if(svm.PredictedB[i]==TestingDataSet[i,18])
  {
    countB=countB+1
  }
}

print(countB)

countC=0

for(i in 1:67009)
{
  if(svm.PredictedC[i]==TestingDataSet[i,19])
  {
    countC=countC+1
  }
}

print(countC)
countD=0

for(i in 1:67009)
{
  if(svm.PredictedD[i]==TestingDataSet[i,20])
  {
    countD=countD+1
  }
}

print(countD)
countE=0

for(i in 1:67009)
{
  if(svm.PredictedE[i]==TestingDataSet[i,21])
  {
    countE=countE+1
  }
}

print(countE)
countF=0

for(i in 1:67009)
{
  if(svm.PredictedF[i]==TestingDataSet[i,22])
  {
    countF=countF+1
  }
}

print(countF)
countG=0

for(i in 1:67009)
{
  if(svm.PredictedG[i]==TestingDataSet[i,23])
  {
    countG=countG+1
  }
}

print(countG)
count=0

for(i in 1:67009)
{
  if((TestingDataSet[i,17]==svm.PredictedA[i])&&(TestingDataSet[i,18]==svm.PredictedB[i])&&(TestingDataSet[i,19]==svm.PredictedC[i])&&(TestingDataSet[i,20]==svm.PredictedD[i])&&(TestingDataSet[i,21]==svm.PredictedE[i])&&(TestingDataSet[i,22]==svm.PredictedF[i])&&(TestingDataSet[i,23]==svm.PredictedG[i]))
  {
    count=count+1
  }
}
print(count)

print(svm.PredictedA)
print(summary(svm.fit1))
print(svm.PredictedB)
print(summary(svm.fit2))
print(svm.PredictedC)
print(summary(svm.fit3))
print(svm.PredictedD)
print(summary(svm.fit4))
print(svm.PredictedE)
print(summary(svm.fit5))
print(svm.PredictedF)
print(summary(svm.fit6))
print(svm.PredictedG)
print(summary(svm.fit7))
#t1<-table(svm.pred1,TestingDataSet)
#print(t1)
#precision1 <-t1[2,2]/(t1[1,2]+t1[2,2])
#print(precision1)
#recall1 <-t1[2,2]/(t1[2,1]+t1[2,2])
#print(recall1)
#fmeasure1 <- 2*recall1*precision1 /(recall1+precision1)
#sprint(fmeasure1)