TrainingDataSet<- read.csv(file.choose(),header=TRUE,sep=",")
normalize <- function(x) {
  return ((x - min(x))/ (max(x) - min(x)))
}
Data_n <- as.data.frame(lapply(TrainingDataSet[,c(2,7,8,9,10,11,12,13,15,16,24,25,26,27,28)], normalize))
Data_train <- Data_n[1:58206,]
Data_test <- Data_n[58207:97009,]
Data_train_target <- TrainingDataSet[1:58206, 17]
Data_test_target <- TrainingDataSet[58207:97009, 17]
require(class)
model1 <- knn(train = Data_train, test = Data_test, cl = Data_train_target, k = 200)
A <- table(Data_test_target, model1)

Data_train_targetB <- TrainingDataSet[1:58206, 18]
Data_test_targetB <- TrainingDataSet[58207:97009, 18]
model2 <- knn(train = Data_train, test = Data_test, cl = Data_train_targetB, k = 200)
B <- table(Data_test_targetB, model2)

Data_train_targetC <- TrainingDataSet[1:58206, 19]
Data_test_targetC <- TrainingDataSet[58207:97009, 19]
model3 <- knn(train = Data_train, test = Data_test, cl = Data_train_targetC, k = 200)
C <- table(Data_test_targetC, model3)

Data_train_targetD <- TrainingDataSet[1:58206, 20]
Data_test_targetD <- TrainingDataSet[58207:97009, 20]
model4 <- knn(train = Data_train, test = Data_test, cl = Data_train_targetD, k = 200)
D <- table(Data_test_targetD, model4)

Data_train_targetE <- TrainingDataSet[1:58206, 21]
Data_test_targetE <- TrainingDataSet[58207:97009, 21]
model5 <- knn(train = Data_train, test = Data_test, cl = Data_train_targetE, k = 200)
E <- table(Data_test_targetE, model5)

Data_train_targetF <- TrainingDataSet[1:58206, 22]
Data_test_targetF <- TrainingDataSet[58207:97009, 22]
model6 <- knn(train = Data_train, test = Data_test, cl = Data_train_targetF, k = 200)
F <- table(Data_test_targetF, model6)

Data_train_targetG <- TrainingDataSet[1:58206, 23]
Data_test_targetG <- TrainingDataSet[58207:97009, 23]
model7 <- knn(train = Data_train, test = Data_test, cl = Data_train_targetG, k = 200)
G <- table(Data_test_targetG, model7)