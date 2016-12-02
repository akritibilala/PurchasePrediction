

#TrainingDataSet<- read.csv(file.choose(),header=TRUE,sep=",")
DataSet<- read.csv(file.choose(),header=TRUE,sep=",")

TestingDataSet <- DataSet[411529:665249,]

GroundTruthDataSet <- subset(TestingDataSet, TestingDataSet$record_type > 0)

TestingDataSet$location[which(is.na(TestingDataSet$location))] <- 0
TestingDataSet$risk_factor[which(is.na(TestingDataSet$risk_factor))] <- 2
TestingDataSet$duration_previous[which(is.na(TestingDataSet$duration_previous))] <- 0
TestingDataSet$C_previous[which(is.na(TestingDataSet$C_previous))] <- 0

customers=unique(TestingDataSet[,1],fromLast = TRUE)
n=length(customers)
print("The number of customers are:")
print(n)

result = matrix(c(0),nrow=n,ncol=7)

countA=0
countB=0
countC=0
countD=0
countE=0
countF=0
countG=0
count=0

p=1

id=TestingDataSet[1,1]

for(i in 2:253720)
{
  temp=TestingDataSet[i,1]
  if(temp>id)
  {
    #result[p,2]=paste0(TestingDataSet[i-1,18],TestingDataSet[i-1,19],TestingDataSet[i-1,20],TestingDataSet[i-1,21],TestingDataSet[i-1,22],TestingDataSet[i-1,23],TestingDataSet[i-1,24],sep='')
    result[p,1]=TestingDataSet[i-2,18]
    result[p,2]=TestingDataSet[i-2,19]
    result[p,3]=TestingDataSet[i-2,20]
    result[p,4]=TestingDataSet[i-2,21]
    result[p,5]=TestingDataSet[i-2,22]
    result[p,6]=TestingDataSet[i-2,23]
    result[p,7]=TestingDataSet[i-2,24]
    p=p+1
    id=temp
  }
}

for(i in 1:37010)
{
  if(GroundTruthDataSet[i,18]==result[i,1])
  {
    countA=countA+1
  }
  if(GroundTruthDataSet[i,19]==result[i,2])
  {
    countB=countB+1
  }
  if(GroundTruthDataSet[i,20]==result[i,3])
  {
    countC=countC+1
  }
  if(GroundTruthDataSet[i,21]==result[i,4])
  {
    countD=countD+1
  }
  if(GroundTruthDataSet[i,22]==result[i,5])
  {
    countE=countE+1
  }
  if(GroundTruthDataSet[i,23]==result[i,6])
  {
    countF=countF+1
  }
  if(GroundTruthDataSet[i,24]==result[i,7])
  {
    countG=countG+1
  }
}


for(i in 1:37010)
{
  if((GroundTruthDataSet[i,18]==result[i,1])&&(GroundTruthDataSet[i,19]==result[i,2])&&(GroundTruthDataSet[i,20]==result[i,3])&&(GroundTruthDataSet[i,21]==result[i,4])&&(GroundTruthDataSet[i,22]==result[i,5])&&(GroundTruthDataSet[i,23]==result[i,6])&&(GroundTruthDataSet[i,24]==result[i,7]))
  {
    count=count+1
  }
}