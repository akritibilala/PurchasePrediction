TrainingDataSet<- read.csv(file.choose(),header=TRUE,sep=",")
customers=unique(TrainingDataSet[,1],fromLast = TRUE)

TrainingDataSet$location[which(is.na(TrainingDataSet$location))] <- 0

TrainingDataSet$risk_factor[which(is.na(TrainingDataSet$risk_factor))] <- 2

TrainingDataSet$duration_previous[which(is.na(TrainingDataSet$duration_previous))] <- 0

TrainingDataSet$C_previous[which(is.na(TrainingDataSet$C_previous))] <- 0

TrainingDataSet_purchasedPolicy <- subset(TrainingDataSet, TrainingDataSet$record_type > 0)

car_value= c(1:97009)
for (i in 1:97009)
{
  if (TrainingDataSet_purchasedPolicy[i,11] == "a")
  {
    car_value[i] = 1
  }
  else if (TrainingDataSet_purchasedPolicy[i,11] == "b")
  {
    car_value[i] = 2
  }
  else if (TrainingDataSet_purchasedPolicy[i,11] == "c")
  {
    car_value[i] = 3
  }
  else if (TrainingDataSet_purchasedPolicy[i,11] == "d")
  {
    car_value[i] = 4
  }
  else if (TrainingDataSet_purchasedPolicy[i,11] == "e")
  {
    car_value[i] = 5
  }
  else if (TrainingDataSet_purchasedPolicy[i,11] == "f")
  {
    car_value[i] = 6
  }
  else
  {
    car_value[i] = 7
  }
}
minimum_cost = c(1:97009)
maximum_cost = c(1:97009)
count = c(1:97009)

for (i in 1:97009)
{
  minimum_cost[i] = 1000
  maximum_cost[i] = 0
  count[i] = 0
}

j=1

for (i in 1:665249)
{
  if (TrainingDataSet[i,1] == customers[j])
  { 
    count[j] = count[j] + 1
    if (TrainingDataSet[i,25] > maximum_cost[j])
    {
      maximum_cost[j] <- TrainingDataSet[i,25]
    }
    if (TrainingDataSet[i,25] < minimum_cost[j])
    {
      minimum_cost[j] <- TrainingDataSet[i,25]
    }
  }
  else
  {
    j = j+1
    maximum_cost[j] = TrainingDataSet[i,25]
    minimum_cost[j] = TrainingDataSet[i,25]
    count[j] = 1
  }
}
TrainingDataSet_purchasedPolicy <- TrainingDataSet_purchasedPolicy[, -11]
TrainingDataSet_purchasedPolicy <- cbind(TrainingDataSet_purchasedPolicy, car_value, maximum_cost, minimum_cost, count)

write.csv(TrainingDataSet_purchasedPolicy,"C:/Users/admin/Documents/Alda/Project/TransformedDataset.csv",quote=F, row.names=F)