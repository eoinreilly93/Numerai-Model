####################
# IMPORT DATA 
####################

trainingdata <- read.csv("Resources/Datasets/numerai_training_data.csv", header = T, sep = ",")
testingdata <- read.csv("Resources/Datasets/numerai_tournament_data.csv", header = T, sep = ",")


#################################################
# CONVERT CONTINUOUS DATA INTO CATEGORICAL DATA
################################################

numColumns <- ncol(trainingdata)

#CONVERT TRAINING DATA
i = 1
j = 1

while (i < numColumns)
{
  trainingdata[, j] <- cut(trainingdata[, j], 5, include.lowest = TRUE, labels = c('VSmall', 'Small', 'Medium','Large', 'VLarge'))
  i = i + 1
  j = j + 1
  
}

trainingdata$target <- as.factor(ifelse(trainingdata$target == 0, "Lost", "Won"))

#CONVERT TEST DATA
i = 2
j = 2

while (i <= numColumns)
{
  testingdata[, j] <- cut(testingdata[, j], 5, include.lowest = TRUE, labels = c('VSmall', 'Small', 'Medium','Large', 'VLarge'))
  i = i + 1
  j = j + 1
  
}


#MOVE TARGET COLUMN TO START OF DATA
trainingdata <- trainingdata[,c(ncol(trainingdata),1:(ncol(trainingdata)-1))]
