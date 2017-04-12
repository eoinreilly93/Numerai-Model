######################################
# IMPORT NECESSARY FILES AND SETTINGS
#####################################

source("Scripts/BuildModels.R")

#DISABLE SCIENTIFIC NOTATION
options(scipen=999)

#######################################
# MAKE PREDICTIONS
######################################

#PREDICTIONS
#TAN PREDICTIONS
#RELABLE ID FOR TESTING ON TRAINED MODEL. HAS TO BE THE SAME AS IN TRAINING 
testdata <- testingdata
testdata$target <- as.factor(ifelse(testdata$t_id >= 132438, 'Won', 'Lost'))
testdata$t_id <- NULL
testdata <- testdata[,c(ncol(testdata),1:(ncol(testdata)-1))]

#MAKE PREDICTIONS
tanPred <- predict(treefitted, testdata, prob = TRUE)
tanProb <- data.frame(t(attributes(tanPred)$prob))

#COMBINE THE IDS AND PROBABILITEIS INTO SINGLE DATAFRAME
predictions <- cbind(testingdata$t_id, tanProb$Won)
colnames(predictions) <- c("t_id", "probability")
predictionsDF <- as.data.frame(predictionsDF)

#WRITE TO A FILE
write.csv(predictionsDF, "Predictions/predictions_12_04_2017.csv", row.names = FALSE)
