source("Scripts/BuildModels.R")

###########################
# 10 FOLD CROSS VALIDATION
###########################

nb10CV = bn.cv(trainingdata, naivemodel, loss = "pred", k = 10,loss.args = list(target = "target"), fit = "mle", debug = FALSE)
tan10CV = bn.cv(trainingdata, treemodel, loss = "pred", k = 10,loss.args = list(target = "target"), fit = "mle", debug = FALSE)

cv.err.10 <- cv.glm(trainingdata, lrmodel, K=10)
print(paste("10 Fold CV Error Rate: ", cv.err.10$delta[1]*100))

##################################
# TEST ON TRAINING DATA
##################################

#SPLIT TRAINING DATA 70/30 IN TRAINING AND TESTING
temp_traindata <- trainingdata[1:121691, ]
temp_testdata <- trainingdata[121692:173844, ]

#BUID NEW MODELS
#NAIVE
temp_naivemodel <- naive.bayes(temp_traindata, "target")
temp_naivefitted <- bn.fit(temp_naivemodel, temp_traindata, method = "bayes", iss=1)

#TAN
temp_treemodel <- tree.bayes(temp_traindata, "target")
temp_treefitted <- bn.fit(temp_treemodel, temp_traindata, method = "bayes", iss=1)

#Logistic Regression
temp_lrmodel <- glm(target~., temp_traindata, family=binomial(link = "logit"))

################################
# MAKE PREDICTIONS
################################

#NAIVE PREDICTIONS
temp_naivePred <- predict(temp_naivefitted, temp_testdata, prob = TRUE)
temp_naiveProb <- data.frame(t(attributes(temp_naivePred)$prob))
temp_naivePredictions <- ifelse(temp_naiveProb$Won >= .5, "WIN", "LOSS")

#TAN PREDICTIONS
temp_tanPred <- predict(temp_treefitted, temp_testdata, prob = TRUE)
temp_tanProb <- data.frame(t(attributes(temp_tanPred)$prob))
temp_tanPredictions <- ifelse(temp_tanProb$Won >= .5, "WIN", "LOSS")

#LOGISTIC REGRESSION PREDICTIONS
temp_lrprob <- predict(temp_lrmodel, newdata=temp_testdata, type="response")
temp_lrPredictions<- ifelse(temp_lrprob>= 0.5, 'WIN', 'LOSS')

#AVERAGING ENSEMBLE 
combinedProbs <- cbind(temp_naiveProb, temp_tanProb, temp_lrprob)
combinedProbs$Lost <- NULL
combinedProbs$Lost <- NULL
colnames(combinedProbs) <- c("Naive Bayes", "TAN", "Logistic Regression")
combinedProbs$EnsembleProb <- (rowSums(combinedProbs))/3
combinedProbs$AverageEnsemblePred <- ifelse(combinedProbs$EnsembleProb >= .5, "WIN", "LOSS")


#VOTING ENSEMBLE
VotingEnsemblePrediction <- ifelse(temp_naivePredictions == temp_tanPredictions, temp_naivePredictions, 
                                   ifelse(temp_naivePredictions != temp_tanPredictions & temp_naivePredictions 
                                          != temp_lrPredictions,temp_lrPredictions,temp_lrPredictions ))


combinedPredictions <- cbind(temp_naivePredictions, temp_tanPredictions, temp_lrPredictions, VotingEnsemblePrediction)
##########################
# RESULTS
#########################

correctResult <- temp_testdata$target

#NAIVE RESULTS
table(temp_naivePredictions, correctResult)

#ACCURACY, TPR AND TNR CALCULATIONS
temp_naivetab <- table(temp_naivePredictions, correctResult)
temp_naiveacc <- (sum(temp_naivetab[1], temp_naivetab[2,2])/sum(temp_naivetab))*100
temp_naivetpr <- (temp_naivetab[2,2]/sum(temp_naivetab[,2]))*100
temp_naivetnr <- (temp_naivetab[1,1]/sum(temp_naivetab[,1]))*100

#TAN RESULTS
table(temp_tanPredictions, correctResult)

#ACCURACY, TPR AND TNR CALCULATIONS
temp_tantab <- table(temp_tanPredictions, correctResult)
temp_tanacc <- (sum(temp_tantab[1], temp_tantab[2,2])/sum(temp_tantab))*100
temp_tantpr <- (temp_tantab[2,2]/sum(temp_tantab[,2]))*100
temp_tantnr <- (temp_tantab[1,1]/sum(temp_tantab[,1]))*100

#LOGISTIC REGRESSION RESULTS
table(temp_lrPredictions, correctResult)

#ACCURACY, TPR AND TNR CALCULATIONS
temp_lrtab <- table(temp_lrPredictions, correctResult)
temp_lracc <- (sum(temp_lrtab[1], temp_lrtab[2,2])/sum(temp_lrtab))*100
temp_lrtpr <- (temp_lrtab[2,2]/sum(temp_lrtab[,2]))*100
temp_lrtnr <- (temp_lrtab[1,1]/sum(temp_lrtab[,1]))*100


#AVERAGING ENSEMBLE RESULTS
table(combinedProbs$AverageEnsemblePred , correctResult)

#ACCURACY, TPR AND TNR CALCULATIONS
temp_avgtab <- table(combinedProbs$AverageEnsemblePred , correctResult)
temp_avgacc <- (sum(temp_avgtab[1], temp_avgtab[2,2])/sum(temp_avgtab))*100
temp_avgtpr <- (temp_avgtab[2,2]/sum(temp_avgtab[,2]))*100
temp_avgtnr <- (temp_avgtab[1,1]/sum(temp_avgtab[,1]))*100

#VOTING ENSEMBLE RESULTS
table(VotingEnsemblePrediction, correctResult)
temp_vottab <- table(VotingEnsemblePrediction, correctResult)
temp_votacc <- (sum(temp_vottab[1], temp_vottab[2,2])/sum(temp_vottab))*100
temp_vottpr <- (temp_vottab[2,2]/sum(temp_vottab[,2]))*100
temp_vottnr <- (temp_vottab[1,1]/sum(temp_vottab[,1]))*100
