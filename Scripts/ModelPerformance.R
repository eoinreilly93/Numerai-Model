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


##########################
# RESULTS
#########################

correctResult <- temp_testdata$target

#NAIVE RESULTS
table(temp_naivePredictions, correctResult)

#ACCURACY, TPR AND TNR CALCULATIONS
temp_naivetab <- table(temp_naivePredictions, correctResult)
temp_naiveacc <- (sum(temp_naivetab[1], temp_naivetab[2,2])/sum(temp_naivetab))*100

#TAN RESULTS
table(temp_tanPredictions, correctResult)

#ACCURACY, TPR AND TNR CALCULATIONS
temp_tantab <- table(temp_tanPredictions, correctResult)
temp_tanacc <- (sum(temp_tantab[1], temp_tantab[2,2])/sum(temp_tantab))*100

#LOGISTIC REGRESSION RESULTS
table(temp_lrPredictions, correctResult)

#ACCURACY, TPR AND TNR CALCULATIONS
temp_lrtab <- table(temp_lrPredictions, correctResult)
temp_lracc <- (sum(temp_lrtab[1], temp_lrtab[2,2])/sum(temp_lrtab))*100


