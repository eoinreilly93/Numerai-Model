####################
# IMPORT LIBRARIES
####################

library("bnlearn", lib.loc="~/R/win-library/3.3")
library("boot", lib.loc="~/R/win-library/3.3")

######################
# BUILD MODELS
######################

#NAIVE
naivemodel <- naive.bayes(trainingdata, "target")
naivefitted <- bn.fit(naivemodel, trainingdata, method = "bayes", iss=1)

#TAN
treemodel <- tree.bayes(trainingdata, "target")
treefitted <- bn.fit(treemodel, trainingdata, method = "bayes", iss=1)

#Logistic Regression
lrmodel <- glm(target~., trainingdata, family=binomial(link = "logit"))



