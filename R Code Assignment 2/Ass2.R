#Sampling the data + removing NA's
rm(list = ls())
WAUS <- read.csv("WAUS2020.csv")
L <- as.data.frame(c(1:49))
set.seed(29677068) # Your Student ID is the random seed
L <- L[sample(nrow(L), 10, replace = FALSE),] # sample 10 locations
WAUS <- WAUS[(WAUS$Location %in% L),]
WAUS <- WAUS[sample(nrow(WAUS), 2000, replace = FALSE),] # sample 2000 rows
WAUSComplete = WAUS[complete.cases(WAUS), ] #removing NA's


#Question 1
#to calculate proprotion of rainy days and fine days in Rain Today 
rd = length(which(WAUS$RainToday == "Yes"))
View(WAUS)
fd = length(which(WAUS$RainToday == "No"))
View(rd)
View(fd)

#to find out proportion of rainy days and fine days in Rain Tomorrow 
rm = length(which(WAUS$RainTomorrow == "Yes"))
fm = length(which(WAUS$RainTomorrow == "No"))

#to get summary stats 
rainSumm <- lm(Rainfall ~  MinTemp + MaxTemp + Evaporation + Sunshine + WindGustSpeed + WindSpeed9am + WindSpeed3pm + Humidity9am + Humidity3pm + Pressure9am + Pressure3pm + Cloud9am + Cloud3pm + Temp9am + Temp3pm, data = WAUSComplete)
rainSumm
apply(WAUSComplete [1:17],2,sd)#to get standard deviation for WAUS predictor variables  
print(round(colMeans(WAUSComplete[1:17], na.rm = TRUE), digits =2))# to get mean for WAUS predictor variables 
summary(rainSumm)

#Extra Stuff
#rainSumm$residuals
#qqnorm(rainSumm$residuals)
#qqline(rainSumm$residuals)
#attach(WAUSComplete)
#plot(Rainfall, rainSumm$residuals)

#################################################################

#Pre-processing to make the data suitable 
#Question 2
WAUSComplete = WAUS[complete.cases(WAUS), ]
WAUSComplete$WindDir9am = NULL
WAUSComplete$WindDir3pm = NULL
WAUSComplete$WindGustDir = NULL
WAUSComplete$Day = NULL
WAUSComplete$Month = NULL
WAUSComplete$Year = NULL

##################################################################

#Question 3 - dividing data set into 70% training and 30% testing 
set.seed(29677068) #Student ID as random seed
train.row = sample(1:nrow(WAUSComplete), 0.7*nrow(WAUSComplete))
WAUSComplete.train = WAUSComplete[train.row,]
WAUSComplete.test = WAUSComplete[-train.row,]

##################################################################
#implementing each classifier model and then using the testing data to create a confusion matrix 
#Question 4 and 5 
#Decision tree
install.packages("tree")
library(tree)
WAUS.decision.tree = tree(RainTomorrow~., data = WAUSComplete.train)
WAUS.decision.tree
summary(WAUS.decision.tree)
WAUS.decision.tree$weights
plot(WAUS.decision.tree)
text(WAUS.decision.tree, pretty =0)
title("Decision Tree for prediciting Rain Tomorrow")

#testing data plus confusion matrix
WAUS.decision.tree.predict = predict(WAUS.decision.tree, WAUSComplete.test, type = "class")
t.1 = table(observed = WAUSComplete.test$RainTomorrow, predicted = WAUS.decision.tree.predict)
cat("\n#Decision Tree Confusion\n")
print(t.1)

#Naive Bayes
install.packages("e1071")
library(e1071)
WAUSNavBay = naiveBayes(RainTomorrow~., data = WAUSComplete.train)

#testing data plus confusion matrix 
WAUSNavBay.predict = predict(WAUSNavBay, WAUSComplete.test)
WAUSNavBay.predict
table(actual = WAUSComplete.test$RainTomorrow, predicted = WAUSNavBay.predict)

#Bagging
install.packages("adabag")
install.packages("rpart")
library(adabag)
library(rpart) 
WAUSbag1 = bagging(RainTomorrow~., data = WAUSComplete.train, mfinal = 10)

#testing data plus confusion matrix 
WAUSbag1.predict = predict.bagging(WAUSbag1, newdata = WAUSComplete.test)
table(observed = WAUSComplete.test$RainTomorrow, predicted = WAUSbag1.predict$class)

#Boosting
library(rpart)
library(adabag)
WAUSboost1 <- boosting(RainTomorrow	~	., data = WAUSComplete.train, mfinal=10)

#testing data plus confusion matrix
WAUSboost1.predcit = predict.boosting(WAUSboost1, newdata = WAUSComplete.test)
table(observed = WAUSComplete.test$RainTomorrow, predicted = WAUSboost1.predcit$class)

#Random Forests 
install.packages("randomForest")
library(randomForest)
WAUSrandomf = randomForest(RainTomorrow~., data = WAUSComplete.train,  na.action = na.exclude)
WAUSrandomf$importance
#testing data plus confusion matrix
WAUSrandomf.predict = predict(WAUSrandomf, WAUSComplete.test)
table(observed = WAUSComplete.test$RainTomorrow, predicted = WAUSrandomf.predict)
a = predict(WAUSrandomf, WAUSComplete.test, type = "prob")
a
plot(randomForest(RainTomorrow~., data = WAUSComplete.train, keep.forest =FALSE, ntree=100))

######################################################################
#This question creates a ROC Curve plotting each classifier on it and also calculates the AUC for each classifier 
#Question 6
#Decision Tree ROC 
install.packages(("ROCR"))
library(ROCR)
WAUS.dt.pred= predict(WAUS.decision.tree, WAUSComplete.test, type = "vector")
# computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
# labels are actual values, predictors are probability of class
WAUSpred <- prediction(WAUS.dt.pred[,2], WAUSComplete.test$RainTomorrow)
WAUSperf <- performance(WAUSpred,"tpr","fpr")
plot(WAUSperf, add = TRUE, col = "lightgreen") #Add to ROC Curve
abline(0,1)
calculate =	performance(WAUSpred,	"auc") #Calculate AUC
print(as.numeric(calculate@y.values))

#Naive Bayes
WAUS.nb.pred = predict(WAUSNavBay, WAUSComplete.test, type = 'raw')
WAUSpred.bayes <- prediction(WAUS.nb.pred[,2], WAUSComplete.test$RainTomorrow)
WAUSperf.bayes <- performance(WAUSpred.bayes,"tpr","fpr")
plot(WAUSperf.bayes, add=TRUE, col = "blueviolet") #add to ROC curve
calculate2 =	performance(WAUSpred.bayes,	"auc")
print(as.numeric(calculate2@y.values))

#Bagging
WAUSpred.bagg <- prediction(WAUSbag1.predict$prob[,2], WAUSComplete.test$RainTomorrow)
WAUSperf.bagg <- performance(WAUSpred.bagg,"tpr","fpr")
plot(WAUSperf.bagg, add=TRUE, col = "blue") #Add to ROC Curve 
calculate3 =	performance(WAUSpred.bagg,	"auc") #Calculate AUC
print(as.numeric(calculate3@y.values))

#Boosting
WAUSpred.boost <- prediction(WAUSboost1.predcit$prob[,2], WAUSComplete.test$RainTomorrow)
WAUSperf.boost <- performance(WAUSpred.boost,"tpr","fpr")
plot(WAUSperf.boost, add=TRUE, col = "red")#Add to ROC Curve 
calculate4 =	performance(WAUSpred.boost,	"auc") #Calculate AUC
print(as.numeric(calculate4@y.values))

#Random Forest
WAUSpred.rf <- predict(WAUSrandomf, WAUSComplete.test, type="prob")
# WAUSrf.pred
WAUS.rf.pred <- prediction(WAUSpred.rf[,2], WAUSComplete.test$RainTomorrow)
WAUS.rf.perf <- performance(WAUS.rf.pred,"tpr","fpr")
plot(WAUS.rf.perf, add=TRUE, col = "orange")#Add to ROC Curve 
calculate5 =	performance(WAUS.rf.pred,	"auc") #Calculate AUC
print(as.numeric(calculate5@y.values))

legend("right", c("decision tree", "naive bayes", "bagging", "boosting", "random forest"), lty=1, 
       col = c("lightgreen","blueviolet", "blue", "red", "orange"), bty="n", inset=c(0,-0.15))
title("ROC Curve comparing Classifiers at various confidence intervals")

#####################################################################

#Question 8
#Attribute importance
cat("\n#Decision Tree Attribute Importance\n")
print(summary(WAUS.decision.tree))
cat("\n#Baging Attribute Importance\n")
print(WAUSbag1$importance)
cat("\n#Boosting Attribute Importance\n")
print(WAUSboost1$importance)
cat("\n#Random Forest Attribute Importance\n")
print(WAUSrandomf$importance)

##############################################################################

#Improved classifer of decision tree (cross-validating and pruning)
#Question 9 
#Pruned decision tree
prune.dt = cv.tree(WAUS.decision.tree, FUN = prune.misclass)
prune.dt
WAUS.prune.dt = prune.misclass(WAUS.decision.tree, best = 6)
WAUS.prune.dt
summary(WAUS.prune.dt)
plot(WAUS.prune.dt)
text(WAUS.prune.dt, pretty = 0)
prunepredict =	predict(WAUS.prune.dt, WAUSComplete.test,	type	=	"class")
table(actual	=	WAUSComplete.test$RainTomorrow,	predicted	=	prunepredict)
title("Pruned Decsion Tree")

#Calculate AUC 
WAUS.ppred9 = predict(WAUS.decision.tree, WAUSComplete.test, type = "vector")
WAUS.prunedtree.pred <- prediction(WAUS.ppred9[,2], WAUSComplete.test$RainTomorrow)
WAUSperf <- performance(WAUSpred,"tpr","fpr")
plot(WAUSperf)

abline(0,1)
calculate6 =	performance(WAUSpred,	"auc")
print(as.numeric(calculate6@y.values))

##########################################################################

#Artificial Neural Network 
#Question 10
rm(list = ls())
options(digits=4)
install.packages("car")
install.packages("neuralnet")
library(car)
library(neuralnet)
A <- read.csv("WAUS2020.csv")
L <- as.data.frame(c(1:49))
set.seed(29677068) # Your Student ID is the random seed
L <- L[sample(nrow(L), 10, replace = FALSE),] # sample 10 locations
A <- A[(A$Location %in% L),]
A <- A[sample(nrow(A), 2000, replace = FALSE),] # sample 2000 rows
A = A[complete.cases(A), ]

A$RainTomorrow = ifelse(A$RainTomorrow == "Yes", '1', '0')
A$RainTomorrow = as.numeric(A$RainTomorrow)

artificial.n.n = as.data.frame(scale(A[c(9,17,19)]))
artificial.n.n = cbind(artificial.n.n, A$RainTomorrow)
colnames(artificial.n.n)[4]= "RainTomorrow"

index = sample(2, NROW(artificial.n.n), replace = TRUE, prob = c(0.7, 0.3)) 
artificial.neural.train = artificial.n.n[index == 1,]
artificial.neural.test = artificial.n.n[!index == 1,]
artificial.testing.df = as.data.frame(artificial.neural.test)

neural.network = neuralnet(RainTomorrow ~ Sunshine + Humidity3pm + Pressure3pm, artificial.neural.train, hidden=3, linear.output = FALSE)
plot(neural.network,	rep="best")

neural.network$result.matrix

artificial.pred = neuralnet::compute(neural.network, artificial.neural.test[c(1:3)])
artificial.pred = as.data.frame(round(artificial.pred$net.result, 0))
table(observed = artificial.neural.test$RainTomorrow, predicted = artificial.pred$V1)

