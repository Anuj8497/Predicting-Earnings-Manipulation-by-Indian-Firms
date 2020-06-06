install.packages("readxl")
library(readxl)
install.packages("caret")
library(caret)
install.packages("ROSE")
library(ROSE)
install.packages("ROCR")
library(ROCR)
install.packages("Metrics")
library(Metrics)
library(pROC)

dataset <- read_excel("complete.xlsx")

# Converting C-Manipulator to factor"
dataset$`C-MANIPULATOR`<- as.factor(dataset$`C-MANIPULATOR`)

# Lets check for the missing cases.
sum(complete.cases(dataset)) # all the cases are complete. No missing data present.

#a) Does the Beneish model developed in 1999 hold good for Indian data?

# For the Beneish model, The threshold is -2.22. If Mscore is less than -2.22 then the company is unlikely
# to be a manipulator.If greater than 2.22 then the company is likely to be a manipulator.

# The mscore is calculated using the following formula.
dummy <- dataset
dummy$score <- -4.84 + (0.92*dummy$DSRI) + (0.528*dummy$GMI) + (0.404*dummy$AQI) + (0.892*dummy$SGI) + 
  (0.115*dummy$DEPI) - (0.172*dummy$SGAI) + (4.679*dummy$ACCR) - (0.327*dummy$LEVI)
dummy$manipulate <- ifelse(dummy$score < -2.22,0, 1) 
dummy$manipulate <- as.factor(dummy$`manipulate`)

# Now lets plot a confusion matrix between columns C-Manipulator and manipulate to check the relevancy of Benish model
matrix = confusionMatrix(dummy$`C-MANIPULATOR`,dummy$manipulate , positive = '1')

# From the output we can see that Sensitivity is 9.5% and specificity is 100%%
# Specificity is how well class 0 is predicted. Whereras Sensitivity is how well class 1 is predicted.
# Recall of 9.5% indicates that there are many Non -Manipulators which are actually classified as manipulators.
# Hence, we reach to a consensus that Beneish model does not hold good to the Indian data.


#b) Which models are robust to unbalanced data? How can one handle unbalanced problems?

# Class imbalance problems may lead to the optimization of a meaningless metric.
# For example accuracy paradox. It is the case where your accuracy measures tell the
# story that you have excellent accuracy (such as 90%)
# but the accuracy is only reflecting the underlying class distribution.
# Decision trees are robust to imbalanced data. For example, decision tree algorithms
# like C4.5, C5.0, CART, and Random Forest.
# Various ways to handle class imbalance problem are as follows.
# Try resampling your dataset.
# Try changing your performance metric
# Try to collect more data.
# try penalized model for example cost-sensitive model.

#c) Create a stepwise logisitc regression model using sample data. 
sample <- read_excel("sample.xlsx")
sample$`C-MANIPULATOR` <- as.factor(sample$`C-MANIPULATOR`)

# Now there are 39 cases which are manipulators and 181 which are not. Class imbalance exists. We will be using
# undersampling technique to undersample the number of majority cases( in our case class 0)

# Spliting the data into training and testing set
set.seed(1234)
index <- sample(2, nrow(sample), replace = T, prob = c(0.7,0.3))

TrainData <- sample[index == 1, ]
TestData <- sample[index == 2, ]


TrainData <- TrainData[-c(1,10)]
TrainData$`CMANIPULATOR` <- TrainData$`C-MANIPULATOR` 
TrainData <- TrainData[-c(9)]


# Applying undersampling technique on the training set.
under <- ovun.sample(`CMANIPULATOR`~.,data = TrainData, method = 'under', N= 61)$data
table(under$CMANIPULATOR)

# Now we will use this undersampled data as the training data to make a stepwise logistic regression model.
null <- glm(CMANIPULATOR ~ 1, data = under, family = "binomial")
full <- glm(CMANIPULATOR~. , data = under , family = "binomial")
step(null, scope = list(lower = null, upper = full), direction = "both")

# Following model is the better one when used step wise method
#Coefficients:
#  (Intercept)      DSRI         SGI          AQI         ACCR          GMI  
#   -15.497        3.125        6.794        1.644       12.212        1.250  

# Degrees of Freedom: 60 Total (i.e. Null);  55 Residual
# Null Deviance:	    84.55 
# Residual Deviance: 35.36 	AIC: 47.36

# creating another sample of data
under_1 <- ovun.sample(`CMANIPULATOR`~.,data = TrainData, method = 'under', N= 56)$data
table(under_1$CMANIPULATOR)
null_1 <- glm(CMANIPULATOR ~ 1, data = under_1, family = "binomial")
full_1 <- glm(CMANIPULATOR~. , data = under_1 , family = "binomial")
step(null_1, scope = list(lower = null_1, upper = full_1), direction = "both")
# Following model is the better one when used step wise method
# Coefficients:
#  (Intercept)    DSRI          SGI          AQI         ACCR         DEPI         LEVI  
# -15.105        3.435        5.612        1.233        6.535        4.181       -1.740  

# Degrees of Freedom: 55 Total (i.e. Null);  49 Residual
# Null Deviance:	    76.99 
# Residual Deviance: 34.85 	AIC: 48.85

under_2 <- ovun.sample(`CMANIPULATOR`~.,data = TrainData, method = 'under', N= 50)$data
table(under_2$CMANIPULATOR)
null_2 <- glm(CMANIPULATOR ~ 1, data = under_2, family = "binomial")
full_2 <- glm(CMANIPULATOR~. , data = under_2 , family = "binomial")
step(null_2, scope = list(lower = null_2, upper = full_2), direction = "both")
# Following model is the better one when used step wise method
# Coefficients:
#  (Intercept)      DSRI          SGI          AQI         ACCR          GMI         LEVI  
#   -10.071        1.400        5.173        1.095        9.629        2.443       -1.825  

# Degrees of Freedom: 49 Total (i.e. Null);  43 Residual
# Null Deviance:	    66.41 
# Residual Deviance: 34.97 	AIC: 48.97



under_3 <- ovun.sample(`CMANIPULATOR`~.,data = TrainData, method = 'under', N= 47)$data
table(under_3$CMANIPULATOR)
null_3 <- glm(CMANIPULATOR ~ 1, data = under_3, family = "binomial")
full_3 <- glm(CMANIPULATOR~. , data = under_3 , family = "binomial")
step(null_3, scope = list(lower = null_3, upper = full_3), direction = "both")
# Following model is the better one when used step wise method
# Coefficients:
#  (Intercept)  DSRI          SGI         ACCR          AQI          GMI  
# -6.1063       0.9880       2.7142       5.5831       0.5611       0.8355  

# Degrees of Freedom: 46 Total (i.e. Null);  41 Residual
# Null Deviance:	    60.28 
# Residual Deviance: 43.78 	AIC: 55.78


# Now we kept a threshold for sampling i.e. at least 50% when it came to undersampling.
# After running step wise logistic regression model on four different samples, we say that model with following
# coefficients works best.

under_best <- ovun.sample(`CMANIPULATOR`~.,data = TrainData, method = 'under', N= 58)$data
under_best <- under_best[-c(5,6,8)]
best <- glm(CMANIPULATOR ~ . , data = under_best, family = "binomial")
summary(best)

#  Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#  (Intercept) -10.8887     3.0823  -3.533 0.000411 ***
#  DSRI          1.3339     0.4084   3.266 0.001089 ** 
#  GMI           3.5471     1.3173   2.693 0.007090 ** 
#  AQI           0.9337     0.2850   3.276 0.001053 ** 
#  SGI           3.1864     1.2849   2.480 0.013144 *  
#  ACCR         14.7033     5.0060   2.937 0.003312 ** 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

# Null deviance: 80.129  on 57  degrees of freedom
# Residual deviance: 41.827  on 52  degrees of freedom
# AIC: 53.827

# Number of Fisher Scoring iterations: 7

predict <- predict(best, newdata = TestData, type="response")
# Now we have to decide an optimal cut-off point for distinguishing between Manipulators 
# and Non-Manipulators

pred <- prediction(predict,TestData$`C-MANIPULATOR`)
perf_ROC <- performance(pred,"tpr","fpr") 
plot(perf_ROC)
abline(a=0,b=1)


# From the plot we see the gain sensitivity(tpr > 0.95), trading off a false positive rate(1 - specificity)
#(>0.15). After this we don't se increase in true positive rate for a trade off increase in false positive rate.

# Now lets see AUC
auc <- performance(pred,"auc")
auc

auc <- unlist(slot(auc, "y.values"))
auc

# Now lets see the optimal cutoff point


opt.cut <- function(perf)
{
  cut.ind <- mapply(FUN = function(x,y,p)
    {
    d = (x-0)^2 + (y-1)^2
    ind <- which(d == min(d))
    c(recall = y[ind] , specificity = 1 - x[ind] , cutoff = p[ind])
  },perf@x.values,perf@y.values,perf@alpha.values)
}  
    
ans <- opt.cut(perf_ROC)
# From this we get that optimal cut-off point will be 0.60
# i.e. if probability is > 0.6 then distinguish it as manipulators else as non-manipulators.

# Lets use youden index method to find the optimal cut-point.
object <- roc(TestData$`C-MANIPULATOR`,predict )
  
coords(object, x="best", input="threshold", best.method="youden", transpose = TRUE) 
plot(object)
# From this approach we get the optimal cut point as 0.58.

# Now lets test our model on the complete dataset.
s <- predict(best, newdata = dataset[-c(1,6,7,9,10)], type="response")

classs <- ifelse(s>0.60, '1','0')
classs <- data.frame(classs)
confusionMatrix(classs$classs,dataset$`C-MANIPULATOR`,positive = '1')
# No we are able to get a Sensitivity of ~84% and Specificity ~88.9%. We were able to improve the sensitivity.
# Since there is a trade off between specificity and sensitivity increase in sensitivty was accompanied by 
# slight decrease in specificity.

# For suggesting a cut-off score to distinguish between manipulators and non-manipulators, lets sort the 
# probabilities of prediction
prob = data.frame(sort(s, decreasing = TRUE))
# From it we see that for instance 28 the probability is exactly the cut-off. Any instances with probability 
# 0.6023749 will be classified as Manipulators.
# Hence calculating the Mscore by using the ratio values of this particular instance.

summary(best)
Mscore  = -10.887 + 1.3339*2.1657 + 3.5471*0.9864 + 0.9337*0.90673 + 3.1864*1.1976 + 14.7033*0.0173
# 0.4177 is the threshold MCA Technologies should use. If any company has a Mscore above 0.4177, there is a
# a chance that particular company is manipulating its earnings.

#########################################################################################################

# Now lets try other machine Learning models

# 1 Decision tree

install.packages("rpart")
library(rpart)
library(rpart.plot)
tree<- rpart(CMANIPULATOR~DSRI+GMI+AQI+SGI+DEPI+ACCR+SGAI+LEVI, data =
               under , method = 'class', parms = list(split = "gini"))

p <- predict(tree, newdata = TestData, type = 'class')

confusionMatrix(p, TestData$`C-MANIPULATOR`,positive = '1')
# The model has a decent Specificity but when it comes to Sensitivity(ability to predict manipulators),
# it is very low.
rpart.plot(tree)

# Lets try inserting a cp value to improve the performance of the model.
printcp(tree)
# From this we see that, error is least when cp = 0.01

# lets try the model now with a value of cp = 0.01
p <- predict(tree, newdata = TestData, type = 'class',control = rpart.control(cp = 0.01))
confusionMatrix(p, TestData$`C-MANIPULATOR`,positive = '1')
# No change.

# 2 Random Forest
install.packages("randomForest")
library(randomForest)
rf = randomForest(CMANIPULATOR~DSRI+GMI+AQI+SGI+ACCR+LEVI+DEPI+SGAI, data = under , ntree = 1000, proximity = TRUE , replace = TRUE, importance = TRUE , sampsize = ceiling(0.65*nrow(under)) ,mtry = sqrt(ncol(under)))
p1 <- predict(rf,newdata = TestData)
confusionMatrix(p1,TestData$`C-MANIPULATOR`,positive = '1')

# Certainly random forest method is a improvement upon Decision tree. And why not since it considers subset subset
# of all posssible features and then runs decision tree algorithm on each subset.

# 3 Adaboosting
install.packages("adabag")
library(adabag)
adaboost <- boosting(CMANIPULATOR ~ ., data = under, mfinal = 10, control = rpart.control(maxdepth = 1))
adaboost$importance
p2 <- predict.boosting(adaboost,newdata = data.frame(TestData) , type = "response")
a <- data.frame(p2$class)
confusionMatrix(a$p2.class,TestData$`C-MANIPULATOR`,positive = '1')
# The Specificity decreases slighly when we copare random forest with Adaboosting. While sensitivity 
# remains the same.

# Recommendations to MCA Technologies.
# Definitely the Beneish model of 1999 cannot be applicable to the Indian Data. So, feature selection should 
# be done to get the most important variables.
# To overcome class imbalance problem, MCA can experiment with techniques like oversampling, SMOTE to get a 
# balanced datasets.
# From our analysis, when feature selection ,parameter tuning, undersampling are done , model performs the best. We get
# a good balance between the values of sensitivity and specificity. 
# Step wise Logisitc regression with parameter tuning performs best and MCA technologies should definitely use it.