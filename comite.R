##
# CONSTANTS
##
CHANCES_OF_DEATH = 0.3838
THRESHOLD = 0.5


##
# LOAD DATASET
##
titanic <- read.csv("# Competition/data/train.csv")
titanic_validation <- read.csv("# Competition/data/test.csv")

# check data
colSums(is.na(titanic))
colSums(titanic == "")

colSums(is.na(titanic_validation))
colSums(titanic_validation == "")

##
# CLEAN DATA
##

# Cast target attribute to factor
titanic$Survived <- as.factor(titanic$Survived)

# Remove worthless columns (PassengerID, Name, Ticket, and Cabin attributes)
titanic <- titanic[, -c(1, 4, 9, 11)]
titanic_validation <- titanic_validation[, -c(1, 3, 8, 10)]

# Remove Ouliers
# TODO

# Filling missing cabin data with the information of availability
# TODO

# Filling missing age data with median
# NUMERIC is fulfilled with median, or we can build a predictive | regression model to fill in
age_median = median(titanic$Age, na.rm = TRUE)
missing_age_indices <- is.na(titanic$Age)
titanic[missing_age_indices, "Age"] <- age_median

age_validation_median = median(titanic_validation$Age, na.rm = TRUE)
missing_validation_age_indices <- is.na(titanic_validation$Age)
titanic_validation[missing_validation_age_indices, "Age"] <- age_validation_median

# Filling missing fare data with median
fare_validation_median = median(titanic_validation$Fare, na.rm = TRUE)
missing_validation_fare_indices <- is.na(titanic_validation$Fare)
titanic_validation[missing_validation_fare_indices, "Fare"] <- fare_validation_median

# Filling missing embarked data with mode
# CATEGORICAL is fulfilled with mode()
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

embarked_mode = getmode(titanic$Embarked)
missing_embarked_indices <- (titanic$Embarked == "")
titanic[missing_embarked_indices, "Embarked"] <- embarked_mode

##
# FEATURE ENGINEERING
##

# Include number of family members
titanic$FamilySize <- titanic$SibSp + titanic$Parch
titanic_validation$FamilySize <- titanic_validation$SibSp + titanic_validation$Parch

# Include age group
age_group <- function(age) {
  if (!is.na(age)){
    if (age < 15) {
      return("crianca")
    } else if (15 <= age & age < 25) {
      return("jovens")
    } else if (25 <= age & age < 35) {
      return("jovens adultos")
    } else if (35 <= age & age < 45) {
      return("adultos")
    } else if (45 <= age & age < 55) {
      return("meia idade")
    } else if (55 <= age) {
      return("seniores")
    }
  }
  else {
    return("not available")
  }
}

for (person in 1:nrow(titanic)) {
  titanic[person,"AgeGroup"] <- age_group(titanic[person,"Age"])
}

for (person in 1:nrow(titanic_validation)) {
  titanic_validation[person,"AgeGroup"] <- age_group(titanic_validation[person,"Age"])
}

# Cast target attribute to factor
titanic$AgeGroup <- as.numeric(as.factor(titanic$AgeGroup))

##
# SPLIT TRAINING DATA
##

# Split data into a training (4/5) and test set (1/5)
index <- 1:nrow(titanic)
testindex <- sample(index, trunc(length(index)/5))
testset <- titanic[testindex,]
trainset <- titanic[-testindex,]

# Check representation
prop.table(table(trainset[,"Survived"]))
prop.table(table(testset[,"Survived"]))


##
# TRAIN MODEL
##

features <- c(
  'Pclass',
  'Sex',
  'Age',
  'SibSp',
  'Parch',
  'Fare',
  'Embarked',
  'FamilySize',
  'AgeGroup'
)

features_formula <- Survived ~ 
  Pclass +
  Sex +
  Age +
  SibSp +
  Parch +
  Fare +
  Embarked +
  FamilySize +
  AgeGroup

train_features <- subset(trainset, select=features)
test_features <- subset(testset, select=features)

train_classes <- subset(trainset, select=Survived)
test_classes <- subset(testset, select=Survived)

# Decision Tree

library(rpart)
params <- rpart.control(minsplit=20, minbucket=7, maxdepth=30, cp=0.01)
fitTree <- rpart(features_formula,
                 control = params,
                 data = trainset)

library(rpart.plot)
rpart.plot(fitTree, type=2, extra=100, branch.lty=3)

# Random Forest

library(randomForest)
bagging.model <- randomForest(features_formula,
                        data = trainset,
                        importance=TRUE,
                        # mtry = 2,
                        ntree=100)
bagging.model
str(bagging.model)
varImpPlot(bagging.model) # remover features irrelevantes (reduce variance)

# SVM

library(e1071)
svm.model <- svm(features_formula,
                 data = trainset,
                 kernel='linear',
                 gamma=0.5)
str(svm.model)
summary(svm.model)

# Logistic Regression
# log_regression.model <- glm(features_formula,
                # data=Smarket,family=binomial)

# Tune Model

##
# TEST MODEL
##

# All Dead
predAllDead <- rep(CHANCES_OF_DEATH, nrow(testset))

# Decision Tree
predDecisionTree <- predict(fitTree, test_features)

# Random Forest
pred_random_forest <- predict(bagging.model, test_features)

# SVM
pred_svm <- predict(svm.model, test_features)

# Logistic Regression

# Comitê
comite <- list(
  allDead = 1,
  DecisionTree = 0,
  random_forest = 2,
  svm = 1
  )

test_prediction <- (comite$allDead * predAllDead +
                      comite$DecisionTree * as.numeric(predDecisionTree) +
                      comite$random_forest * as.numeric(as.character(pred_random_forest)) + 
                      comite$svm * as.numeric(as.character(pred_svm))) / 
  (comite$allDead +
     comite$DecisionTree +
     comite$random_forest +
     comite$svm)
test_prediction[test_prediction > THRESHOLD] = 1
test_prediction[test_prediction <= THRESHOLD] = 0

rmse <- sqrt( mean( (test_classes-test_prediction)^2 , na.rm = TRUE ) )
rmse

##
# SUBMIT PREDICTIONS
##

titanic_validation_features <- subset(titanic_validation, select=features)

# All Dead
validationPredAllDead <- CHANCES_OF_DEATH

# Decision Tree
validation_pred_decision_tree <- predict(fitTree, titanic_validation_features)

# Random Forest
validation_pred_random_forest <- predict(bagging.model, titanic_validation_features)

# SVM Pred
validation_pred_svm <- predict(svm.model, titanic_validation_features)

##
# CONSOLIDATE PREDCTIONS
## 
final_pred <- (comite$allDead * validationPredAllDead +
                 comite$DecisionTree * validation_pred_decision_tree +
                 comite$random_forest * as.numeric(validation_pred_random_forest) + 
                 comite$svm * validation_pred_svm) / 
  (comite$allDead +
     comite$DecisionTree +
     comite$svm)
final_pred[final_pred > THRESHOLD] = 1
final_pred[final_pred <= THRESHOLD] = 0
Survived <- c(final_pred)
titanic_validation <- cbind(titanic_validation, Survived)


##
# EXPORT TO CSV
##
export <- titanic_validation[,c("PassengerId", "Survived")]
write.csv(export, "# Competition/export/3svm_2dt_1dead(0.5028011).csv", row.names=FALSE)