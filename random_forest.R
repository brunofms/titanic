##
# Sources
##
# https://www.kaggle.com/c/titanic
# https://www.kaggle.com/c/titanic/forums

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
#titanic <- titanic[, -c(1, 4, 9, 11)]
#titanic_validation <- titanic_validation[, -c(1, 3, 8, 10)]

# Remove Ouliers
# TODO

# Filling missing cabin data with the information of availability
titanic$CabinAvailable <- titanic$Cabin != ""
titanic$CabinAvailable <- as.factor(titanic$CabinAvailable)

titanic_validation$CabinAvailable <- titanic_validation$Cabin != ""
titanic_validation$CabinAvailable <- as.factor(titanic_validation$CabinAvailable)

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
titanic$Embarked <- droplevels(titanic$Embarked)

##
# FEATURE ENGINEERING
##

# Include family
library(stringr)

titanic$Surname <- sapply((str_split(string = titanic$Name, pattern = ',')), function(x) x[1])
titanic$Surname <- as.numeric(as.factor(titanic$Surname))

titanic_validation$Surname <- sapply((str_split(string = titanic_validation$Name, pattern = ',')), function(x) x[1])
titanic_validation$Surname <- as.numeric(as.factor(titanic_validation$Surname))
  
# Include title
splitted_name <- sapply((str_split(string = titanic$Name, pattern = ',')), function(x) x[2])
titanic$Title <- sapply((str_split(string = splitted_name, pattern = ' ')), function(x) x[2])
titanic$Title <- as.numeric(as.factor(titanic$Title))

splitted_name <- sapply((str_split(string = titanic_validation$Name, pattern = ',')), function(x) x[2])
titanic_validation$Title <- sapply((str_split(string = splitted_name, pattern = ' ')), function(x) x[2])
titanic_validation$Title <- as.numeric(as.factor(titanic_validation$Title))

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
titanic_validation$AgeGroup <- as.numeric(as.factor(titanic_validation$AgeGroup))

# Include fare group
fare_group <- function(fare) {
  if (!is.na(fare)){
    if (fare < 50) {
      return("lote 1")
    } else if (50 <= fare & fare < 100) {
      return("lote 2")
    } else if (100 <= fare & fare < 300) {
      return("lote 3")
    } else if (300 <= fare) {
      return("lote 4")
    }
  }
  else {
    return("not available")
  }
}

for (person in 1:nrow(titanic)) {
  titanic[person,"FareGroup"] <- fare_group(titanic[person,"Fare"])
}

for (person in 1:nrow(titanic_validation)) {
  titanic_validation[person,"FareGroup"] <- age_group(titanic_validation[person,"Fare"])
}

# Cast target attribute to factor
titanic$FareGroup <- as.numeric(as.factor(titanic$FareGroup))
titanic_validation$FareGroup <- as.numeric(as.factor(titanic_validation$FareGroup))

##
# SPLIT TRAINING DATA
##

# Split data into a training (4/5) and test set (1/5)
set.seed(123)
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
  # 'Parch',
  'Fare',
  'Embarked',
  'CabinAvailable',
  'Title',
  'FamilySize',
  'AgeGroup',
  'FareGroup',
  'Surname'
)

features_formula <- Survived ~ 
  Pclass +
  Sex +
  Age +
  SibSp +
  # Parch +
  Fare +
  Embarked +
  CabinAvailable +
  Title +
  FamilySize +
  AgeGroup +
  FareGroup +
  Surname

train_features <- subset(trainset, select=features)
test_features <- subset(testset, select=features)

train_classes <- trainset$Survived
test_classes <- testset$Survived

# Random Forest

library(randomForest)
# rf.tune <- tuneRF(train_features,
#                   train_classes,
#                  stepFactor=2,
#                  improve=1e-5,
#                  ntree=200)
# print(rf.tune)

rf.cv <- rfcv(train_features,
              train_classes, 
              cv.fold = 10,
              mtry = function(p) max(1, floor(sqrt(p))))
rf.cv

bagging.model <- randomForest(features_formula,
                        data = trainset,
                        importance=TRUE,
                        mtry = 4,
                        ntree=500)
bagging.model
str(bagging.model)
varImpPlot(bagging.model) # remover features irrelevantes (reduce variance)

# Tune Model

##
# TEST MODEL
##

# Random Forest
pred_random_forest <- predict(bagging.model, test_features)
rmse <- sqrt( mean( (as.numeric(test_classes) - as.numeric(pred_random_forest))^2 , na.rm = TRUE ) )
rmse

confusion_matrix <- table(pred_random_forest, test_classes)
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
accuracy

precision <- confusion_matrix[2,2]/sum(confusion_matrix[2,])
precision

recall <- confusion_matrix[2,2]/sum(confusion_matrix[,2])
recall

##
# SUBMIT PREDICTIONS
##

titanic_validation_features <- subset(titanic_validation, select=features)

# Random Forest
validation_pred_random_forest <- predict(bagging.model, titanic_validation_features)
prop.table(table(validation_pred_svm))

##
# CONSOLIDATE PREDCTIONS
## 
titanic_validation$Survived <- validation_pred_random_forest


##
# EXPORT TO CSV
##
export <- titanic_validation[,c("PassengerId", "Survived")]
# write.csv(export, "# Competition/export/rf_500_tc_faregroup_family(0.367194).csv", row.names=FALSE)
# write.csv(export, "# Competition/export/rf_500_tc_faregroup_family_title(0.3894681_0.8483146).csv", row.names=FALSE)
# write.csv(export, "# Competition/export/rf_500_tc_fareg_fam_tit_cab(0.3894681_0.8483146).csv", row.names=FALSE)
write.csv(export, "# Competition/export/rf_500_tc_fareg_fam_tit_cab-parch(0.3821877_0.8539326).csv", row.names=FALSE)