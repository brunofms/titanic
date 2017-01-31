##
# Load dataset
##
titanic <- read.csv("# Competition/data/train.csv")
titanic_validation <- read.csv("# Competition/data/test.csv")

##
# Explore Data
##
str(titanic)
names(titanic_train)

##
# Visualize Data
##

##
# FEATURE ENGINEERING
##

# Include number of family members
titanic$Family <- titanic$SibSp + titanic$Parch
titanic_validation$Family <- titanic_validation$SibSp + titanic_validation$Parch

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
  cat(person)
  titanic[person,"AgeGroup"] <- age_group(titanic[person,"Age"])
}

for (person in 1:nrow(titanic_validation)) {
  cat(person)
  titanic_validation[person,"AgeGroup"] <- age_group(titanic_validation[person,"Age"])
}

##
# CLEAN DATA
##

# Remove Ouliers
# Numeric:
# Default: median
# predictive model to fill in | regression model
# Categorical:
# Default: mode
# Gender to predict empty gender

##
# TRAIN MODEL
##

# Split data into a training (3/4) and test set (1/4)
index <- 1:nrow(titanic)
testindex <- sample(index, trunc(length(index)/4))
testset <- titanic[testindex,]
trainset <- titanic[-testindex,]

# Check representation
prop.table(table(trainset[,"Survived"]))
prop.table(table(testset[,"Survived"]))

features <- c(
  'Pclass',
  'Sex',
  'Age',
  'SibSp',
  'Parch',
  'Fare',
  # 'Cabin',
  'Embarked',
  'Family',
  'AgeGroup'
)

train_features <- subset(trainset, select=features)
test_features <- subset(testset, select=features)

train_classes <- subset(trainset, select=Survived)
test_classes <- subset(testset, select=Survived)

# Decision Tree
library(rpart)
start_time = Sys.time()
params <- rpart.control(minsplit=10, minbucket=7, maxdepth=40, cp=0.01)
fitTree <- rpart(Survived ~ 
                   Pclass +
                   Sex + 
                   Age +
                   SibSp +
                   Parch +
                   Fare +
                   # Cabin + (factor Cabin has new levels)
                   Embarked +
                   Family +
                   AgeGroup,
                 control = params,
                 data = trainset)
end_time = Sys.time()
end_time - start_time

library(rpart.plot)
rpart.plot(fitTree, type=2, extra=100, branch.lty=3)

# Tune Model

# Evaluating model
pred <- predict(fitTree, test_features)
pred[pred > .5] = 1
pred[pred <= .5] = 0

rmse <- sqrt( mean( (test_classes-pred)^2 , na.rm = TRUE ) )
rmse

##
# MAKE PREDICTIONS
##

titanic_validation_features <- subset(titanic_validation, select=features)
pred <- predict(fitTree, titanic_validation_features)
pred[pred > .5] = 1
pred[pred <= .5] = 0
Prediction <- c(pred)
titanic_validation <- cbind(titanic_validation, Prediction)

##
# EXPORT TO CSV
##
export <- titanic_validation[,c("PassengerId", "Prediction")]
colnames(export)[2] <- "Survived"
write.csv(export, "# Competition/export/decision_tree(0.4839775).csv", row.names=FALSE)