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

##
# JOIN TRAINS AND TEST DATASETS
##

titanic_validation$Survived <- NA
full_titanic <- rbind(titanic, titanic_validation)

##
# CLEAN DATA
##

# Filling missing cabin data with the information of availability
missing_cabin_indices <- full_titanic$Cabin == ""
levels(full_titanic$Cabin) <- c(levels(full_titanic$Cabin),"U0")
full_titanic[missing_cabin_indices, "Cabin"] <- "U0"

# Filling missing fare data with median
fare_median = median(full_titanic$Fare, na.rm = TRUE)
missing_fare_indices <- is.na(full_titanic$Fare)
full_titanic[missing_fare_indices, "Fare"] <- fare_median

# Filling missing embarked data with mode
# CATEGORICAL is fulfilled with mode()
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

embarked_mode = getmode(full_titanic$Embarked)
missing_embarked_indices <- (full_titanic$Embarked == "")
full_titanic[missing_embarked_indices, "Embarked"] <- embarked_mode
full_titanic$Embarked <- droplevels(full_titanic$Embarked)

# Filling missing age data with median
# NUMERIC is fulfilled with median, or we can build a predictive | regression model to fill in
age_median = median(full_titanic$Age, na.rm = TRUE)
missing_age_indices <- is.na(full_titanic$Age)
full_titanic[missing_age_indices, "Age"] <- age_median

# Predicting missing age data with regression model
# NUMERIC is fulfilled with median, or we can build a predictive | regression model to fill in
# upper_whisker <- boxplot.stats(full_titanic$Age)$stats[5]
# outlier.filter <- full_titanic$Age <= upper_whisker
# 
# age.equation <- "Age ~ Pclass + Fare + Sex + SibSp + Parch + Embarked"
# age.model <- lm(formula = age.equation,
#    data = full_titanic[outlier.filter,])
# 
# missing_age_indices <- is.na(full_titanic$Age)
# age.rows <- full_titanic[missing_age_indices, c("Pclass", "Fare", "Sex", "SibSp", "Parch", "Embarked")]
# age.predictions <- predict(age.model, newdata = age.rows)
# 
# full_titanic[missing_age_indices, "Age"] <- age.predictions

# Cast target attribute to factor
full_titanic$Survived <- as.factor(full_titanic$Survived)

# Remove worthless columns (PassengerID, Name, Ticket, and Cabin attributes)
#titanic <- titanic[, -c(1, 4, 9, 11)]
#titanic_validation <- titanic_validation[, -c(1, 3, 8, 10)]

# Remove Ouliers
# TODO

##
# FEATURE ENGINEERING
##

# Dummies for Sex
for (level in unique(full_titanic$Sex)) {
  full_titanic[(paste("Sex", level, sep = "_"))] <- as.numeric(full_titanic$Sex == level)
}

# Dummies for Embarked
for (level in unique(full_titanic$Embarked)) {
  full_titanic[(paste("Embarked", level, sep = "_"))] <- as.numeric(full_titanic$Embarked == level)
}

# Include Ticket Prefix
library(stringr)
full_titanic$TicketPrefix <- as.numeric(as.factor(str_extract(full_titanic$Cabin, "([a-zA-Z\\.\\/]+)")))

# Dummies for Title
for (level in unique(full_titanic$TicketPrefix)) {
  full_titanic[(paste("TicketPrefix", level, sep = "_"))] <- as.numeric(full_titanic$TicketPrefix == level)
}

# Include Cabin Letter
full_titanic$CabinLetter <- as.numeric(as.factor(str_extract(full_titanic$Cabin, "([a-zA-Z]+)")))
full_titanic$CabinRoom <- as.numeric(as.factor(str_extract(full_titanic$Cabin, "([0-9]+)")))

# Dummies for CabinLetter
for (level in unique(full_titanic$CabinLetter)) {
  full_titanic[(paste("CabinLetter", level, sep = "_"))] <- as.numeric(full_titanic$CabinLetter == level)
}

# Include Cabin Room
room_mode = getmode(full_titanic$CabinRoom)
missing_room_indices <- is.na(full_titanic$CabinRoom)
full_titanic[missing_room_indices, "CabinRoom"] <- room_mode

# Include name size
full_titanic$NameSize <- length(str_split(string = full_titanic$Name, pattern = ',')[[1]])

# Include family
full_titanic$Surname <- sapply((str_split(string = full_titanic$Name, pattern = ',')), function(x) x[1])
full_titanic$Surname <- as.numeric(as.factor(full_titanic$Surname))
  
# Include title
splitted_name <- sapply((str_split(string = full_titanic$Name, pattern = ',')), function(x) x[2])
full_titanic$Title <- sapply((str_split(string = splitted_name, pattern = ' ')), function(x) x[2])

# Group low-occuring, related titles together
full_titanic[full_titanic$Title == 'Jonkheer.', "Title"] <- 'Master.'
full_titanic[full_titanic$Title == 'Ms.' | full_titanic$Title == 'Mlle.', "Title"] <-  'Miss.'
full_titanic[full_titanic$Title == 'Mme.', "Title"] <- 'Mrs.'
full_titanic[full_titanic$Title == 'Capt.' | 
               full_titanic$Title == 'Don.' | 
               full_titanic$Title == 'Major.' | 
               full_titanic$Title == 'Col.', "Title"] <-  'Sir.'
full_titanic[full_titanic$Title == 'Dona.' | full_titanic$Title == 'the', "Title"] <-  'Lady.'

# Include Mother
full_titanic$Mother <- 0
full_titanic$Mother[full_titanic$Sex == 'female' & 
                      full_titanic$Parch > 0 & 
                      full_titanic$Age > 18 & 
                      full_titanic$Title != 'Miss.'] <- 1

# Typecasting Title
full_titanic$Title <- as.numeric(as.factor(full_titanic$Title))

# Dummies for Title
for (level in unique(full_titanic$Title)) {
  full_titanic[(paste("Title", level, sep = "_"))] <- as.numeric(full_titanic$Title == level)
}

# Include number of family members
full_titanic$FamilySize <- full_titanic$SibSp + full_titanic$Parch

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

for (person in 1:nrow(full_titanic)) {
  full_titanic[person,"AgeGroup"] <- age_group(full_titanic[person,"Age"])
}

# Cast target attribute to factor
full_titanic$AgeGroup <- as.numeric(as.factor(full_titanic$AgeGroup))

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

for (person in 1:nrow(full_titanic)) {
  full_titanic[person,"FareGroup"] <- fare_group(full_titanic[person,"Fare"])
}

# Cast target attribute to factor
full_titanic$FareGroup <- as.numeric(as.factor(full_titanic$FareGroup))

##
# SEPARATE DATASETS
##

# check data
# colSums(is.na(full_titanic))
# colSums(full_titanic == "")

titanic <- full_titanic[1:891,]
titanic_validation <- full_titanic[892:1309,]

# trainset = titanic[-subset,]
# testset = titanic[subset,]
# 
# test_features <- subset(testset, select=features)
# test_classes <- as.numeric(as.character(testset$Survived))

# colSums(is.na(titanic))
# colSums(titanic == "")
# 
# colSums(is.na(titanic_validation))
# colSums(titanic_validation == "")

##
# TRAIN MODEL
##

features <- c(
  'Pclass',
  # 'Sex',
  'Sex_male',
  'Sex_female',
  'Age',
  'SibSp',
  'Fare',
  'Parch',
  # 'TicketPrefix',
  'TicketPrefix_9',
  'TicketPrefix_3',
  'TicketPrefix_5',
  'TicketPrefix_7',
  'TicketPrefix_4',
  'TicketPrefix_1',
  'TicketPrefix_2',
  'TicketPrefix_6',
  'TicketPrefix_8',
  # 'CabinLetter',
  'CabinLetter_9',
  'CabinLetter_3',
  'CabinLetter_5',
  'CabinLetter_7',
  'CabinLetter_4',
  'CabinLetter_1',
  'CabinLetter_2',
  'CabinLetter_6',
  'CabinLetter_8',
  'CabinRoom',
  'NameSize',
  # 'Title',
  'Title_5',
  'Title_6',
  'Title_4',
  'Title_3',
  'Title_8',
  'Title_7',
  'Title_1',
  'Title_2',
  'Mother',
  'FamilySize',
  'AgeGroup',
  'FareGroup',
  'Surname',
  # 'Embarked',
  'Embarked_S',
  'Embarked_C',
  'Embarked_Q'
)

features_formula <- Survived ~ 
  Pclass +
  # Sex +
  Sex_male +
  Sex_female +
  Age +
  SibSp +
  Fare +
  Parch +
  # TicketPrefix +
  TicketPrefix_9 +
  TicketPrefix_3 +
  TicketPrefix_5 +
  TicketPrefix_7 +
  TicketPrefix_4 +
  TicketPrefix_1 +
  TicketPrefix_2 +
  TicketPrefix_6 +
  TicketPrefix_8 +
  # CabinLetter +
  CabinLetter_9 +
  CabinLetter_3 +
  CabinLetter_5 +
  CabinLetter_7 +
  CabinLetter_4 +
  CabinLetter_1 +
  CabinLetter_2 +
  CabinLetter_6 +
  CabinLetter_8 +
  CabinRoom +
  NameSize +
  # Title +
  Title_5 +
  Title_6 +
  Title_4 +
  Title_3 +
  Title_8 +
  Title_7 +
  Title_1 +
  Title_2 +
  Mother +
  FamilySize +
  AgeGroup +
  FareGroup +
  Surname +
  # Embarked +
  Embarked_S +
  Embarked_C +
  Embarked_Q

# CForest w/ Cross Validation
# http://topepo.github.io/caret/model-training-and-tuning.html
# http://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/

library(caret)

set.seed(123)

# Set up caret to perform 10-fold cross validation repeated 3 times
caret.control <- trainControl(method = "repeatedcv",
                              number = 4,
                              repeats = 1,
                              search = "random")

# tunegrid <- expand.grid(mtry = (1:5)*4)

# Use caret to train the rpart decision tree using 10-fold cross 
# validation repeated 3 times
# cv.fit <- train(features_formula, 
#                   data = titanic,
#                   method = "cforest",
#                   trControl = caret.control,
#                   #tuneGrid = tunegrid,
#                   tuneLength = 15,
#                   metric = "Accuracy")

cv.fit <- train(features_formula,
                data = titanic,
                method = "cforest",
                trControl = caret.control,
                controls = cforest_unbiased(ntree = 2000),
                tuneLength = 20,
                metric = "AccuracySD")

# Display the results of the cross validation run
cv.fit
cv.fit$results

trellis.par.set(caretTheme())
plot(cv.fit)

##
# RETRAIN MODEL W/ 100% DATA
##
# final.model <- randomForest(features_formula,
#                         data = titanic,
#                         importance=TRUE,
#                         mtry = 5,
#                         ntree = 5000)
# varImpPlot(final.model) # remover features irrelevantes (reduce variance)

# final.model <- cforest(features_formula,
#                data = titanic,
#                mtry = 10,
#                ntree = 2000)
# varimp(final.model) # remover features irrelevantes (reduce variance)

##
# SUBMIT PREDICTIONS
##

# cforest Forest
validation_pred_random_forest <- predict(cv.fit, newdata = titanic_validation)
prop.table(table(validation_pred_random_forest))

##
# CONSOLIDATE PREDCTIONS
## 
titanic_validation$Survived <- validation_pred_random_forest


##
# EXPORT TO CSV
##
export <- titanic_validation[,c("PassengerId", "Survived")]
write.csv(export, "# Competition/export/cf_caret_all(27_1800).csv", row.names=FALSE)