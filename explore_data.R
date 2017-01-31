##
# Load dataset
##
titanic <- read.csv("# Competition/data/train.csv")
titanic_validation <- read.csv("# Competition/data/test.csv")

##
# Understand Data
##
str(titanic)

colSums(with(titanic,df == Inf))
colSums(is.na(trainset))
colSums(is.na(titanic_validation))
colSums(titanic == "")

##
# Explore Data
##
table(titanic$Survived, titanic$Sex)
table(titanic$Survived)

library(dummies)

##
# Visualize Data
##
library(ggplot2)
boxplot(Fare~Survived, data=titanic[titanic$Fare < 65,])
boxplot(Age~Survived, data=titanic)

hist(titanic$Age,10)
boxplot(Fare~Survived, data=titanic)
boxplot(Fare~Pclass, data=titanic)
boxplot(Pclass~Survived, data=titanic)

hist(as.numeric(titanic$Ticket))