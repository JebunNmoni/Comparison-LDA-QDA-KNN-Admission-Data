#### Perform an exploratory analysis of data ####
# Understand variables
# Find patterns in data
# Suggest modeling stategies

# Clear console
cat("\014")
dev.off()

# Remove all variables
rm(list=ls(all=TRUE)) 

# Set working directory
setwd('C:\\Users\\Md Arafat H Khan\\Dropbox\\Academics - University of Texas at Dallas\\STAT 6340.18s Statistical and Machine Learning - Spring 2018\\Mini Projects\\Project 03')
# Load Data
admission = read.csv("admission.csv")

#### Exploratory Data Analysis ####
str(admission)
plot(admission$GPA,admission$Group)
plot(admission$GMAT,admission$Group)
plot(admission$GMAT,admission$GPA)
cor(admission)

#Install if the package doesn't exist 
if (!require(ISLR)) install.packages('ISLR')
library(ISLR) 
if (!require(MASS)) install.packages('MASS')
library(MASS)

# Training Data and Test Data
testDataFronEachGroup = 5;
group1 = admission[admission$Group == 1, ]
group2 = admission[admission$Group == 2, ]
group3 = admission[admission$Group == 3, ]
trainingDatagroup1 = group1[1:(nrow(group1)-5),]
trainingDatagroup2 = group2[1:(nrow(group2)-5),]
trainingDatagroup3 = group3[1:(nrow(group3)-5),]
testDatagroup1 = group1[(nrow(group1)-testDataFronEachGroup+1):nrow(group1),]
testDatagroup2 = group2[(nrow(group2)-testDataFronEachGroup+1):nrow(group2),]
testDatagroup3 = group3[(nrow(group3)-testDataFronEachGroup+1):nrow(group3),]
trainingData = rbind(trainingDatagroup1, trainingDatagroup2, trainingDatagroup3)
testData = rbind(testDatagroup1, testDatagroup2, testDatagroup3)

# 1(b) LDA
lda.fit = lda(Group~GPA+GMAT, data=trainingData)
lda.fit
plot(lda.fit)
lda.pred = predict(lda.fit, trainingData)
class(lda.pred)
table(lda.pred$class,trainingData$Group)
source("decisionplot.R")
plot(trainingData$GPA,trainingData$GMAT)
decisionplot(lda.fit, trainingData, class = "Group", main = "LDA")
legend("topleft", pch=c(3,2,4), col=c("green", "red","blue"), c("Group 1", "Group 2", "Group 3"))
1-mean(lda.pred$class == trainingData$Group)
lda.pred = predict(lda.fit, testData)
class(lda.pred)
table(lda.pred$class,testData$Group)
source("decisionplot.R")
plot(testData$GPA,testData$GMAT)
decisionplot(lda.fit, testData, class = "Group", main = "LDA")
legend("topleft", pch=c(3,2,4), col=c("green", "red","blue"), c("Group 1", "Group 2", "Group 3"))
1-mean(lda.pred$class == testData$Group)


# 1(c) QDA

qda.fit = qda(Group~GPA+GMAT, data=trainingData)
qda.fit
plot(qda.fit)
qda.pred = predict(qda.fit, trainingData)
class(qda.pred)
table(qda.pred$class,trainingData$Group)
source("decisionplot.R")
plot(trainingData$GPA,trainingData$GMAT)
decisionplot(qda.fit, trainingData, class = "Group", main = "qda")
legend("topleft", pch=c(3,2,4), col=c("green", "red","blue"), c("Group 1", "Group 2", "Group 3"))
1-mean(qda.pred$class == trainingData$Group)
qda.pred = predict(qda.fit, testData)
class(qda.pred)
table(qda.pred$class,testData$Group)
source("decisionplot.R")
plot(testData$GPA,testData$GMAT)
decisionplot(qda.fit, testData, class = "Group", main = "qda")
legend("topleft", pch=c(3,2,4), col=c("green", "red","blue"), c("Group 1", "Group 2", "Group 3"))
1-mean(qda.pred$class == testData$Group)



