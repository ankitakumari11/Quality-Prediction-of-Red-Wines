wine <- read.csv("winequality-red.csv") 

wine$quality[wine$quality=="3"] <- 4
wine$quality[wine$quality=="8"] <- 7


wine$quality <- as.factor(wine$quality)
cat("\nInterior structure of dataset: \n")
str(wine)
cat("\n\n")

library(caret)
cat("\n after Upsampling\n")
wine <- upSample(wine[,-12], wine$quality, yname="quality")
cat("\nNEW Interior structure of dataset: \n")
str(wine)


#Splitting the data set

samp <- sample(nrow(wine), 0.8 * nrow(wine))
train <- wine[samp, ]
test <- wine[-samp, ]


#Naive Bayes
library(naivebayes)
model <- naive_bayes(quality ~ ., data = train,usekernel=T) 
print(model)
p1 <- predict(model, train)
t <- table(actual=train$quality, predicted=p1)
print(t)


#Accuracy, precision and recall
n = sum(t) 
nc = nrow(t) 
diag = diag(t) 
rowsums = apply(t, 1, sum) 
colsums = apply(t, 2, sum) 
p = rowsums / n 
q = colsums / n                    


accuracy = sum(diag) / n 
cat("\nThe accuracy of Naive Bayes: ", accuracy,"\n")
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
y<-data.frame(precision, recall, f1)
print(y)