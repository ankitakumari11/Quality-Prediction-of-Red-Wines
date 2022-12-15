library('caret')
wine <- read.csv("winequality-red.csv")
set.seed(123)
wine$quality[wine$quality=="3"] <- 4
wine$quality[wine$quality=="8"] <- 7

cat("\nInterior structure of dataset: \n")
str(wine)
cat("\n\n")
wine$quality <- as.factor(wine$quality)
cat("\n\n")

cat("\nNEW Interior structure of dataset: \n")
str(wine)
wine <- upSample(wine[,-12],wine$quality,yname="quality")
cat("\n********************AFTER UPSAMPLING*******************************\n")

str(wine)
cat("\n****************************************************\n")

intrain <- createDataPartition(y = wine$quality, p= 0.8, list = FALSE)
training <- wine[intrain,]
testing <- wine[-intrain,]

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

#SVM
cat("\nsvm linear\n")
svm_Linear <- train(quality ~., data = training, method = "svmLinear",trControl=trctrl,preProcess = c("center", "scale"),tuneLength = 10)

print(svm_Linear)

test_pred <- predict(svm_Linear, newdata = testing)
cat("\n PREDICTIONG ON TESTING DATASET:\n")
print(test_pred)

cat("\n\n")
print(confusionMatrix(table(testing$quality,test_pred)))
t<-table(actual=testing$quality,predicted=test_pred)


#ACCURACY PRECISION AND RECALL
n = sum(t) 
nc = nrow(t) 
diag = diag(t) 
rowsums = apply(t, 1, sum) 
colsums = apply(t, 2, sum) 
p = rowsums / n 
q = colsums / n 
accuracy = sum(diag) / n
cat("\nThe accuracy of SVM is: ",accuracy,"\n")
precision = diag / colsums
recall = diag / rowsums
f1 = 2 * precision * recall / (precision + recall)
j<-data.frame(precision, recall, f1)
cat("\nSVM LINEAR\n")
print(j)


#svmradial
cat("\n\nSVM RADIAL\n")
library('e1071')
svm_Linear <- train(quality ~., data = training, method = "svmRadial",trControl=trctrl,preProcess = c("center", "scale"),tuneLength = 10)

print(svm_Linear)

test_pred <- predict(svm_Linear, newdata = testing)
cat("\n PREDICTIONG ON TESTING DATASET:\n")
print(test_pred)

cat("\n\n")
print(confusionMatrix(table(testing$quality,test_pred)))
t<-table(actual=testing$quality,predicted=test_pred)


#ACCURACY PRECISION AND RECALL
n = sum(t) 
nc = nrow(t) 
diag = diag(t) 
rowsums = apply(t, 1, sum) 
colsums = apply(t, 2, sum)
p = rowsums / n 
q = colsums / n 
accuracy = sum(diag) / n
cat("\nThe accuracy of SVM radial is: ",accuracy,"\n")
precision = diag / colsums
recall = diag / rowsums
f1 = 2 * precision * recall / (precision + recall)
l<-data.frame(precision, recall, f1)
cat("\nSVM RADIAL\n")
print(l)

