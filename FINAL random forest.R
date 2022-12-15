wine <- read.csv("winequality-red.csv") 
set.seed(123)
barplot(table(wine$quality))
wine$quality[wine$quality=="3"] <- 4
wine$quality[wine$quality=="8"] <- 7


wine$quality <- as.factor(wine$quality)
str(wine)

cat("The interior of quality column:- \n")
print(str(wine$quality))
cat("\n-------------------------------------------------------\n")
barplot(table(wine$quality)) 
cat("\n Table of wine quality: \n")
print(table(wine$quality) )
cat("\n-------------------------------------------------------\n")

#balancing the dataset
library(caret)
nwine <-  upSample(wine[,-12],wine$quality,yname="quality")
cat("\n\n********************AFTER UPSAMPLING*******************************\n\n")

str(nwine)
cat("table: \n")
print(table(nwine$quality))
barplot(table(nwine$quality)) 

# Moving onto the Data visualization

library(ggplot2)
print(ggplot(wine,aes(fixed.acidity,volatile.acidity))+ geom_point(aes(color=quality)))# This command is used to display a scatter plot. The output looks like below
print(ggplot(wine,aes(alcohol)) + geom_histogram(aes(fill=quality),color='black',bins=50) )# This command is used to display a stacked bar chart. The output looks like below

#Splitting the dataset
random_state=134
samp <- sample(nrow(nwine), 0.8 * nrow(nwine))
train <- nwine[samp, ]
test <- nwine[-samp, ]



cat("\n")
cat("dimension of training: ",dim(train))
cat("\n")
cat("dimension of training: ",dim(test))  
cat("\n-------------------------------------------------------\n")


#Random forest
library(randomForest)
model <- randomForest(quality ~ ., data = train, ntree =500, mtry = 5)
cat("\n\n")
cat("model: \n")
print(model)
t<-model$confusion
t<-t[,-5]


cat("\nThe edited confusion matrix\n")
print(t)



prediction <- predict(model, newdata = test)


results<-cbind(prediction,test$quality)
colnames(results)<-c('pred','real')
results<-as.data.frame(results)
View(results)
cat("---------------------------------------------------------------------------------\n")


# Accuracy

cm<-t
n = sum(cm) 
nc = nrow(cm) 
diag = diag(cm)
rowsums = apply(cm, 1, sum) 
colsums = apply(cm, 2, sum) 
p = rowsums / n 
q = colsums / n 
accuracy = sum(diag) / n
cat("\naccuracy of classifier is :",accuracy,"\n")
precision = diag / colsums
recall = diag / rowsums
f1 = 2 * precision * recall / (precision + recall)
k<- data.frame(precision, recall, f1)
print(k)