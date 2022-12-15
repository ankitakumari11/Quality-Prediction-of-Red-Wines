prc <- read.csv("winequality-red.csv") 
set.seed(123)

str(prc)    

prc$quality[prc$quality=="3"] <- 4
prc$quality[prc$quality=="8"] <- 7
prc$quality <- as.factor(prc$quality)


library(caret)
prc <-  upSample(prc[,-12],prc$quality,yname="quality")


#NORMALIZATION
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
prc_n <- as.data.frame(lapply(prc[1:11], normalize))

#SPLITING THE DATASET
samp <- sample(nrow(prc), 0.8 * nrow(prc))
prc_train <- prc_n[samp,-12]
prc_test <- prc_n[-samp,-12]
prc_train_labels <- prc[samp, 12]
prc_test_labels <- prc[-samp, 12]  

#KNN 
library(class)
prc_test_pred <- knn(train = prc_train, test = prc_test,cl = prc_train_labels, k=5)
t <- table(actual=prc_test_labels,predicted=prc_test_pred)
cat("\n\nCONFUSION MATRIX:- \n")
print(t)


#ACCURACY,PRECISION AND RECALL
n = sum(t) 
nc = nrow(t) 
diag = diag(t) 
rowsums = apply(t, 1, sum) 
colsums = apply(t, 2, sum) 
p = rowsums / n 
q = colsums / n                    


accuracy = sum(diag) / n 
cat("\nThe accuracy of model : ",accuracy,"\n\n")
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
s<-data.frame(precision, recall, f1)
print(s)