## R Basic

data(anscombe)
View(anscombe)
names(anscombe)
plot(y1 ~ x1, data = anscombe)
fit <- lm(y1 ~ x1, data= anscombe )
fit
abline(fit, col="red")
predict(fit, data.frame(x1 = 18))


## Iris Analysis
data(iris)
View(iris)
class(iris)
str(iris)
summary(iris)

head(iris)
tail(iris)
?head


iris[ c(1,2,3)  ,    ]
iris[ 1:6       ,    ]
iris[ 1:6       , 1:2]
iris[ 1:6       , c('Sepal.Length')]
iris$Sepal.Length
iris[iris$Species =='setosa',      ]

iris[(iris$Species =='setosa') & (iris$Sepal.Length >= 5),      ]
sort(iris$Sepal.Length)
sort(iris$Sepal.Length, decreasing = TRUE)


a <-  c(5,2,7,4,2,5)
sort(a)
order(a)

iris[order(iris$Sepal.Length, decreasing = TRUE),]

## Plot
pie(table(iris$Species))
hist(iris$Sepal.Length)
boxplot(iris$Sepal.Length)
plot(iris$Petal.Length, iris$Petal.Width, col=iris$Species)

#ML
install.packages("rpart")
library(rpart)
head(iris)
fit <- rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris)
fit
plot(fit, margin = 0.1)
text(fit)
pred <- predict(fit, iris[,-5], type='class')
table(iris$Species, pred)
(50 + 49 + 45)/ 150


plot(iris$Petal.Length, iris$Petal.Width, col=iris$Species)
abline(v = 2.45, col = "orange")
abline(h = 1.75, col = "blue")


install.packages("e1071")
library(e1071)
fit <- svm(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris)
pred <- predict(fit, iris[,-5], type='class')
table(iris$Species, pred)
(50 + 48 + 48)/ 150



## Churn Rate Analysis
install.packages("C50")
library(C50)
data(churn)
View(churnTrain)


churnTrain <- churnTrain[, ! names(churnTrain) %in% c('state', 'area_code', 'account_length')]
View(churnTrain)


set.seed(123)
sample.int(42, 6)


nrow(churnTrain)
set.seed(2)
ind <- sample(2, nrow(churnTrain), prob= c(0.7, 0.3), replace= TRUE)
trainset <- churnTrain[ind == 1, ]
testset  <- churnTrain[ind == 2, ]

#names(churnTrain)
library(rpart)
fit <- rpart(churn ~ ., data = trainset)
plot(fit, margin = 0.1)
text(fit)


predictions <- predict(fit, testset, type="class" )
table(testset$churn, predictions)


## Cross Validation
install.packages("caret")
library(caret)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <-  train(churn~., data=trainset, method="rpart", preProcess="scale", trControl=control)
model


## Pick Important Variable
#install.packages("rminer")
library(rminer)
model=fit(churn~.,trainset,model="rpart")
VariableImportance=Importance(model,trainset,method="sensv")
L=list(runs=1,sen=t(VariableImportance$imp),sresponses=VariableImportance$sresponses)
mgraph(L,graph="IMP",leg=names(trainset),col="gray",Grid=10)


## Use ROCR
install.packages('ROCR')
library(ROCR)
predictions <- predict(fit, testset, type="prob")

pred.to.roc <- predictions[, 1] 
pred.rocr <- prediction(pred.to.roc, as.factor(testset[,(dim(testset)[[2]])])) 
perf.rocr <- performance(pred.rocr, measure = "auc", x.measure = "cutoff") 
perf.tpr.rocr <- performance(pred.rocr, "tpr","fpr") 
plot(perf.tpr.rocr, colorize=T,main=paste("AUC:",(perf.rocr@y.values)))



## Clustering
data(iris)
View(iris)
fit <- hclust(dist(iris[   ,   -5]), 'ward.D2')
plot(fit)
iris.grp<- cutree(fit, k = 3)
iris.grp
rect.hclust(fit,  k = 3)


download.file('https://github.com/ywchiu/rtibame/raw/master/data/applenews.RData', 'applenews.RData')
load('applenews.RData')

a <- '【熊本強震】取消去九州　華航5月8日前退改票免手續'

install.packages('jiebaR')
library(jiebaR)





