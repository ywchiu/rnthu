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
