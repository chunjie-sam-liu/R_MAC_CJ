

training.data.raw <- read.csv("/home/liucj/tmp/titanic/train.csv", header = T, na.strings = c(""))
sapply(training.data.raw, function(x) sum(is.na(x)))
sapply(training.data.raw, function(x) length(unique(x)))
library(Amelia)
missmap(training.data.raw, main = "Missing values vs observed")
data <- subset(training.data.raw, select = c(2, 3, 5, 6, 7, 8, 10, 12))
data$Age[is.na(data$Age)] <- mean(data$Age,na.rm=T)
is.factor(data$Sex)
is.factor(data$Embarked)
contrasts(x = as.factor(data$Sex))
contrasts(x = as.factor(data$Embarked))


data <- data[!is.na(data$Embarked),]
rownames(data) <- NULL
data.train <- data[1:800,]
data.test <- data[801:889,]

model <- glm(Survived ~ Sex,family=binomial(link='logit'),data=train)



broom::tidy(model)
summary(model)
anova(model, test="Chisq") 
library(pscl)
pR2(model)
fitted.results <- predict(model,newdata=subset(test,select=c(2,3,4,5,6,7,8)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))

library(ROCR)
p <- predict(model, newdata=subset(test,select=c(2,3,4,5,6,7,8)), type="response")
pr <- prediction(p, test$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

0.8647186

data$Survived <- as.factor(data$Survived)
data.train <- data[1:800,]
data.test <- data[801:889,]

model <- glm(Survived ~ Sex,family=binomial(link='logit'),data=data.train)


elastic <- train(
  Survived ~., data = data.train, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
# Model coefficients
coef(elastic$finalModel, elastic$bestTune$lambda)


fitted.results <- predict(elastic,data.test,type='prob')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != data.test$Survived)
print(paste('Accuracy',1-misClasificError))

library(ROCR)
p <- predict(elastic, data.test, type='prob')
pr <- prediction(p, data.test$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


# Make predictions
predictions <- elastic %>% predict(data.test)
# Model prediction performance
data.frame(
  RMSE = RMSE(predictions, test.data$medv),
  Rsquare = R2(predictions, test.data$medv)
)
