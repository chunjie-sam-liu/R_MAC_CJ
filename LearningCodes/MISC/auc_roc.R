
# library -----------------------------------------------------------------

library(plotROC)



set.seed(2529)

D.ex <- rbinom(200, size = 1, prob = 0.5)

M1 <- rnorm(200, mean = D.ex, sd = 0.65)
M2 <- rnorm(200, mean = D.ex, sd = 1.5)

test <- data.frame(D = D.ex, D.str = c('Healthy', 'Ill')[D.ex + 1], M1 = M1, M2 = M2, stringsAsFactors = FALSE)

basicplot <- ggplot(test, aes(d = D, m = M1)) + geom_roc()
ggplot(test, aes(d = D.str, m = M1)) + geom_roc()
ggplot(test, aes(d = D, m = M1)) + geom_roc(n.cuts = 0)

basicplot + style_roc()

longtest <- melt_roc(test, 'D', c("M1", "M2"))

ggplot(longtest, aes(d = D, m = M, color = name)) + geom_roc() + style_roc()



D.cov <- rbinom(400, 1, 0.5)
gender <- c("Male", "Female")[rbinom(400, 1, 0.49) + 1]
M.diff <- rnorm(400, mean = D.cov, sd = ifelse(gender == "Male", 0.5, 1.5))
test.cov <- data.frame(D = D.cov, gender = gender, M = M.diff)
bygend <- ggplot(test.cov, aes(d = D, m = M, color = gender)) + geom_roc(show.legend = FALSE)
direct_label(bygend) + style_roc()


basicplot 
  

true_Y = c(1,1,1,1,2,1,2,1,2,2)
probs = c(1,0.999,0.999,0.973,0.568,0.421,0.382,0.146,0.377,0.11)

probsSort = sort(probs, decreasing = TRUE, index.return = TRUE)
val = unlist(probsSort$x)
idx = unlist(probsSort$ix)  

roc_y = true_Y[idx]
stack_x = cumsum(roc_y == 2)/sum(roc_y == 2)
stack_y = cumsum(roc_y == 1)/sum(roc_y == 1)

auc = sum((stack_x[2:length(roc_y)]-stack_x[1:length(roc_y)-1])*stack_y[2:length(roc_y)])


getROC_AUC = function(probs, true_Y){
    probsSort = sort(probs, decreasing = TRUE, index.return = TRUE)
    val = unlist(probsSort$x)
    idx = unlist(probsSort$ix)  

    roc_y = true_Y[idx];
    stack_x = cumsum(roc_y == 2)/sum(roc_y == 2)
    stack_y = cumsum(roc_y == 1)/sum(roc_y == 1)    

    auc = sum((stack_x[2:length(roc_y)]-stack_x[1:length(roc_y)-1])*stack_y[2:length(roc_y)])
    return(list(stack_x=stack_x, stack_y=stack_y, auc=auc))
}

aList = getROC_AUC(probs, true_Y) 

stack_x = unlist(aList$stack_x)
stack_y = unlist(aList$stack_y)
auc = unlist(aList$auc)
plot(stack_x, stack_y, type = "l", col = "blue", xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC")
axis(1, seq(0.0,1.0,0.1))
axis(2, seq(0.0,1.0,0.1))
abline(h=seq(0.0,1.0,0.1), v=seq(0.0,1.0,0.1), col="gray", lty=3)
legend(0.7, 0.3, sprintf("%3.3f",auc), lty=c(1,1), lwd=c(2.5,2.5), col="blue", title = "AUC")


# two steps 

# 1. Sort the observed outcomes by their predicted scores with highest scores first.
# 2. Cal cumulative TPR and TNR for the ordered observed outcomes.

simple_roc <- function(labels, scores) {
  labels <- labels[order(scores, decreasing = TRUE)]
  data.frame(TPR = cumsum(labels) / sum(labels), FPR = cumsum(!labels) / sum(!labels), labels)
}

set.seed(1)
sim_widgetd_data <- function(N, noise = 100) {
  x <- runif(N, min = 0, max = 100)
  y <- 122 - x / 2 + rnorm(N, sd = noise)
  bad_widget <- factor(y > 100)
  data.frame(x, y, bad_widget)
}

widget_data <- sim_widgetd_data(500, 10)
test_set_idx <- sample(1:nrow(widget_data), size = floor(nrow(widget_data)) / 4)
test_set <- widget_data[test_set_idx, ]
training_set <- widget_data[-test_set_idx, ]

library(ggplot2)
library(dplyr)

test_set %>% 
  ggplot(aes(x = x, y = y, col = bad_widget)) +
  scale_color_manual(values = c('black', 'red')) +
  geom_point() +
  ggtitle("Bad widgets related to x")

fit_glm <- glm(bad_widget ~ x, training_set, family = binomial(link = 'logit'))
glm_link_scores <- predict(fit_glm, test_set, type = 'link')
glm_response_scores <- predict(fit_glm, test_set, type = 'response')
score_data <- data.frame(link = glm_link_scores, response = glm_response_scores, bad_widget = test_set$bad_widget, stringsAsFactors = F)

score_data %>% 
  ggplot(aes(x = link, y = response, col = bad_widget)) +
  scale_color_manual(values = c('black', 'red')) +
  geom_point() +
  geom_rug()
library(pROC)
plot(roc(test_set$bad_widget, glm_response_scores, direction = "<"), col = 'yellow', lwd = 3, main = "The turtle find its way")
glm_simple_roc <- simple_roc(test_set$bad_widget == "TRUE", glm_link_scores)
with(glm_simple_roc, points(1 - FPR, TPR, col=1 + labels))



# use glm

glm(formula = vs ~ wt + disp, data = mtcars, family = binomial) -> model
summary(model)

newdata = data.frame(wt = 2.1, disp = 180)
predict(model, newdata, type = 'response')
predict(model, newdata, type = 'link')
library(ResourceSelection)
hoslem.test(mtcars$vs, fitted(model))
model_weight <- glm(formula = vs ~ wt, family = binomial, data = mtcars)

xweight <- seq(0, 6, 0.01)
yweight <- predict(model_weight, list(wt = xweight),type="response")

plot(mtcars$wt, mtcars$vs, pch = 16, xlab = "WEIGHT (g)", ylab = "VS")
lines(xweight, yweight)




library(tidyverse)
library(caret)
data("PimaIndiansDiabetes2", package = "mlbench")
pima.data <- na.omit(PimaIndiansDiabetes2)
sample_n(pima.data, 3)
set.seed(123)
training.samples <- pima.data$diabetes %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- pima.data[training.samples, ]
test.data <- pima.data[-training.samples, ]
set.seed(123)
model <- train(
  diabetes ~., data = train.data, method = "svmLinear",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale")
)
# Make predictions on the test data
predicted.classes <- model %>% predict(test.data)
head(predicted.classes)
mean(predicted.classes == test.data$diabetes)
set.seed(123)
model <- train(
  diabetes ~., data = train.data, method = "svmLinear",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(C = seq(0, 2, length = 20)),
  preProcess = c("center","scale")
)
# Plot model accuracy vs different values of Cost
plot(model)

set.seed(123)
model <- train(
  diabetes ~., data = train.data, 
  method = "svmRadial",
  trControl = trainControl("repeatedcv", number = 10, classProbs = T),
  preProcess = c("center","scale"),
  tuneLength = 10
)

model$bestTune
c("#104E8B", "#1E90FF", "#EE2C2C", "#FF6A6A")


c("#006400", "#00008B", "#B22222")






