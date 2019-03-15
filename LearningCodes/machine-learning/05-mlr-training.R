

# training a learner means fitting a model to given data set.
# calling funciton train() on a Learner and a suitable Task().


library(magrittr)
library(mlr)

task <- makeClassifTask(id = 'iris', data = iris, target = 'Species')
lrn <- makeLearner(cl = 'classif.lda')

mod <- train(learner = lrn, task = task)
mod


mod <- train(learner = 'classif.lda', task = task)


mod <- train('surv.coxph', task = lung.task)



# accessing learning model ------------------------------------------------

data(ruspini, package = 'cluster')
plot(y ~ x, ruspini)


ruspini.task <- makeClusterTask(data = ruspini)
lrn <- makeLearner(cl = 'cluster.kmeans', centers = 4)
mod <- train(learner = lrn, task = ruspini.task)

names(mod)


mod$learner
mod$task.desc
mod$learner.model
mod$features

getLearnerModel(mod)

n <- getTaskSize(bh.task)
train.set <- sample(n, size = n /3)
mod <- train(learner = 'regr.lm', bh.task, subset = train.set)

target <- getTaskTargets(bc.task)
tab <- as.numeric(table(target))
w <- 1/tab[target]

mod <- train(learner = 'classif.rpart', task = bc.task, weights = w)

#


# predict -----------------------------------------------------------------

n <- getTaskSize(x = bh.task)

train.set <- seq(1, n, by = 2)
test.set <- seq(1, n, by = 2)

lrn <- makeLearner(cl = 'regr.gbm', n.trees = 100)
mod <- train(learner = lrn, task = bh.task, subset = train.set)

task.pred <- predict(object = mod, task = bh.task, subset = test.set)

bh.task.test <-  subsetTask(task = bh.task, subset = test.set)
df.test <-  getTaskData(bh.task.test)

bh.pred <- predict(object = mod, newdata = df.test)


n <- nrow(iris)

iris.train <- iris[seq(1, n, by = 2), -5]
iris.test <- iris[seq(1, n, by = 2), -5]

task <- makeClusterTask(id = 'cluster', data = iris.train)
mod <- train(learner = 'cluster.kmeans', task = task)

newdata.pred <- predict(object = mod, newdata = iris.test)

#accessing prediction
names(newdata.pred)

getPredictionTruth(pred = task.pred)
getPredictionResponse(pred = task.pred)
getPredictionProbabilities(pred = task.pred)


listLearners(obj = bh.task, check.packages = FALSE, properties = 'se')[c('class', 'name')]

lrn.lm <- makeLearner(cl = 'regr.lm', predict.type = 'se')

mod.lm <- train(learner = lrn.lm, task = bh.task, subset = train.set)
task.pred.lm <- predict(object = mod.lm, task = bh.task, subset = test.set)
getPredictionSE(task.pred.lm) %>% head



lrn <- makeLearner(cl = 'cluster.cmeans', predict.type = 'prob')
mod <- train(learner = lrn, task = mtcars.task)
pred <- predict(object = mod, task = mtcars.task)


getPredictionProbabilities(pred = pred)

mod <- train(learner = 'classif.lda', task = iris.task)
pred <- predict(object = mod, task = iris.task)
pred

lrn <- makeLearner(cl = 'classif.rpart', predict.type = 'prob')
mod <- train(learner = lrn, task = iris.task)
pred <- predict(object = mod, newdata = iris)

conf.matrix <- calculateConfusionMatrix(pred = pred, relative = TRUE, sums = TRUE)

#





