
# classifcation
# mean misclassification error (mmce)
# accuracy (acc)
# measures (auc)
# false positive rate (fpr)

# regression
# mean of squared errors (mse)
# mean of absolute errors (mae)

# clustering
# Dunn index (dunn)


# survival 
# Concordance Index (cindex)

# cost-sensitive predictions
# misclassification penalty (mcp)

# timetrain 
# timepredict
# timeboth



# Listing measures --------------------------------------------------------
library(mlr)

listMeasures(obj = 'classif', properties = 'classif.multi')
listMeasures(obj = iris.task)


getDefaultMeasure(x = iris.task)
getDefaultMeasure(x = makeLearner(cl = 'regr.lm'))

n <- getTaskSize(x = bh.task)
lrn <- makeLearner(cl = 'regr.gbm', n.trees = 1000)
mod <- train(learner = lrn, task = bh.task, subset = seq(1, n, 2))
pred <- predict(object = mod, task = bh.task, subset = seq(2, n, 2))
performance(pred = pred)
pred

# median of squared errors (medse)

performance(pred = pred, measures = medse)
performance(pred = pred, measures = list(mse, medse, mae))

performance(pred = pred, measures = timetrain, model = mod)
lrn <- makeLearner(cl = 'cluster.kmeans', centers = 3)
mod <- train(learner = lrn, task = mtcars.task)
pred <- predict(object = mod, task = mtcars.task)
performance(pred = pred, measures = dunn, task = mtcars.task)

lrn <- makeLearner(cl = 'classif.rpart', predict.type = 'prob')
mod <- train(learner = lrn, task = sonar.task)
pred <- predict(object = mod, task = sonar.task)
performance(pred = pred, measures = auc)

mmce
auc
str(auc)



# Binary classification ---------------------------------------------------

lrn <- makeLearner(cl = 'classif.lda', predict.type = 'prob')
n <- getTaskSize(sonar.task)
mod <- train(learner = lrn, task = sonar.task, subset = seq(1, n, by = 2))
pred <- predict(object = mod, task = sonar.task, subset = seq(2, n, by = 2))
performance(pred = pred, measures = list(fpr, fnr, mmce))

d <- generateThreshVsPerfData(obj = pred, measures = list(fpr, fnr, mmce))
plotThreshVsPerf(d)

r <- calculateROCMeasures(pred = pred)
r
str(r)
r$confusion.matrix
print(r, abbreviations = FALSE)
