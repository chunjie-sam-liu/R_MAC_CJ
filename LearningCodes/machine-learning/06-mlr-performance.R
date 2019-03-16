
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


# Plot performance versus threshold


# resampling --------------------------------------------------------------

# resampling
# performance the resampling
# accessing resample results

# stratification, blocking and grouping
# resample description and resample instances
# Aggregating performance values

# 1. Cross-validation (CV)
# 2. Leave-one-out cross-validation (LOO)
# 3. Repeated cross-validation (RepCV)
# 4. Out-of-bag bootstrap and other variants like b632 (Bootstrap)
# 5. Subsampling, also called Monte-Carlo cross-validation (subsample)
# 6. Holdout (training/test) (Holdout)

rdesc <- makeResampleDesc(method = 'CV', iters = 3)

rdesc <- makeResampleDesc(method = 'Holdout')

holdout
cv3

rdesc <- makeResampleDesc(method = 'CV', iters = 3)

# Calculate the performance
r <- resample(learner = 'regr.lm', task = bh.task, resampling = rdesc)

names(r)
r$aggr
r$measures.test


rdesc <- makeResampleDesc(method = 'Subsample', iters = 5)
rdesc <- makeResampleDesc(method = 'Subsample', iters = 5, split = 4/5)

lrn <- makeLearner(cl = 'classif.rpart', parms = list(split = 'information'))

r <- resample(learner = lrn, task = sonar.task, resampling = rdesc, measures = list(mmce, fpr, fnr, timetrain))


getRRPredictionList(res = r)

addRRMeasure(res = r, measures = list(ber, timepredict))

getRRPredictions(r)

r$pred


rdesc <- makeResampleDesc(method = 'Holdout', predict = 'both')
r <- resample(learner = 'classif.lda', task = iris.task, resampling = rdesc, show.info = FALSE)
r$pred

names(r)
r$measures.test
r$measures.train


predList <- getRRPredictionList(r)


rdesc <- makeResampleDesc(method = 'CV', iters = 3)
r <- resample(learner = 'surv.coxph', task = lung.task, resampling = rdesc, show.info = FALSE, models = T)

names(r)
r$learner.id
r$task.id
getRRPredictionList(r)
r$models


rdesc <- makeResampleDesc(method = 'CV', iters = 3)
r <- resample(learner = 'cluster.kmeans', task = mtcars.task, resampling = rdesc, show.info = FALSE, centers = 3, extract = function(x) getLearnerModel(x)$centers)


r <- resample(learner = 'regr.rpart', task = bh.task, resampling = rdesc, show.info = F, extract = getFeatureImportance, keep.pred = T)

r$extract

getResamplingIndices(r)

# stratification, blocking, grouping


# 3-fold cross-validation

rdesc <- makeResampleDesc(method = 'CV', iters = 3, stratify = TRUE)
r <- resample(learner = 'classif.lda', task = iris.task, resampling = rdesc, show.info = FALSE)

r$measures.test
r$aggr
r$extract

# stratification with respect to explanatory variables


# Tuning ------------------------------------------------------------------

# 1. the search space
# 2. optimization algorithm (aka tuning method)
