
# library -----------------------------------------------------------------

library(mlr)
library(magrittr)

# Binary classifiers a threshold (cutoff) value controls how predicted posterior probabilites are converted into class labels.


# ROC curves and other performance plots serve to visualize and analyse the relationship between one or two performance measures and threshold.


# receiver operating characteristics (ROC) curves
# plot the true positive rate (sensitivity) on the vertical axis agians the false positive rate (1 - specificity, fall-out) on the horizontal axis for all threshold.


# 1. determining an optimal decision threshold for given class prior probabilities and misclassification costs.
# 2. identifying regions where one classifier outperforms another and building suitable multi-classifier systems.
# 3. obtaining calibrated estimates of the posterior probabilities.



# diagnostic tests.
# spam detection.

# Only use learners that are capable of prediciting probabilities.
listLearners(obj = 'classif', properties = c('twoclass', 'prob'), check.packages = F)



n <- getTaskSize(x = sonar.task)
train.set <- sample(n, size = round(2/3 * n))
test.set <- setdiff(x = seq_len(n), train.set)

lrn1 <- makeLearner(cl = 'classif.lda', predict.type = 'prob')
mod1 <- train(learner = lrn1, task = sonar.task, subset = train.set)
pred1 <- predict(object = mod1, task = sonar.task, subset = test.set)

df <- generateThreshVsPerfData(obj = pred1, measures = list(fpr, tpr, mmce))
df$data %>% head()
plotROCCurves(df)

performance(pred = pred1, measures = auc)
plotThreshVsPerf(obj = df)


lrn2 <- makeLearner(cl = 'classif.ksvm', predict.type = 'prob')
mod2 <- train(learner = lrn2, task = sonar.task, subset = train.set)
pred2 <- predict(object = mod2, task = sonar.task, subset = test.set)

performance(pred = pred2, measures = auc)
performance(pred = pred1, measures = auc)

df2 <- generateThreshVsPerfData(obj = list(lda = pred1, ksvm = pred2), measures = list(fpr, tpr))
plotROCCurves(df2)


library(ggplot2)
qplot(x = fpr, y = tpr, color = learner, data = df2$data, geom = 'path')


# precision/recall graph
# precision = positive predictive value = ppv
# recall = tpr

# sensitivity/specificity plot
# sensitivity = tpr, specificity = tnr

df <- generateThreshVsPerfData(obj = list(lda = pred1, ksvm = pred2), measures = list(ppv, tpr, tnr))

plotROCCurves(obj = df, measures = list(tpr, ppv))

plotROCCurves(obj = df, measures = list(tnr, tpr))


# benchmark experiment.

# Tune wrapper for ksvm
rdesc.inner <- makeResampleDesc(method = 'Holdout')
ms <- list(auc, mmce)

ps <- makeParamSet(
  makeDiscreteParam(id = 'C', values = 2^(-1:1))
)
ctrl <- makeTuneControlGrid()
lrn2 <- makeTuneWrapper(learner = lrn2, resampling = rdesc.inner, measures = ms, par.set = ps, control = ctrl)

lrns <- list(lrn1, lrn2)
rdesc.outer <- makeResampleDesc(method = 'CV', iters = 5)
bmr <- benchmark(learners = lrns, tasks = sonar.task, resamplings = rdesc.outer, measures = ms, models = TRUE)

bmr

df <- generateThreshVsPerfData(obj = bmr, measures = list(fpr, tpr, mmce), aggregate = F)

plotROCCurves(obj = df)



preds <- getBMRPredictions(bmr = bmr, drop = TRUE)
preds2 <- lapply(preds, function(x) {class(x) = 'Prediction'; return(x)})
preds2


