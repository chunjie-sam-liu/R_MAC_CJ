

library(mlr)
ps <- makeParamSet(
  makeNumericParam(id = 'C', lower = -12, upper = 12, trafo = function(x) 2^x),
  makeDiscreteParam(id = 'kernel', values = c('vanilladot', 'polydot', 'rdfdot')),
  makeNumericParam(id = 'sigma', lower = -12, upper = 12, trafo = function(x) 2^x, requires = quote(kernel == 'rbfdot')),
  makeIntegerParam(id = 'degree', lower = 2L, upper = 5L, requires = quote(kernel == 'polydot'))
)

ctrl <- makeTuneControlIrace(maxExperiments = 200L)
rdesc <- makeResampleDesc(method = 'Holdout')
res <- tuneParams(learner = 'classif.ksvm', task = iris.task, resampling = rdesc, par.set = ps, control = ctrl, show.info = FALSE)
df <- as.data.frame(res$opt.path)
head(df)

base.learners <- list(
  makeLearner(cl = 'classif.ksvm'),
  makeLearner(cl = 'classif.randomForest')
)
lrn <- makeModelMultiplexer(base.learners = base.learners)

ps <- makeModelMultiplexerParamSet(
  multiplexer = lrn,
  makeNumericParam(id = 'sigma', lower = -12, upper = 12, trafo = function(x) 2^x),
  makeIntegerParam(id = 'ntree', lower = 1, upper = 500)
)
print(ps)
rdesc <- makeResampleDesc(method = 'CV', iters = 2L)
ctrl <- makeTuneControlIrace(maxExperiments = 200L)
res <- tuneParams(learner = lrn, task = iris.task, resampling = rdesc, par.set = ps, control = ctrl, show.info = FALSE)


ps <- makeParamSet(
  makeNumericParam(id = 'C', lower = -12, upper = 12, trafo = function(x) 2^x),
  makeNumericParam(id = 'sigma', lower = -12, upper = 12, trafo = function(x) 2^x)
)
ctrl <- makeTuneMultiCritControlRandom(maxit = 30L)
rdesc <- makeResampleDesc('Holdout')
lrn <- makeLearner(cl = 'classif.ksvm', predict.type = 'prob')
res <- tuneParamsMultiCrit(learner = lrn, task = sonar.task, resampling = rdesc, measures = list(fpr, fnr), par.set = ps, control = ctrl, show.info = FALSE)


plotTuneMultiCritResult(res)

# Feature Selection -------------------------------------------------------


# 1. Filtering
# An external algorithm computes a rank of the variables.
# Features are subsetted by a certain criteria. he selected features will theb be used to fit a model with optional hyperparameters selected by tuning.
# 2. feature subset selection
# No ranking of features is done. Features are selected by a random subset of the data.
# Then, a model is fit and the performance is chekced. This is done for a lot of feature combinations in a CV setting and the best combination is reported.
# This method is very computational intense as a lot of models are fitted. Also, strictly all these models would need to be tuned before the performance is estimated which would require an additional nested level in a CV setting.

library(mlr)
fv = generateFilterValuesData(iris.task, method = "FSelectorRcpp_information.gain")
fv
library(FSelector)
fv2 = generateFilterValuesData(
  iris.task, 
  method = c("FSelectorRcpp_information.gain", "FSelector_chi.squared")
  )

plotFilterValues(fv2) + ggpubr::theme_pubr()


# Keep the 2 most important features
filtered.task = filterFeatures(iris.task, method = "information.gain", abs = 2)
# Keep the 25% most important features
filtered.task = filterFeatures(iris.task, fval = fv, perc = 0.75)
# Keep all features with importance greater than 0.5
filtered.task = filterFeatures(iris.task, fval = fv, threshold = 0.99)
filtered.task


# 1. keep a certain absolute (abs) number of features with highest importance.
# 2. Keep a certain percentage (perc) of features with highest importance
# 3. Keep all features whose importance execeed a certain threshold value (threshold)

# feature selection based on a filter method.
# automate the selection of the features
# Learner can be fused with a filter method by function makeFilterWrapper()
# tuning the dataset



lrn <- makeFilterWrapper(learner = 'classif.fnn', fw.method = 'information.gain', fw.abs = 2)
rdesc <- makeResampleDesc(method = 'CV', iters = 10)
r <- resample(learner = lrn, task = iris.task, resampling = rdesc, models = TRUE)
r$models


sfeats <-  sapply(r$models, getFilteredFeatures)
table(sfeats)



# Tuning the size of the feature subset
# tune the number of features
# 1. The percentage of featuers selected (fw.perc)
# 2. The absolute number of features selected (fw.abs)
# 3. The threshold of the filter method (fw.threshold)

lrn <- makeFilterWrapper(learner = 'regr.ksvm', fw.method = 'information.gain')
ps <- makeParamSet(
  makeNumericParam('fw.perc', lower = 0, upper = 1),
  makeNumericParam(id = 'C', lower = -10, upper = 10, trafo = function(x) 10^x),
  makeNumericParam(id = 'sigma', lower = -10, upper = 10, trafo = function(x) 10^x)
)
rdesc <- makeResampleDesc(method = 'CV', iters = 3)
ctrl <- makeTuneControlRandom(maxit = 20)
res <- tuneParams(learner = lrn, task = bh.task, resampling = rdesc, measures = mse, par.set = ps, control = ctrl)


df <- as.data.frame(res$opt.path)
df

lrn <- makeFilterWrapper(learner = 'regr.lm', fw.method = 'information.gain', fw.perc = res$x$fw.perc, C = res$x$C, sigma = res$x$sigma)

mod <- train(lrn, bh.task)
getFilteredFeatures(mod)



# select a feature subset.

# search strategy
ctrl <- makeFeatSelControlRandom(maxit = 20L)
ctrl

rdsc <- makeResampleDesc(method = 'Holdout')
# select features
sfeats <- selectFeatures(learner = 'surv.coxph', task = wpbc.task, resampling = rdesc, control = ctrl)

ctrl <- makeFeatSelControlSequential(method = 'sfs', alpha = 0.02)
rdesc <- makeResampleDesc(method = 'CV', iters = 10)

sfeats <- selectFeatures(learner = 'regr.lm', task = bh.task, resampling = rdesc, measures = mse, control = ctrl)
sfeats$x
analyzeFeatSelResult(res = sfeats)

rdesc <- makeResampleDesc(method = 'CV', iters = 3)
ctrl <- makeFeatSelControlRandom(maxit = 10)
lrn <- makeFeatSelWrapper(learner = 'surv.coxph', resampling = rdesc, control = ctrl)

mod <- train(learner = lrn, task = wpbc.task)


sfeats <- getFeatSelResult(mod)
sfeats$x


task = makeClassifTask(data = iris, target = "Species")
lrn = makeLearner("classif.ranger", importance = c("permutation"))
mod = train(lrn, task)
getFeatureImportance(mod)



