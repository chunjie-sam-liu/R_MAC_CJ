

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
filtered.task = filterFeatures(iris.task, fval = fv, perc = 0.25)
# Keep all features with importance greater than 0.5
filtered.task = filterFeatures(iris.task, fval = fv, threshold = 0.5)
filtered.task









