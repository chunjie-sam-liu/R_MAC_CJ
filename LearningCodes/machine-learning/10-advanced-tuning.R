

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












