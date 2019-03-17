

# 1. define the learning task
# 2. tuning model
# 3. conduct a benchmark experiment
# 4. evalutate the performance of the model
library(mlr)
data(BostonHousing, package = 'mlbench')
summary(BostonHousing)

regr.task <- makeRegrTask(data = BostonHousing, target = 'medv')

regr.task

listLearners('classif')

# With so many learners it is difficult to choose which one would be optimal for this specific task.
# classical linear regression model (regr.lm)
# SVM with radial basis kernel (regr.ksvm)
# random forest from the ranger (ranger::ranger()) (regr.ranger)


# get a quick overview of all learner-specific tunable parameters
# getLearnerParamSet()

# Before setting up a benchmark experiment we can specify which hyperparameters are going to be tuned.


set.seed(12345)

# define a searc space for each learner's parameter
ps_ksvm <- makeParamSet(
  makeNumericParam(id = 'sigma', lower = -12, upper = 12, trafo = function(x) 2^x)
)
ps_rf <- makeParamSet(
  makeIntegerParam(id = 'num.trees', lower = 1L, upper = 100L)
)

# Choose a resampling strategy
rdesc <- makeResampleDesc(method = 'CV', iters = 5L)

# Choose a performance measure
meas <- rmse

# Choose a tuning method

ctrl <- makeTuneControlCMAES(budget = 100L)

# Make tuning wrappers

tuned.ksvm <- makeTuneWrapper(
  learner = 'regr.ksvm',
  resampling = rdesc,
  measures = meas,
  par.set = ps_ksvm,
  control = ctrl, 
  show.info = FALSE
)
tuned.rf <- makeTuneWrapper(
  learner = 'regr.ranger',
  resampling = rdesc,
  measures = meas,
  par.set = ps_rf,
  control = ctrl,
  show.info = FALSE
)

# Three learners to be compared
lrns <- list(makeLearner('regr.lm'), tuned.ksvm, tuned.rf)

# Conduct the benchmark experiment
bmr <- benchmark(learners = lrns, tasks = regr.task, resamplings = rdesc, measures = meas, show.info = FALSE)
plotBMRBoxplots(bmr = bmr)


data(iris)
task <- makeClassifTask(
  data = iris,
  target = 'Species',
  weights = as.integer(iris$Species)
)
base.lrn <- makeLearner('classif.rpart')
wrapped.lrn <- makeBaggingWrapper(learner = base.lrn, bw.iters = 100, bw.feats = 0.5)

print(wrapped.lrn)
benchmark(tasks = task, learners = list(base.lrn, wrapped.lrn))
getParamSet(x = wrapped.lrn)


ctrl <- makeTuneControlRandom(maxit = 10)
rdesc <- makeResampleDesc(method = 'CV', iters = 3)
par.set <- makeParamSet(
  makeIntegerParam(id = 'minsplit', lower = 1, upper = 10),
  makeNumericParam(id = 'bw.feats', lower = 0.25, upper = 1)
)

tuned.lrn <- makeTuneWrapper(learner = wrapped.lrn, resampling = rdesc, measures = mmce, par.set = par.set, ctrl)
print(tuned.lrn)


lrn <- train(learner = tuned.lrn, task = task)
