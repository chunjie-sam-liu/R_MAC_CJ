# In a benchmark experiment different learning methods are applied to one or several data sets.
# Compare and rank the algorithms with respect to one or more perfromance measures.

# benchmark()
# benchmark basically executes resample() for each combination of makeLearner() and Task().
# MASS::lda(); rpart::rpart()
library(mlr)

lrns <- list(makeLearner('classif.lda'), makeLearner('classif.rpart'))

# choose the resampling strategy
rdesc <- makeResampleDesc(method = 'Holdout')

# conduct the benchmark experiment

bmr <- benchmark(learners = lrns, tasks = sonar.task, resamplings = rdesc)

lrns <- list(makeLearner(cl = 'classif.lda', predict.type = 'prob'), 'classif.rpart')
bmr <- benchmark(learners = lrns, tasks = sonar.task, resamplings = rdesc)


getBMRPerformances(bmr)
getBMRAggrPerformances(bmr = bmr)

getBMRPerformances(bmr = bmr, drop = TRUE)
getBMRPredictions(bmr = bmr, as.df = TRUE)
getBMRPredictions(bmr = bmr)
head(getBMRPredictions(bmr = bmr, learner.ids = 'classif.rpart', as.df = TRUE))
getBMRTaskIds(bmr = bmr)
getBMRLearnerIds(bmr = bmr)
getBMRMeasureIds(bmr = bmr)

class(bmr)
getBMRModels(bmr = bmr, drop = TRUE)
getBMRModels(bmr = bmr, learner.ids = 'classif.lda')
getBMRLearnerIds(bmr = bmr)
getBMRLearners(bmr = bmr)
getBMRMeasures(bmr = bmr)

lrns2 <- list(makeLearner(cl = 'classif.randomForest'), makeLearner(cl = 'classif.qda'))
bmr2 <- benchmark(learners = lrns2, tasks = sonar.task, resamplings = rdesc, show.info = FALSE)

mergeBenchmarkResults(bmrs = list(bmr, bmr2))

rin <- getBMRPredictions(bmr)[[1]][[1]]$instance
rin

bmr3 <- benchmark(learners = lrns2, tasks = sonar.task, resamplings = rin, show.info = FALSE)
bmr3
mergeBenchmarkResults(bmrs = list(bmr, bmr3))


# 1. visualization
# 2. ranking of learning algorithms and hypothesis tests


# lda, rpart and random Forest

library(mlbench)

# create a list of leaners
lrns <- list(
  makeLearner(cl = 'classif.lda', id = 'lda'),
  makeLearner(cl = 'classif.rpart', id = 'rpart'),
  makeLearner(cl = 'classif.randomForest', id = 'randomForest')
)

# get additional Tasks from package mlbench

ring.task <- convertMLBenchObjToTask(x = 'mlbench.ringnorm', n = 600)
wave.task <- convertMLBenchObjToTask(x = 'mlbench.waveform', n = 600)

tasks <- list(iris.task, sonar.task, pid.task, ring.task, wave.task)
rdesc <- makeResampleDesc(method = 'CV', iters = 10)
meas <- list(mmce, ber, timetrain)

bmr <- benchmark(learners = lrns, tasks = tasks, resamplings = rdesc, measures = meas)



perf <- getBMRPerformances(bmr = bmr, as.df = TRUE)


# Integrated plots


plotBMRBoxplots(bmr = bmr, measure = mmce, order.lrns = getBMRLearnerIds(bmr))

library(ggplot2)
plotBMRBoxplots(
  bmr = bmr,
  measure = ber,
  style = 'violin',
  pretty.names = TRUE,
  order.lrns = getBMRLearnerIds(bmr = bmr)) +
  aes(color = learner.id) +
  theme(strip.text.x = element_text(size = 8))

plt <- plotBMRBoxplots(
  bmr = bmr,
  measure = mmce,
  order.lrns = getBMRLearnerIds(bmr)
)

levels(plt$data$task.id) <- c('Iric', 'Ringnorm', 'Waveform', 'Diabetes', 'Sonar')
levels(plt$data$learner.id) <- c('LDA', 'CART', 'RF')
plt
plt + ylab(label = 'Error rate')


# Visualizing aggregated performances

plotBMRSummary(bmr = bmr)

m <- convertBMRToRankMatrix(bmr = bmr, measure = mmce)
plotBMRRanksAsBarChart(bmr = bmr, pos = 'tile', order.lrns = getBMRLearnerIds(bmr = bmr))
plotBMRSummary(bmr = bmr, trafo = 'rank', jitter = 0)

plotBMRRanksAsBarChart(bmr = bmr, order.lrns = getBMRLearnerIds(bmr = bmr))
plotBMRRanksAsBarChart(bmr = bmr, pos = 'dodge', order.lrns = getBMRLearnerIds(bmr = bmr))

friedmanTestBMR(bmr = bmr)
friedmanPostHocTestBMR(bmr = bmr, p.value = 0.1)


# Nemenyi test
g <- generateCritDifferencesData(bmr = bmr, p.value = 0.1, test = 'nemenyi')
plotCritDifferences(g) +
  coord_cartesian(
    xlim = c(-1, 5),
    ylim = c(0, 2)
  ) +
  scale_colour_manual(
    values = c('lda' = '#F8766D', 'rpart' = '#00BA38', 'randomForest' = '#619CFF')
  )

# Bonferroni-Dunn test
g = generateCritDifferencesData(bmr, p.value = 0.1, test = "bd", baseline = "randomForest")
plotCritDifferences(g) + coord_cartesian(xlim = c(-1,5), ylim = c(0,2)) +
  scale_colour_manual(values = c("lda" = "#F8766D", "rpart" = "#00BA38", "randomForest" = "#619CFF"))

perf = getBMRPerformances(bmr, as.df = TRUE)
# Density plots for two tasks
qplot(mmce, colour = learner.id, facets = . ~ task.id,
      data = perf[perf$task.id %in% c("iris-example", "Sonar-example"),], geom = "density") +
  theme(strip.text.x = element_text(size = 8))

# Compare mmce and timetrain
df = reshape2::melt(perf, id.vars = c("task.id", "learner.id", "iter"))
df = df[df$variable != "ber",]
head(df)

qplot(variable, value, data = df, colour = learner.id, geom = "boxplot",
      xlab = "measure", ylab = "performance") +
  facet_wrap(~ task.id, nrow = 2)


perf = getBMRPerformances(bmr, task.id = "Sonar-example", as.df = TRUE)
df = reshape2::melt(perf, id.vars = c("task.id", "learner.id", "iter"))
df = df[df$variable == "mmce",]
df = reshape2::dcast(df, task.id + iter ~ variable + learner.id)
head(df)

GGally::ggpairs(df, 3:5)




