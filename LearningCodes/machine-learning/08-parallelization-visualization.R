
# parallelMap() into mlr.
# activate the parallel computing capabilities
# local multicore execution using parallel()
# socket and MPI cluster using snow()
# makeshift SSH-clusters using BatchJobs()

# high performance computing clusters (managed by a scheduler like SLURM, Torque/PBS, SGE or LSF) also using BatchJobs()


# parallelStart* from parallelMap::parallelStart() function
# parallelStop from parallelMap::parallelStop() at the end of your script.

library(parallelMap)
library(mlr)

# Starting parallelization in mode = socket with cpus = 2
parallelStartSocket(2)
rdesc <- makeResampleDesc(method = 'CV', iters = 3)
r <- resample(learner = 'classif.lda', task = iris.task, resampling = rdesc)
parallelStop()
parallelGetRegisteredLevels()



# Generation and plotting functions

# mlr's visualization capabilities on generation functions which generate data for plots.

lrn <- makeLearner(cl = 'classif.lda', predict.type = 'prob')
n <- getTaskSize(sonar.task)
mod <- train(learner = lrn, task = sonar.task, subset = seq(1, n, by = 2))
pred <- predict(object = mod, task = sonar.task, subset = seq(2, n, by = 2))
d <- generateThreshVsPerfData(obj = pred, measures = list(fpr, fnr, mmce, auc))
class(d)

head(d$data)
plotThreshVsPerf(obj = d)






