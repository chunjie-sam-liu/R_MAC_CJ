
# library -----------------------------------------------------------------

library(mRMRe)
set.thread.count(2)
data(cgps)

data.annot <- data.frame(cgps.annot)
data.cgps <- data.frame(cgps.ic50, cgps.ge)
library(survival)


df <- data.frame(
  'surv1' = Surv(runif(100), sample(0:1, 100, replace = T)),
  'cont1' = runif(100),
  'disc1' = factor(sample(1:5, 100, replace = T), ordered = T),
  'surv2' = Surv(runif(100), sample(0:1, 100, replace = T)),
  'cont2' = runif(100),
  'cont3' = runif(100),
  'surv3' = Surv(runif(100), sample(0:1, 100, replace = T)),
  'disc3' = factor(sample(1:5, 100, replace = T), ordered = T)
)

dd <- mRMR.data(data = df)

dd <- mRMR.data(data = data.cgps)

dd <- subsetData(dd, 1:10, 1:10)
spearman_mim <- mim(dd, continuous_estimator = "spearman")
pearson_mim <- mim(dd, continuous_estimator = "pearson")
correlate(cgps.ge[, 1], cgps.ge[, 2], method = "cindex")
x <- sample(factor(c("CAT_1", "CAT_2", "CAT_3"), ordered = TRUE), 100, replace = TRUE)
y <- sample(factor(c("CAT_1", "CAT_2"), ordered = TRUE), 100, replace = TRUE)
correlate(x, y, method = "cramersv")

strata <- sample(factor(c('STRATUM_1', 'STRATUM_2', 'STRATUM_3'), ordered = TRUE), nrow(cgps.ge), replace = T)
weights <- runif(nrow(cgps.ge))
correlate(cgps.ge[,1], cgps.ge[, 2], strata = strata, weights = weights, method = 'pearson')

dd <- mRMR.data(data = data.cgps)
mRMR.classic(data = dd, target_indices = c(1), feature_count = 30)
ens <- mRMR.ensemble(data = dd, target_indices = c(1), solution_count = 1, feature_count = 30)


causality(ens)
