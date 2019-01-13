
# library -----------------------------------------------------------------

library(MLSeq)
library(DESeq2)

# prepare data ------------------------------------------------------------

filepath <- system.file("extdata/cervical.txt", package = "MLSeq")

cervical <- read.table(file = filepath, header = T)

class <- DataFrame(condition = factor(rep(c("N", "T"), c(29, 29))))

# split data --------------------------------------------------------------

set.seed(123)

vars <- sort(apply(cervical, 1, var, na.rm = TRUE), decreasing = TRUE)
data <- cervical[names(vars)[1:100], ]
nTest <- ceiling(ncol(data) * 0.3)
ind <- sample(ncol(data), nTest, FALSE)

data.train <- as.matrix(data[, -ind] + 1)
data.test <- as.matrix(data[ , ind] + 1)

classtr <- DataFrame(condition = class[-ind, ])
classts <- DataFrame(condition = class[ind, ])


data.trainS4 <- DESeqDataSetFromMatrix(countData = data.train, colData = classtr, design = formula(~condition))
data.testS4 <- DESeqDataSetFromMatrix(countData = data.test, colData = classts, design = formula(~condition))

# svmRadial
fit <- classify(data = data.trainS4, method = 'svmRadial', preProcessing = 'deseq-rlog', ref = 'T', control = trainControl(method = 'repeatedcv', number = 2, repeats = 2, classProbs = TRUE))
show(fit)

# voomNSC
fit <- classify(data = data.trainS4, method = 'voomNSC', normalize = 'deseq', ref = 'T', control = voomControl(tuneLength = 20))
show(fit)
trained(fit)

# optimizing model parameters

set.seed(123)

fit.svm <- classify(data = data.trainS4, method = 'svmRadial', preProcessing = 'deseq-vst', ref = 'T', tuneLength = 10, control = trainControl(method = 'repeatedcv', number = 5, repeats = 10, classProbs = T))
show(fit.svm)

trained(fit.svm)
plot(fit.svm)

# caret::trainControl parameter tuning.
# Optimizing model parameters.
# evaluates k-fold repeated cv on trainning set.
# number = 5 5-fold cross validation
# repeats = 10 repeated 10 times.
# tuneLength = 10, is the length of tuning parameter space.

show(fit.svm)
trained(fit.svm)

plot(fit.svm)

# discreateControl -> PLDA, PLDA2, BNLDA
# voomControl -> voomDLDA, vommDQDA, voomNSC
# trainControl -> All others.

ctrl.svm <- trainControl(method = 'repeatedcv', number = 5, repeats = 10)
ctrl.plda <- discreteControl(method = 'repeatedcv', number = 5, repeats = 10, tuneLength = 10)
ctrl.voomDLDA <- voomControl(method = 'repeatedcv', number = 5, repeats = 10, tuneLength = 10)


fit.svm <- classify(data = data.trainS4, method = 'svmRadial', preProcessing = 'deseq-vst', ref = 'T', tuneLength = 10, control = ctrl.svm)
trained(fit.svm)

fit.plda <- classify(data = data.trainS4, method = 'PLDA', normalize = 'deseq', ref = 'T', control = ctrl.plda)
trained(fit.plda)

fit.voomDLDA <- classify(data = data.trainS4, method = 'voomDLDA', ref = 'T', control = ctrl.voomDLDA)
trained(fit.voomDLDA)

show(fit.svm)
show(fit.plda)
show(fit.voomDLDA)
trained(fit.svm)
trained(fit.plda)
trained(fit.voomDLDA)


# predicting --------------------------------------------------------------

pred.svm <- predict(fit.svm, data.testS4)
pred.svm
pred.svm <- relevel(pred.svm, ref = 'T')
actual <- relevel(classts$condition, ref = 'T')

tbl <- table(Predicted = pred.svm, Actual = actual)

confusionMatrix(tbl, positive = 'T')


set.seed(123)
ctrl.continous <- trainControl(method = 'repeatedcv', number = 5, repeats = 10)
ctrl.discrete <- discreteControl(method = 'repeatedcv', number = 5, repeats = 10, tuneLength = 10)
ctrl.voom <- voomControl(method = 'repeatedcv', number = 5, repeats = 10, tuneLength = 10)

# 1. Continous classifiers, SVM, NSC
fit.svm <- classify(data = data.trainS4, method = 'svmRadial', preProcessing = 'deseq-vst', ref = 'T', tuneLength = 10, control = ctrl.continous)

fit.NSC <- classify(data = data.trainS4, method = 'pam', preProcessing = 'deseq-vst', ref = 'T', tuneLength = 10, control = ctrl.continous)

# 2. Discrete classifiers

fit.plda <- classify(data = data.trainS4, method = 'PLDA', normalize = 'deseq', ref = 'T', control = ctrl.discrete)

fit.plda2 <- classify(data = data.trainS4, method = 'PLDA2', normalize = 'deseq', ref = 'T', control = ctrl.discrete)

fit.nblda <- classify(data = data.trainS4, method = 'NBLDA', normalize = 'deseq', ref = 'T', control = ctrl.discrete)

# 3. voom-based classifiers
fit.voomDLDA <- classify(data = data.trainS4, method = 'voomDLDA', normalize = 'deseq', ref = 'T', control = ctrl.voom)

fit.voomNSC <- classify(data = data.trainS4, method = 'voomNSC', normalize = 'deseq', ref = 'T', control = ctrl.voom)


# 4. Predictions

pred.svm <- predict(fit.svm, data.testS4)
pred.NSC <- predict(fit.NSC, data.testS4)
pred.plda <- predict(fit.plda, data.testS4)
pred.plda2 <- predict(fit.plda2, data.testS4)
pred.nblda <- predict(fit.nblda, data.testS4)
pred.voomdlda <- predict(fit.voomDLDA, data.testS4)
pred.voomNSC <- predict(fit.voomNSC, data.testS4)



selectedGenes(fit.voomNSC)
selectedGenes(fit.svm)

ctrl <- discreteControl(method = 'repeatedcv', number = 5, repeats = 2, tuneLength = 10)
fit <- classify(data = data.trainS4, method = 'PLDA', normalize = 'deseq', ref = 'T', control = ctrl)

show(fit)
trained(fit)
method(fit) <- 'PLDA2'
show(fit)
metaData(fit)

