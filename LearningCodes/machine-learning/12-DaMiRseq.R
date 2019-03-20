library(DaMiRseq)

# 1. normalization
# 2. feature selection
# 3. classification

# 1. limits overfitting
# 2. improves classification performance of predicitors.
# 3. reduces time training processing

data(SE)
assay(SE)
colData(SE)

data_norm <- DaMiR.normalization(data = SE, minCounts = 10, fSample = 0.7, hyper = 'yes', th.cv = 3)
