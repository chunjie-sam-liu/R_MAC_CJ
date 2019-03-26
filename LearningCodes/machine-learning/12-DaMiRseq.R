library(DaMiRseq)
library(DESeq2)
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
assay(data_norm)
assay(SE)


data_filt <- DaMiR.sampleFilt(data = data_norm, th.corr = 0.9)

sv <- DaMiR.SV(data = data_filt, method = 'fve')


DaMiR.corrplot(sv = sv, colData(data.fit), sig.level = 0.01)

data_adjust <- DaMiR.SVadjust(data = data_filt, sv = sv, n.sv = 4)

DaMiR.Allplot(data = data_filt, colData(data_filt))
DaMiR.Allplot(data = data_adjust, colData(data_adjust))

data_clean <- DaMiR.transpose(assay(data_adjust))
df <- colData(data_adjust)
data_reduced <- DaMiR.FSelect(data_clean, df, th.corr=0.4)
data_reduced <- DaMiR.FReduct(data_reduced$data)
df.importance <- DaMiR.FSort(data_reduced, df)
selected_features <- DaMiR.FBest(data_reduced, ranking=df.importance, autoselect = 'yes')
