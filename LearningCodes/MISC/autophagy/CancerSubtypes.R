library(CancerSubtypes)
library(RTCGA.mRNA)
library(magrittr)
data("BRCA.mRNA")
mRNA <- t(as.matrix(BRCA.mRNA[,-1]))
colnames(mRNA) <- BRCA.mRNA[,1]

data.checkDistribution(mRNA)
index=which(is.na(mRNA))


res1=data.imputation(mRNA,fun="median")
res2=data.imputation(mRNA,fun="mean")
res3=data.imputation(mRNA,fun="microarray")

result1=data.normalization(mRNA,type="feature_Median",log2=FALSE)
result2=data.normalization(mRNA,type="feature_zscore",log2=FALSE)

data1=FSbyVar(mRNA, cut.type="topk",value=1000)
data2=FSbyVar(mRNA, cut.type="cutoff",value=0.5)

data1=FSbyMAD(mRNA, cut.type="topk",value=1000)
data2=FSbyMAD(mRNA, cut.type="cutoff",value=0.5)

mRNA1=data.imputation(mRNA,fun="microarray")
data1=FSbyPCA(mRNA1, PC_percent=0.9,scale = TRUE)

data(GeneExp)
data(time)
data(status)
data1=FSbyCox(GeneExp,time,status,cutoff=0.05)


data(GeneExp)
result=ExecuteCC(clusterNum=3,d=GeneExp,maxK=10,clusterAlg="hc",distance="pearson",title="GBM")

data(GeneExp)
data(miRNAExp)
GBM=list(GeneExp=GeneExp,miRNAExp=miRNAExp)
result=ExecuteCC(clusterNum=3,d=GBM,maxK=10,clusterAlg="hc",distance="pearson",title="GBM")

result=ExecuteCNMF(GBM,clusterNum=3,nrun=30)

data1=FSbyVar(GeneExp, cut.type="topk",value=1000)
data2=FSbyVar(miRNAExp, cut.type="topk",value=300)
GBM=list(GeneExp=data1,miRNAExp=data2)
result=ExecuteiCluster(datasets=GBM, k=3, lambda=list(0.44,0.33,0.28))

result=ExecuteSNF(GBM, clusterNum=3, K=20, alpha=0.5, t=20)

result=ExecuteSNF.CC(GBM, clusterNum=3, K=20, alpha=0.5, t=20,maxK = 10, pItem = 0.8,reps=500, 
                     title = "GBM", plot = "png", finalLinkage ="average")

result=ExecuteSNF(GBM, clusterNum=3, K=20, alpha=0.5, t=20,plot = FALSE)


sil=silhouette_SimilarityMatrix(result$group, result$distanceMatrix)
sil
plot(sil)

sil1=silhouette(result$group, result$distanceMatrix)
plot(sil1)

data1=FSbyCox(GeneExp,time,status,cutoff=0.05)
data2=FSbyCox(miRNAExp,time,status,cutoff=0.05)
GBM=list(GeneExp=data1,miRNAExp=data2)
result1=ExecuteSNF(GBM, clusterNum=3, K=20, alpha=0.5, t=20,plot = FALSE)
group1=result1$group
distanceMatrix1=result1$distanceMatrix
p_value=survAnalysis(mainTitle="GBM1",time,status,group1,
                     distanceMatrix1,similarity=TRUE)


result2=ExecuteSNF.CC(GBM, clusterNum=3, K=20, alpha=0.5, t=20,
                      maxK = 5, pItem = 0.8,reps=500, 
                      title = "GBM2", plot = "png", 
                      finalLinkage ="average")
group2=result2$group
distanceMatrix2=result2$distanceMatrix
p_value=survAnalysis(mainTitle="GBM2",time,status,group2,
                     distanceMatrix2,similarity=TRUE)




GBM=list(GeneExp=GeneExp,miRNAExp=miRNAExp)
result=ExecuteSNF(GBM, clusterNum=3, K=20, alpha=0.5, t=20,plot = FALSE)
group=result$group
sigclust=sigclustTest(miRNAExp,group, nsim=1000, nrep=1, icovest=1)






