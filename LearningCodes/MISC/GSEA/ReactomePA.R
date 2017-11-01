library(ReactomePA)
data(geneList)
de <- names(geneList)[abs(geneList) > 1.5]
head(de)
x <- enrichPathway(gene=de,pvalueCutoff=0.05, readable=T)
head(as.data.frame(x))
barplot(x, showCategory=8)
dotplot(x, showCategory=15)
enrichMap(x, layout=igraph::layout.kamada.kawai, vertex.label.cex = 1)
cnetplot(x, categorySize="pvalue", foldChange=geneList)
require(clusterProfiler)
data(gcSample)
res <- compareCluster(gcSample, fun="enrichPathway")
plot(res)

y <- gsePathway(geneList, nPerm=1000,
                minGSSize=120, pvalueCutoff=0.2,
                pAdjustMethod="BH", verbose=FALSE)
res <- as.data.frame(y)
head(res)
enrichMap(y)
gseaplot(y, geneSetID = "R-HSA-69242")
