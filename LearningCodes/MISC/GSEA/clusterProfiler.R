library(clusterProfiler)

x <- c("GPX3",  "GLRX",   "LBP",   "CRYAB", "DEFB1", "HCLS1",   "SOD2",   "HSPA2",
       "ORM1",  "IGFBP1", "PTHLH", "GPC3",  "IGFBP3","TOB1",    "MITF",   "NDRG1",
       "NR1H4", "FGFR3",  "PVR",   "IL6",   "PTPRM", "ERBB2",   "NID2",   "LAMB1",
       "COMP",  "PLS3",   "MCAM",  "SPP1",  "LAMC1", "COL4A2",  "COL4A1", "MYOC",
       "ANXA4", "TFPI2",  "CST6",  "SLPI",  "TIMP2", "CPM",     "GGT1",   "NNMT",
       "MAL",   "EEF1A2", "HGD",   "TCN2",  "CDA",   "PCCA",    "CRYM",   "PDXK",
       "STC1",  "WARS",  "HMOX1", "FXYD2", "RBP4",   "SLC6A12", "KDELR3", "ITM2B")
eg = bitr(x, fromType="SYMBOL", toType="ENTREZID", OrgDb="org.Hs.eg.db")
library(org.Hs.eg.db)
keytypes(org.Hs.eg.db)
ids <- bitr(
  x, 
  fromType = "SYMBOL",
  toType = c("UNIPROT", "ENSEMBL"),
  OrgDb = "org.Hs.eg.db"
)
head(ids)
data("gcSample")
hg <- gcSample[[1]]
eg2np <- bitr_kegg(hg, fromType = 'kegg', toType = "ncbi-proteinid", organism = 'hsa')

data("geneList", package = 'DOSE')
gene <- names(geneList)[abs(geneList) > 2]
gene.df <- bitr(
  gene,
  fromType = "ENTREZID",
  toType = c("ENSEMBL", "SYMBOL"),
  OrgDb = org.Hs.eg.db
)

head(gene.df)
ggo <- groupGO(
  gene = gene,
  OrgDb = org.Hs.eg.db,
  ont = "CC",
  level = 3,
  readable = T
)

head(ggo)

ego <- enrichGO(gene          = gene,
                universe      = names(geneList),
                OrgDb         = org.Hs.eg.db,
                ont           = "CC",
                pAdjustMethod = "BH",
                pvalueCutoff  = 0.01,
                qvalueCutoff  = 0.05,
                readable      = TRUE)
head(ego)
ego2 <- enrichGO(gene         = gene.df$ENSEMBL,
                 OrgDb         = org.Hs.eg.db,
                 keytype       = 'ENSEMBL',
                 ont           = "CC",
                 pAdjustMethod = "BH",
                 pvalueCutoff  = 0.01,
                 qvalueCutoff  = 0.05)

ego2 <- setReadable(ego2, OrgDb = org.Hs.eg.db)

ego3 <- gseGO(geneList     = geneList,
              OrgDb        = org.Hs.eg.db,
              ont          = "CC",
              nPerm        = 1000,
              minGSSize    = 100,
              maxGSSize    = 500,
              pvalueCutoff = 0.05,
              verbose      = FALSE)
search_kegg_organism('ece', by='kegg_code')
ecoli <- search_kegg_organism('Escherichia coli', by='scientific_name')
dim(ecoli)
kk <- enrichKEGG(gene         = gene,
                 organism     = 'hsa',
                 pvalueCutoff = 0.05)
head(kk)
k2 <- gseKEGG(geneList     = geneList,
              organism     = 'hsa',
              nPerm        = 1000,
              minGSSize    = 120,
              pvalueCutoff = 0.05,
              verbose      = FALSE)
head(kk2)

mkk <- enrichMKEGG(gene = gene,
                   organism = 'hsa')
mkk2 <- gseMKEGG(geneList = geneList,
                 species = 'hsa')
david <- enrichDAVID(gene = gene,
                     idType = "ENTREZ_GENE_ID",
                     listType = "Gene",
                     annotation = "KEGG_PATHWAY",
                     david.user = "clusterProfiler@hku.hk")

gmtfile <- system.file("extdata", "c5.cc.v5.0.entrez.gmt", package="clusterProfiler")
c5 <- read.gmt(gmtfile)

egmt <- enricher(gene, TERM2GENE=c5)
head(egmt)
egmt2 <- GSEA(geneList, TERM2GENE=c5, verbose=FALSE)
head(egmt2)
barplot(ggo, drop=TRUE, showCategory=12)
barplot(ego, showCategory=8)
dotplot(ego)
enrichMap(ego)
cnetplot(ego, categorySize="pvalue", foldChange=geneList)
plotGOgraph(ego)
gseaplot(kk2, geneSetID = "hsa04145")
browseKEGG(kk, 'hsa04110')
library("pathview")
hsa04110 <- pathview(gene.data  = geneList,
                     pathway.id = "hsa04110",
                     species    = "hsa",
                     limit      = list(gene=max(abs(geneList)), cpd=1))








