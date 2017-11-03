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

geneSets <- DOSE:::getGeneSet(GO_DATA)
DOSE:::check_gene_id(geneList, geneSets)
tmp_res <- fgsea::fgsea(pathways=geneSets,
                 stats=geneList,
                 nperm=nPerm,
                 minSize=minGSSize,
                 maxSize=maxGSSize,
                 gseaParam=exponent,
                 nproc = 0)
p.adj <- p.adjust(tmp_res$pval, method=pAdjustMethod)
qvalues <- DOSE:::calculate_qvalue(tmp_res$pval)
Description <- DOSE:::TERM2NAME(tmp_res$pathway, GO_DATA)

params <- list(pvalueCutoff = pvalueCutoff,
               nPerm = nPerm,
               pAdjustMethod = pAdjustMethod,
               exponent = exponent,
               minGSSize = minGSSize,
               maxGSSize = maxGSSize
)

res <- data.frame(
  ID = as.character(tmp_res$pathway),
  Description = Description,
  setSize = tmp_res$size,
  enrichmentScore = tmp_res$ES,
  NES = tmp_res$NES,
  pvalue = tmp_res$pval,
  p.adjust = p.adj,
  qvalues = qvalues,
  stringsAsFactors = FALSE
)
res <- res[!is.na(res$pvalue),]
res <- res[ res$pvalue <= pvalueCutoff, ]
res <- res[ res$p.adjust <= pvalueCutoff, ]
idx <- order(res$pvalue, decreasing = FALSE)
res <- res[idx, ]

if (nrow(res) == 0) {
  message("no term enriched under specific pvalueCutoff...")
  return(
    new("gseaResult",
        result     = res,
        geneSets   = geneSets,
        geneList   = geneList,
        params     = params,
        readable   = FALSE
    )
  )
}
row.names(res) <- res$ID

observed_info <- lapply(geneSets[res$ID], function(gs)
  DOSE:::gseaScores(geneSet=gs,
             geneList=geneList,
             exponent=exponent)
)
ledge <- DOSE:::leading_edge(observed_info)


# Fgsea -------------------------------------------------------------------

tmp_res <- fgsea(pathways=geneSets,
                 stats=geneList,
                 nperm=nPerm,
                 minSize=minGSSize,
                 maxSize=maxGSSize,
                 gseaParam=exponent,
                 nproc = 0)

pathways = geneSets
stats = geneList 
nperm = 1000
minSize=100 
maxSize=500
nproc=0
gseaParam=1
BPPARAM=NULL

if (is.null(BPPARAM)) {
  if (nproc != 0) {
    if (.Platform$OS.type == "windows") {
      # windows doesn't support multicore, using snow instead
      BPPARAM <- SnowParam(workers = nproc)
    } else {
      BPPARAM <- MulticoreParam(workers = nproc)
    }
  } else {
    BPPARAM <- BiocParallel:::bpparam()
  }
}
minSize <- max(minSize, 1)
stats <- sort(stats, decreasing=TRUE)
stats <- abs(stats) ^ gseaParam
pathwaysFiltered <-
  lapply(pathways, function(p) {
    as.vector(na.omit(fastmatch:::fmatch(p, names(stats))))
  })
# pathwaysFiltered is list of the index of stats

pathwaysSizes <- sapply(pathwaysFiltered, length)

toKeep <- which(minSize <= pathwaysSizes & pathwaysSizes <= maxSize)
m <- length(toKeep)

pathwaysFiltered <- pathwaysFiltered[toKeep]
npermActual <- nperm
gseaStatRes <- do.call(rbind,
                       lapply(
                         pathwaysFiltered,
                         calcGseaStat,
                         stats = stats,
                         returnLeadingEdge = TRUE
                       ))

leadingEdges <- mapply("[", list(names(stats)), gseaStatRes[, "leadingEdge"], SIMPLIFY = FALSE)
pathwayScores <- unlist(gseaStatRes[, "res"])

granularity <- 1000
permPerProc <- rep(granularity, floor(npermActual / granularity))
if (npermActual - sum(permPerProc) > 0) {
  permPerProc <- c(permPerProc, npermActual - sum(permPerProc))
}

universe <- seq_along(stats)
seeds <- sample.int(10^9, length(permPerProc))
# calculate p-value
counts <- BiocParallel::bplapply(seq_along(permPerProc), function(i) {
  nperm1 <- permPerProc[i]
  leEs <- rep(0, m)
  geEs <- rep(0, m)
  leZero <- rep(0, m)
  geZero <- rep(0, m)
  leZeroSum <- rep(0, m)
  geZeroSum <- rep(0, m)
  if (m == 1) {
    for (i in seq_len(nperm1)) {
      randSample <- sample.int(length(universe), K)
      randEsP <- calcGseaStat(
        stats = stats,
        selectedStats = randSample,
        gseaParam = 1)
      leEs <- leEs + (randEsP <= pathwayScores)
      geEs <- geEs + (randEsP >= pathwayScores)
      leZero <- leZero + (randEsP <= 0)
      geZero <- geZero + (randEsP >= 0)
      leZeroSum <- leZeroSum + pmin(randEsP, 0)
      geZeroSum <- geZeroSum + pmax(randEsP, 0)
    }
  } else {
    aux <- fgsea:::calcGseaStatCumulativeBatch(
      stats = stats,
      gseaParam = 1,
      pathwayScores = pathwayScores,
      pathwaysSizes = pathwaysSizes,
      iterations = nperm1,
      seed = seeds[i])
    leEs = get("leEs", aux)
    geEs = get("geEs", aux)
    leZero = get("leZero", aux)
    geZero = get("geZero", aux)
    leZeroSum = get("leZeroSum", aux)
    geZeroSum = get("geZeroSum", aux)
  }
  # Time consumption
  data.table(
    pathway = seq_len(m),
    leEs = leEs,
    geEs = geEs,
    leZero = leZero,
    geZero = geZero,
    leZeroSum = leZeroSum,
    geZeroSum = geZeroSum
  )
}, BPPARAM = BPPARAM)

counts <- rbindlist(counts)

# fgsea algorithm
# for calcGseaStat-----
stats <- stats
# Ordered whole differential gene list
selectedStats <- pathwaysFiltered[[1]]
gseaParam = 1
returnAllExtreme <- F
returnLeadingEdge <- T

S <- selectedStats # gene set index of stats
r <- stats # the differential genes
p <- gseaParam # the power

S <- sort(S)
m <- length(S)
N <- length(r)

if (m == N) {
  stop("GSEA statistic is not defined when all genes are selected")
  # this is fucking important
  # you can't use whole gene list as gene set
  # fucking stupid
}
r[S]
NR <- (sum(abs(r[S]) ^ p))
rAdj <- abs(r[S]) ^p # rAdj only contains the gene in set S
if (NR == 0) {
  # this is equivalent to rAdj being rep(eps, m)
  rCumSum <- seq_along(rAdj) / length(rAdj)
} else {
  rCumSum <- cumsum(rAdj) / NR # divided by NR to normalized to 1
}
rCumSum # is the running score.

tops <- rCumSum - (S - seq_along(S)) / (N - m)
# It's strange to use S minux the index of the S
# what is the (S - seq_along(S)) / (N - m)
tops

if (NR == 0) {
  # this is equivalent to rAdj being rep(eps, m)
  bottoms <- tops - 1 / m
} else {
  bottoms <- tops - rAdj / NR
}
maxP <- max(tops)
minP <- min(bottoms)
if(maxP > -minP) {
  geneSetStatistic <- maxP
} else if (maxP < -minP) {
  geneSetStatistic <- minP
} else {
  geneSetStatistic <- 0
}
res <- list(res=geneSetStatistic)
res <- c(res, list(tops=tops, bottoms=bottoms))
if (returnLeadingEdge) {
  leadingEdge <- if (maxP > -minP) {
    S[seq_along(S) <= which.max(bottoms)]
  } else if (maxP < -minP) {
    S[seq_along(S) >= which.min(bottoms)]
  } else {
    NULL
  }
  
  res <- c(res, list(leadingEdge=leadingEdge))
}

# Enrichment score algorithm  in DOSE ---------------------------------------------
geneSets$`GO:0000139` -> gs
N <- length(geneList)
Nh <- length(gs)

Phit <- Pmiss <- numeric(N)
hits <- names(geneList) %in% gs

Phit[hits] <- abs(geneList[hits])^exponent
NR <- sum(Phit)

Phit <- cumsum(Phit/NR)
Pmiss[!hits] <- 1 / (N-Nh)
Pmiss <- cumsum(Pmiss)
# This is running enrichment score
# NR is the sum difference of set
runningES <- Phit - Pmiss
## ES is the maximum deviation from zero of Phit-Pmiss
max.ES <- max(runningES)
min.ES <- min(runningES)
if( abs(max.ES) > abs(min.ES) ) {
  ES <- max.ES
} else {
  ES <- min.ES
}

df <- data.frame(x=seq_along(runningES),
                 runningScore=runningES,
                 position=as.integer(hits)
)

df

df$gene = names(geneList)
res <- list(ES=ES, runningES = df)

# END -----------------------------------------------------------------------




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








