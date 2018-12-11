# fgsea

library(fgsea)
library(magrittr)


# load pathways and gene-level statistics ---------------------------------


data("examplePathways")
data('exampleRanks')

examplePathways %>% 
  purrr::map_int(length) %>% 
  tibble::enframe() %>%
  dplyr::arrange(-value) ->
  .d

.d %>% dplyr::filter(dplyr::between(value, 15, 500))

data("exampleRanks")

exampleRanks %>% head()
exampleRanks %>% length()


fgsea::fgsea(
  pathways = examplePathways,
  stats = exampleRanks,
  minSize = 15,
  maxSize = 500,
  nperm = 10000
) -> rs_fgsea

rs_fgsea[order(pval),] %>% head()

sum(rs_sfgsea[, padj < 0.01])


plotEnrichment(examplePathways[["5990980_Cell_Cycle"]], exampleRanks)

rs_fgsea[ES > 0][head(order(pval), 10), pathway] -> topPathwaysUp
rs_fgsea[ES < 0][head(order(pval), 10), pathway] -> topPathwaysDown
topPathways <- c(topPathwaysUp, rev(topPathwaysDown))

plotGseaTable(examplePathways[topPathways], exampleRanks, rs_fgsea, gseaParam = 0.5)
library(data.table)

reactomePathways(names(exampleRanks)) -> pathways
fgseaRes <- fgsea(pathways = pathways, exampleRanks, nperm = 1000, maxSize = 500)
head(fgseaRes)


rnk.file <- system.file("extdata", "naive.vs.th1.rnk", package="fgsea")
gmt.file <- system.file("extdata", "mouse.reactome.gmt", package="fgsea")


ranks <- read.table(rnk.file,header=TRUE, colClasses = c("character", "numeric"))
ranks <- setNames(ranks$t, ranks$ID)
str(ranks)
pathways <- gmtPathways(gmt.file)
str(head(pathways))
fgseaRes <- fgsea(pathways, ranks, minSize=15, maxSize=500, nperm=1000)

