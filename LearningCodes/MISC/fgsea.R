# fgsea

library(fgsea)


# load pathways and gene-level statistics ---------------------------------


data("examplePathways")

examplePathways %>% 
  purrr::map_int(length) %>% 
  tibble::enframe() %>%
  dplyr::arrange(-value) ->
  .d

.d %>% dplyr::filter(dplyr::between(value, 15, 500))

data("exampleRanks")

exampleRanks %>% head()


fgsea::fgsea(
  pathways = examplePathways,
  stats = exampleRanks,
  minSize = 15,
  maxSize = 500,
  nperm = 10000
) -> rs_fgsea

rs_fgsea[order(pval),] %>% head()

sum(rs_fgsea[, padj < 0.01])


plotEnrichment(examplePathways[["5990980_Cell_Cycle"]], exampleRanks)

rs_fgsea[ES > 0][head(order(pval), 10), pathway] -> topPathwaysUp
rs_fgsea[ES < 0][head(order(pval), 10), pathway] -> topPathwaysDown
topPathways <- c(topPathwaysUp, rev(topPathwaysDown))

plotGseaTable(examplePathways[topPathways], exampleRanks, rs_fgsea, gseaParam = 0.5)
