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





