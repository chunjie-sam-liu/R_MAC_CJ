library(magrittr)
rppa_json_url <- "http://tcpaportal.org/tcpa/_design/basic/_show/annotation-antibody_list/annotation-antibody"

jsonlite::fromJSON(txt = rppa_json_url) %>% 
  dplyr::as_tibble() %>% 
  dplyr::select(`0`:`5`) %>% 
  dplyr::rename(
    protein = `0`,
    symbol = `1`,
    status = `2`,
    orign = `3`,
    source = `4`,
    catalog = `5`
  ) %>% 
  dplyr::mutate_all(.funs = dplyr::funs(stringr::str_trim)) -> protein_symbol_map


atg_lys_gene_list <-
  readr::read_rds(path = "rds_03_a_atg_lys_gene_list.rds.gz")

core_atg <- 
  atg_lys_gene_list %>% 
  dplyr::filter(pathway == "autophagesome formation-core")

protein_symbol_map %>% 
  dplyr::inner_join(core_atg, by = "symbol") -> core_atg_protein
# only BECN1

# maybe add all regulators
protein_symbol_map %>% 
  dplyr::inner_join(atg_lys_gene_list, by = "symbol")




