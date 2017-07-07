library(googlesheets)
atg_lys_ss <- gs_title("1.gene_list.xlsx")

gs_ws_ls(ss = atg_lys_ss)

atg_lys_gene <- gs_read(ss = atg_lys_ss, ws = "atg_lys_complete")
# rds_03_at_ly_comb_gene_list.rds.gz

readr::write_rds(x = atg_lys_gene, path = "rds_03_a_atg_lys_gene_list.rds.gz")

