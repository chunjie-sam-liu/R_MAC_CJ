library(googlesheets)
atg_lys_ss <- gs_title("1.gene_list.xlsx")

gs_ws_ls(ss = atg_lys_ss)

atg_lys_gene <- gs_read(ss = atg_lys_ss, ws = "atg_lys_complete")
# rds_03_at_ly_comb_gene_list.rds.gz

atg_lts_mark <- gs_read(ss = atg_lys_ss, ws = "reported_marker_gene")
readr::write_rds(x = atg_lts_mark, path = "rds_03_a_atg_lys_marker.rds.gz", compress = "gz")

readr::write_rds(x = atg_lys_gene, path = "rds_03_a_atg_lys_gene_list.rds.gz")

test_gene_list <- gs_read(ss = atg_lys_ss, ws = "reported_marker_gene")
