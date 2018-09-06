install.packages('cgdsr')

library(cgdsr)
mycgds = CGDS("http://www.cbioportal.org/public-portal/")


test(mycgds)


getCancerStudies(mycgds) -> cancers


# analysis ----------------------------------------------------------------



# Get available case lists (collection of samples) for a given cancer study
# lung cancer 103
mycancerstudy = getCancerStudies(mycgds)[103,1]
mycaselist = getCaseLists(mycgds,mycancerstudy)[1,1]


# Get available genetic profiles
mygeneticprofile = getGeneticProfiles(mycgds,mycancerstudy)[c(1, 3, 5), 1]

# luad_tcga_pan_can_atlas_2018_gistic -> cnv amp -> 2, del -> -2
# luad_tcga_pan_can_atlas_2018_rna_seq_v2_mrna_median_Zscores -> expression up -> 2.33 down -> -2.33
# luad_tcga_pan_can_atlas_2018_mutations -> mutation

# Get data slices for a specified list of genes, genetic profile and case list
gene_list <- c("YAP1", "TAZ", "YKT6")

gene_list %>% 
  purrr::map(
    .f = function(.x) {
      getProfileData(mycgds, .x , mygeneticprofile,mycaselist) %>% tibble::as_tibble() -> d
      names(d) <- c("cnv", "rna", "snv")

# Data transformation -----------------------------------------------------

      
      d %>% 
        dplyr::mutate(cnv = cnv %>% as.character() %>% as.numeric()) %>% 
        dplyr::mutate(rna = rna %>% as.character() %>% as.numeric()) %>% 
        dplyr::mutate(snv = snv %>% as.character()) ->
        dd

# Cal output --------------------------------------------------------------

      
      dd %>% dplyr::filter(cnv == 2) %>% nrow() -> amp
      dd %>% dplyr::filter(cnv == -2) %>% nrow() -> del
      
      dd %>% dplyr::filter(rna > 2.33) %>% nrow() -> up
      dd %>% dplyr::filter(rna < -2.33) %>% nrow() -> down
      
      dd %>% dplyr::filter(grepl(pattern = "\\w+\\d+\\w+", x = snv)) %>% nrow() -> mutation
      
      tibble::tibble(gene = .x, amp = amp, del = del, up = up, down = down, mutation = mutation) %>% 
        dplyr::mutate_if(.predicate = is.numeric, .funs = dplyr::funs(round(. / nrow(dd), digits = 3)))
        
    }
  ) %>% 
  dplyr::bind_rows() ->
  rs

rs %>% writexl::write_xlsx(path = "yaptaz.xlsx")






