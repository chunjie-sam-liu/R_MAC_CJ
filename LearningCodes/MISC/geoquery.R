library(GEOquery)


gds <- getGEO(filename=system.file("extdata/GDS507.soft.gz",package="GEOquery"))

gsm <- getGEO(filename=system.file("extdata/GSM11805.txt.gz",package="GEOquery"))
gse <- getGEO(filename=system.file("extdata/GSE781_family.soft.gz",package="GEOquery"))
head(Meta(gse))
names(GSMList(gse))
GSMList(gse)[[1]]
names(GPLList(gse))
gse2553 <- getGEO('GSE2553',GSEMatrix=TRUE)
