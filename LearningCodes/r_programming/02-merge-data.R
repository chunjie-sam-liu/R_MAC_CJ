n <- 1000
set.seed(2018)

ldf <- data.frame(id1 = sample(n, n), id2 = sample(n / 100, n, replace = TRUE))
rdf <- data.frame(id1 = sample(n, n), id2 = sample(n / 100, n, replace = TRUE))

system.time(join1 <- merge(ldf, rdf, by = c("id1", "id2")))


ldt <- data.table::data.table(ldf, key = c("id1", "id2"))
rdt <- data.table::data.table(rdf, key = c("id1", "id2"))
system.time(join2 <- merge(ldt, rdt, key = c("id1", "id2")))
