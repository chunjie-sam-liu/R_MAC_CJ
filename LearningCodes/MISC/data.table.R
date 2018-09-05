# data.table

# examples from data.table cheatsheet

library(data.table)


set.seed(45L)

DT <- data.table(
  V1 = c(1L, 2L),
  V2 = LETTERS[1:3],
  V3 = round(rnorm(4), 4), 
  V4 = 1:12
)

DT

# DT[i, j, by]
# Subset using i ----------------------------------------------------------


DT[1:2]
DT[1:2,]

DT[V2 == "A"]


DT[V2 %in% c("A", "C")]


# manipulating on columns in j --------------------------------------------
class(DT)
DT[, list(V2)]

DT[, list(V2, V3)]
DT[, sum(V1)]
DT[, list(sum(V1), sd(V3))]
DT[, list(Aggregate = sum(V1), Sd.V3 = sd(V3))]

DT[, list(V1, sd = sd(V3))] # recycle

DT[, {print(V2)
  plot(V3)
  NULL}]


# doing j by group --------------------------------------------------------

DT[, list(V4.sum = sum(V4)), by = V1]

DT[, .(V4.sum = sum(V4)), by = .(V1, V2)]

DT[, .(V4.sum = sum(V4)), by = sign(V1 - 1)]

DT[1:5, .(V4.sum = sum(V4)), by = V1]

DT[, .N, by = V1]

DT[, V1 := round(exp(V1), 2)]
DT

DT[, c("V1", "V2") := list(round(exp(V1), 2), LETTERS[4:6])]
DT[, ':='(V1 = round(exp(V1), 2), V2 = LETTERS[4:6])][]
DT[, V1 := NULL]
DT[, c("V3", "V2") := NULL]


setkey(DT, V2)
DT
DT["A"]


# Advanced data.table -----------------------------------------------------


