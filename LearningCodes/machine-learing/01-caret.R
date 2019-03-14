
# visualizations ----------------------------------------------------------

str(iris)

library(AppliedPredictiveModeling)
transparentTheme(trans = 0.4)
library(caret)
featurePlot(x = iris[, 1:4], y = iris$Species, plot = 'pairs', auto.key = list(columns = 3))
featurePlot(x = iris[, 1:4], y = iris$Species, plot = 'ellipse', auto.key = list(columns = 3))
