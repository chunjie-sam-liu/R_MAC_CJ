library(plsVarSel)
data(gasoline, package = "pls")
d <- with( gasoline, bve_pls(octane, NIR))


fit <- glm(formula = octane ~ NIR, data = gasoline)
summary(fit)
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
print(d.AD <- data.frame(treatment, outcome, counts))
glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
anova(glm.D93)
summary(glm.D93)

utils::data(anorexia, package = "MASS")

anorex.1 <- glm(Postwt ~ Prewt + Treat + offset(Prewt),
                family = gaussian, data = anorexia)
summary(anorex.1)
clotting <- data.frame(
  u = c(5,10,15,20,30,40,60,80,100),
  lot1 = c(118,58,42,35,27,25,21,19,18),
  lot2 = c(69,35,26,21,18,16,13,12,12))
summary(glm(lot1 ~ log(u), data = clotting, family = Gamma))
summary(glm(lot2 ~ log(u), data = clotting, family = Gamma))

library(parallel)
## This creates the cluster:
cl <- parallel::makeCluster(20, type = "PSOCK")
pls.options(parallel = cl)
parallel::stopCluster(cl)

y = gasoline$octane
X <- gasoline$NIR
bvepls <- function(y, X, ncomp = 10, ratio = 0.75, VIP.threshold = 1) {
  modeltype <- "prediction"
  if (is.factor(y)) {
    modeltype <- "classification"
    y.orig <- as.numeric(y)
    y <- model.matrix(~y - 1)
    tb <- names(table(y.orig))
  } else {
    y <- as.matrix(y)
  }
  X <- unclass(as.matrix(X))
  nn <- dim(X)[1]
  pp <- dim(X)[2]
  Z <- X
  terminated <- F
  Pg <- NULL
  K <- floor(nn * ratio)
  if (modeltype == "prediction") {
    K <- floor(nn * ratio)
    temp <- sample(nn)
    calk <- temp[1:K]
    Zcal <- Z[calk, ]
    ycal <- y[calk, ]
    Ztest <- Z[-calk, ]
    ytest <- y[-calk, ]
  } else if (modeltype == "classification") {
    calK <- c()
    for (jj in 1:length(tb)) {
      temp <- sample(which(y.orig == tb[jj]))
      K <- floor(length(temp) * ratio)
      calK <- c(calK, temp[1:K])
    }
    Zcal <- Z[calK, ]
    ycal <- y[calK, ]
    Ztest <- Z[-calK, ]
    ytest <- y[-calK, ]
  }
  Variable.list <- list()
  is.selected <- rep(T, pp)
  variables <- which(is.selected)
  while (!terminated) {
    co <- min(ncomp, ncol(Zcal) - 1)
    
    pls.object <- plsr(
      y ~ X, ncomp = co, 
      data = data.frame(y = I(ycal), X = I(Zcal)), 
      validation = "LOO"
    )
    
    if (modeltype == "prediction") {
      opt.comp <- which.min(pls.object$validation$PRESS[1, 
                                                        ])
    } else if (modeltype == "classification") {
      classes <- lda_from_pls_cv(pls.object, Zcal, y.orig[calK], 
                                 co)
      opt.comp <- which.max(colSums(classes == y.orig[calK]))
    }
    if (modeltype == "prediction") {
      mydata <- data.frame(
        yy = c(ycal, ytest), 
        ZZ = I(rbind(Zcal, Ztest)), 
        train = c(rep(TRUE, length(ycal)), rep(FALSE, length(ytest)))
      )
    } else {
      mydata <- data.frame(yy = I(rbind(ycal, ytest)), 
                           ZZ = I(rbind(Zcal, Ztest)), train = c(rep(TRUE, 
                                                                     length(y.orig[calK])), rep(FALSE, length(y.orig[-calK]))))
    }
    
    pls.fit <- plsr(
      yy ~ ZZ, ncomp = opt.comp, 
      data = mydata[mydata$train, ]
      )
    
    if (modeltype == "prediction") {
      pred.pls <- predict(pls.fit, ncomp = opt.comp, newdata = mydata[!mydata$train, 
                                                                      ])
      Pgi <- sqrt(sum((ytest - pred.pls[, , ])^2))
    } else if (modeltype == "classification") {
      classes <- lda_from_pls(pls.fit, y.orig[calK], Ztest, 
                              opt.comp)[, opt.comp]
      Pgi <- 100 - sum(y.orig[-calK] == classes)/nrow(Ztest) * 
        100
    }
    
    Pg <- c(Pg, Pgi)
    Vip <- VIP(pls.fit, opt.comp = opt.comp)
    VIP.index <- which(as.matrix(Vip) < VIP.threshold)
    if (length(VIP.index) <= (ncomp + 1)) {
      VIP.index <- sort(Vip, decreasing = FALSE, index.return = T)$ix[1:ncomp]
    }
    is.selected[variables[VIP.index]] <- F
    variables <- which(is.selected)
    Variable.list <- c(Variable.list, list(variables))
    Zcal <- Zcal[, VIP.index]
    Ztest <- Ztest[, VIP.index]
    indd <- unique(which(apply(Zcal, 2, var) == 0), which(apply(Ztest, 2, var) == 0))
    Zcal <- Zcal[, -indd]
    Ztest <- Ztest[, -indd]
    if (ncol(Zcal) <= ncomp + 1) {
      terminated <- TRUE
    }
  }
  opt.iter <- which.min(Pg)
  bve.selection <- Variable.list[[opt.iter]]
  return(list(bve.selection = bve.selection))
}
bvepls(y = y, X = X)

data(yarn)
yarn.pcr <- pcr(density ~ NIR, 6, data = yarn, validation = "CV")
yarn.pls <- plsr(density ~ NIR, 6, data = yarn, validation = "CV")
yarn.cppls <- cppls(density ~ NIR, 6, data = yarn, validation = "CV")
yarn.oscorespls <- mvr(density ~ NIR, 6, data = yarn, validation = "CV",
                       method = "oscorespls")
yarn.simpls <- mvr(density ~ NIR, 6, data = yarn, validation = "CV",
                   method = "simpls")
pls.options(parallel = 4) 
pls.options(parallel = quote(makeCluster(4, type = "PSOCK")))
yarn.pls <- plsr(density ~ NIR, 6, data = yarn, validation = "CV")
yarn.pcr <- pcr(density ~ NIR, 6, data = yarn, validation = "CV")
data(oliveoil)
sens.pcr <- pcr(sensory ~ chemical, ncomp = 4, scale = TRUE, data = oliveoil)
sens.pls <- plsr(sensory ~ chemical, ncomp = 4, scale = TRUE, data = oliveoil)


data(gasoline, package = "pls")
library(pls)
pls  <- plsr(octane ~ NIR, ncomp = 10, validation = "LOO", data = gasoline)
comp <- which.min(pls$validation$PRESS)
X    <- gasoline$NIR
vip <- VIP(pls, comp)
sr  <- SR (pls, comp, X)
smc <- sMC(pls, comp, X)
lw  <- LW (pls, comp)
rc  <- RC (pls, comp)
matplot(scale(cbind(vip, sr, smc, lw, rc)), type = 'l')
