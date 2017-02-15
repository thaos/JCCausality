library(mgcv)
library(hexbin)
library(gRbase)
library(igraph)
library(zoo)
library(corrplot)
library(lubridate)
library(dplyr)
library(reshape2)
library(tseries)
library(vars)

# simple test for 2 variables grangers causality
tests_stationarity <- function(df){
  lapply(df, function(x) list(acf=acf(x),
			      pcaf=pacf(x),
			      Box.test=Box.test(x),
			      adf.test=adf.test(x),
			      kpss.test=kpss.test(x)))
}

vm_to_granger <- function(vm){
  lmodels <- vm[[1]] 
  ynames <- names(lmodels)
  res <- matrix(1, ncol=length(ynames), nrow=length(ynames))
  gtest <- function(yname){
    model <- lmodels[[yname]]
    dat <- model$model
    lm_fit <- lm(y ~ -1 + ., data=dat)
    ytotests <- ynames[!(ynames %in% yname)]
    wtest <- function(ytotest, lm_fit){
      xnames <- rownames(summary(lm_fit)$coefficients)
      xnames <- xnames[-grep(ytotest, xnames)] 
      lower <- paste(xnames, collapse="+")
      lower <- paste("y ~ -1+", lower, sep="")
      lower <- as.formula(lower)
      lm_h0 <- update(lm_fit, formula=lower)
      waldtest(lm_fit, lm_h0)[2, 4]
    }
    sapply(ytotests, wtest, lm_fit=lm_fit)
  }
  ans <- sapply(ynames, gtest)
  for(i in seq_along(ynames)){
    res[-i, i] <- ans[, i]
  }
  colnames(res) <- ynames
  rownames(res) <- ynames
  res
}
# gmat <- vm_to_granger(varmodel)
# gmat <- gmat <= 0.05
# gmat <- (as(gmat, "graphNEL"))
# plot(gmat)

build_ggraph <- function(dat, alpha){
  ina <- which(apply(dat, 1, function(x) any(is.na(x))))
  if(length(ina) > 0) dat <- dat[-ina, ] 
  #   stests <- tests_stationarity(dat)
  vs <- VARselect(dat)
  varmodel <- VAR(dat, p=max(vs$selection))
  gmat <- vm_to_granger(varmodel)
  gmat <- gmat <= alpha
  gmat <- (as(gmat, "graphNEL"))
  list(gmat, varmodel)
}
matequal <- function(x, y)
  is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)

doallgraphs <- function(dat, date_col, window_size=120, alpha=0.01){
  ina <- which(apply(dat, 1, function(x) any(is.na(x))))
  if(length(ina) > 0) dat <- dat[-ina, ] 
  dat_m  <- melt(dat, id="date")
  ldates <- dat[, date_col]
  dat <- dat[, -date_col]
  print(head(dat))
  lgraphs <- lapply(1:(nrow(dat)-(window_size-1)),
		      function(x){
			d <- dat[x:(x+window_size-1), ] 
			as(build_ggraph(d, alpha)[[1]], "matrix")
  })
  gunique <- unique(lgraphs)
  gunique <- gunique[order(sapply(gunique, sum))]
  whichgraph <- function(x, lgraph){
    which(sapply(gunique, function(y) matequal(y, x)))
  }
  lgnumbers <- sapply(lgraphs, whichgraph, lgraph=gunique)
  alinks <- array(unlist(lgraphs), dim = c(dim(lgraphs[[1]]), length(lgraphs)))
  dimnames(alinks) <- dimnames(lgraphs[[1]])
  alinks_m <- melt(alinks)
  alinks_m <- alinks_m[alinks_m[[1]] != alinks_m[[2]],]
  alinks_m$link <- paste(alinks_m[[1]],"->", alinks_m[[2]])
  alinks_m$date <- rep(ldates[seq_along(lgraphs)], table(alinks_m[[3]]))
  alinks_m <- alinks_m[, -(1:3)]
  names(lgraphs) <- ldates[seq_along(lgraphs)]
  names(lgnumbers) <- ldates[seq_along(lgnumbers)]
  list(dat_m=dat_m, alinks_m=alinks_m, lgraphs=lgraphs, lgnumbers=lgnumbers, window_size=window_size, alpha=alpha)
}
