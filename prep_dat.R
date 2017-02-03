library(gRbase)
library(corrplot)
library(lubridate)
library(dplyr)
library(reshape2)
library(tseries)
library(vars)
lfiles <- list.files("Data")
lfiles_complete <- list.files("Data", full.names=TRUE)
ldf <- lapply(lfiles_complete, read.table, header=TRUE)
lvarnames <- lapply(lfiles, strsplit, "_")
lvarnames <- sapply(lvarnames, function(x){
		      x <- unlist(x)
		      paste(x[3],"_",x[4], sep="")
})
lvarnames <- gsub("-", "_", lvarnames)
ldf <- mapply(function(x, y){
	     names(x)[3] <- y
	     x
},ldf, lvarnames, SIMPLIFY=FALSE
)
dfm <- merge(ldf[[1]], ldf[[2]])
for(i in 2:length(ldf)) dfm <- merge(dfm, ldf[[i]])
dfm_y <- dfm
dfm_y <- dfm_y[with(dfm_y, order(year, month)), ]
dfm_y <- as.data.frame(scale(dfm, scale=TRUE))

dfm <- mutate(dfm, season = ceiling(month/3))
groupf <- with(dfm, paste(year, season))
dfm <- split.data.frame(dfm, f=groupf)
dfm <- lapply(dfm, function(x) apply(x, 2, mean))
dfm <- as.data.frame(do.call(rbind, dfm))
dfm$month <- with(dfm, floor(month)) 
dfm <- mutate(dfm, date = as.Date(paste(year, month, "01", sep="-")))
dfm[,!(names(dfm) %in% c("year", "month", "season", "date"))] <- scale(dfm[, !(names(dfm) %in% c("year", "month", "season", "date"))], scale=TRUE)

dfm_s <-  melt(dfm, id=c("year", "month", "season", "date"))
dfm_s <-  dcast(dfm_s, year ~ variable + season) 

# dfm <- with(dfm, paste(year, month, "01", sep="-"))
# dfm <- arrange(dfm, date)
# 
dfm_m <- dfm[, c(!(names(dfm) %in% c("skt_Amaz_NE", "skt_Amaz_SE", "skt_Amaz_NW", "skt_Amaz_SW")))]
cormatrix <- cor(dfm_m[, sapply(dfm_m, is.numeric)])
corrplot(cormatrix)
dfm_m  <- melt(dfm_m, id=c("year", "month", "season", "date"))
ggplot(data=dfm_m, aes(x=date, y=value, group=variable, color=variable))+geom_line()

# simple test for 2 variables grangers causality
dfm_0 <- dfm[, c("skt_Amaz_SE", "skt_EN34", "sic_area")]
dfm_1 <- dfm[, c("skt_Amaz_CE", "skt_EN34", "sic_area")]
dfm_2 <- dfm[, c(!(names(dfm) %in% c("date", "year", "month", "season", "skt_Amaz_NE", "skt_Amaz_SE", "skt_Amaz_NW", "skt_Amaz_SW")))]
dfm_3 <- dfm[, c(!(names(dfm) %in% c("date", "year", "month", "season", "skt_Amaz_NE", "skt_Amaz_CE", "skt_Amaz_NW", "skt_Amaz_SW")))]
dfm_4 <- dfm[, c(!(names(dfm) %in% c("date", "year", "month", "season")))]
dfm_y4 <- dfm_y[, c(!(names(dfm) %in% c("date", "year", "month", "season")))]
tests_stationarity <- function(df){
  lapply(df, function(x) list(acf=acf(x),
				 Box.test=Box.test(x),
				 adf.test=adf.test(x),
				 kpss.test=kpss.test(x)))
}
stats_dfm_4 <- tests_stationarity(dfm_4)
stats_dfm_4 <- tests_stationarity(dfm_y4)
dfm_s <- dfm[dfm$season==3, ]
dfm_s <- dfm_s[, c(!(names(dfm) %in% c("date", "year", "month", "season")))]
dfm_4 <- apply(dfm_4, 2, diff)
dfm_y4 <- apply(dfm_y4, 2, diff)
VARselect(dfm_4)
VARselect(dfm_y4)
VARselect(dfm_s)
varmodel_0 <- VAR(dfm_0[, -1], p=5)
varmodel_1 <- VAR(dfm_1[, -1], p=2)
varmodel_2 <- VAR(dfm_2[, -ncol(dfm_2)], p=8)
varmodel_3 <- VAR(dfm_3, p=8)
varmodel_4 <- VAR(dfm_4, p=1)
varmodel_y4 <- VAR(dfm_y4, p=24)
gENtoAmaz <- grangertest(skt_Amaz_CE~skt_EN34, order=2, data=dfm_1[, -1])
gAmaztoEN <- grangertest(skt_EN34~skt_Amaz_CE, order=2, data=dfm_1[, -1])

waldtest(varmodel_1[[1]]$skt_Amaz_CE, varmodel_2[[1]]$skt_Amaz_CE)

vm_to_granger <- function(vm, alpha){
  lmodels <- vm[[1]] 
  ynames <- names(lmodels)
  dobackward <- function(yname, alpha){
    model <- lmodels[[yname]]
    dat <- model$model
    lower <- colnames(dat)[grep(yname, colnames(dat))] 
    lower <- paste(lower, collapse="+")
    lower <- paste("y ~ ", lower, sep="")
    lower <- paste(lower, "+const", sep="")
    lower <- as.formula(lower)
    lm_fit <- lm(y ~ -1 + ., data=dat)
    upper <- lm_fit$terms
    attributes(upper) <- NULL
    scope <- list(lower=lower, upper=upper)
    lm_fit <- step(lm_fit, scope=scope, direction='backward', trace=0) 
    print(summary(lm_fit)) 
    coeff <- summary(lm_fit)$coefficients
    vsignif <- which(coeff[, 4] <= alpha)
    vsignif <- rownames(coeff)[vsignif]
    lsignif <- sapply(vsignif, function(x){
			lag <- substr(x, nchar(x), nchar(x)) 
			ans <- 0
			try( ans <-  as.numeric(lag))
			ans})
    vsignif <- sub("\\.l.$", "", vsignif)
    ans <- matrix(ynames %in% vsignif, ncol=1) 
  }
  ans <- sapply(ynames, dobackward, alpha=alpha)
  colnames(ans) <- ynames
  rownames(ans) <- ynames
  ans
}

gmat <- vm_to_granger(varmodel_4, 0.05)
gmat <- (as(gmat, "graphNEL"))
plot(gmat)

vm_to_granger <- function(vm, alpha){
  lmodels <- vm[[1]] 
  ynames <- names(lmodels)
  res <- matrix(1, ncol=length(ynames), nrow=length(ynames))
  gtest <- function(yname, alpha){
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
gmat <- vm_to_granger(varmodel_4)
gmat <- vm_to_granger(varmodel_y4)
gmat <- gmat <= 0.05
gmat <- (as(gmat, "graphNEL"))
plot(gmat)

	


