# library(gam)
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
dfm <- mutate(dfm, skt_Amaz = (skt_Amaz_CE +  skt_Amaz_NE + skt_Amaz_NW + skt_Amaz_SW + skt_Amaz_SE)/5)
dfm <- mutate(dfm, skt_TN = (skt_TN1 + skt_TN2)/2)
dfm <- mutate(dfm, date = as.Date(paste(year, month, "01", sep="-")))
dfm[,!(names(dfm) %in% c("year", "month", "season", "date"))] <- scale(dfm[, !(names(dfm) %in% c("year", "month", "season", "date"))], scale=TRUE)
# dfm <- arrange(dfm, date)
dfm <- dfm[order(dfm$date),]


bw <-12 
weights <- rep(1/bw, bw)
idfm <- dfm[,!(names(dfm) %in% c("year", "month", "season", "date"))] 
rmean  <- as.data.frame(lapply(idfm, stats::filter, weights, side=2))
rmean <- as.data.frame(lapply(idfm, function(x) rollapply(x, width=bw, FUN=mean, fill=NA)))
rvar <- as.data.frame(lapply(idfm, function(x) rollapply(x, width=bw, FUN=var, fill=NA)))
rmgam <- as.data.frame(lapply(idfm, function(x) predict(gam(x  ~ s(as.numeric(dfm$date))))))
rmgamv <- as.data.frame(lapply(idfm-rmgam, function(x) rollapply(x, width=bw, FUN=function(y)mean(y^2), fill=NA)))
rmean <- cbind(dfm[, c("year", "month", "date")], rmean) 
rvar <- cbind(dfm[, c("year", "month", "date")], rvar) 
rmgam <- cbind(dfm[, c("year", "month", "date")], rmgam) 
rmgamv <- cbind(dfm[, c("year", "month", "date")], rmgamv) 

dfm <- dfm[, c("year", "month", "date", "sic_area", "skt_NA", "skt_EN34", "skt_TN", "skt_Amaz")]
dfm_m  <- melt(dfm, id=c("year", "month", "date"))
saveRDS(dfm_m, file="dfm_m.rds")
ggplot(data=dfm_m, aes(x=date, y=value, group=variable, color=variable))+geom_line()+facet_grid(variable ~ .)+ggtitle("raw time-series")
cormatrix <- cor(dfm[, sapply(dfm, is.numeric)], use="pairwise.complete.obs")
corrplot(cormatrix)

rmean <- rmean[, c("year", "month", "date", "sic_area", "skt_NA", "skt_EN34", "skt_TN", "skt_Amaz")]
rmean_m  <- melt(rmean, id=c("year", "month", "date"))
ggplot(data=rmean_m, aes(x=date, y=value, group=variable, color=variable))+geom_line()+facet_grid(variable ~ .)+ggtitle("running mean")
cormatrix <- cor(rmean[, sapply(rmean, is.numeric)], use="pairwise.complete.obs")
corrplot(cormatrix)

rvar <- rvar[, c("year", "month", "date", "sic_area", "skt_NA", "skt_EN34", "skt_TN", "skt_Amaz")]
rvar_m  <- melt(rvar, id=c("year", "month", "date"))
ggplot(data=rvar_m, aes(x=date, y=value, group=variable, color=variable))+geom_line()+facet_grid(variable ~ .)+ggtitle("running variance")
cormatrix <- cor(rvar[, sapply(rvar, is.numeric)], use="pairwise.complete.obs")
corrplot(cormatrix)

rmgam <- rmgam[, c("year", "month", "date", "sic_area", "skt_NA", "skt_EN34", "skt_TN", "skt_Amaz")]
rmgam_m  <- melt(rmgam, id=c("year", "month", "date"))
ggplot(data=rmgam_m, aes(x=date, y=value, group=variable, color=variable))+geom_line()+facet_grid(variable ~ .)+ggtitle("running variance")
cormatrix <- cor(rmgam[, sapply(rmgam, is.numeric)], use="pairwise.complete.obs")
corrplot(cormatrix)

rmgamv <- rmgamv[, c("year", "month", "date", "sic_area", "skt_NA", "skt_EN34", "skt_TN", "skt_Amaz")]
rmgamv_m  <- melt(rmgamv, id=c("year", "month", "date"))
ggplot(data=rmgamv_m, aes(x=date, y=value, group=variable, color=variable))+geom_line()+facet_grid(variable ~ .)+ggtitle("running variance")
cormatrix <- cor(rmgamv[, sapply(rmgamv, is.numeric)], use="pairwise.complete.obs")
corrplot(cormatrix)


dfm_h <- (dfm[, -(1:3)] - rmean[, -(1:3)])/sqrt(rvar[, -(1:3)])  
dfm_h <- cbind(dfm[, c("year", "month", "date")], dfm_h) 
dfm_h <- dfm_h[, c("year", "month", "date", "sic_area", "skt_NA", "skt_EN34", "skt_TN", "skt_Amaz")]
dfm_h_m  <- melt(dfm_h, id=c("year", "month", "date"))
ggplot(data=dfm_h_m, aes(x=date, y=value, group=variable, color=variable))+geom_line()+facet_grid(variable ~ .)+ggtitle("running variance")
cormatrix <- cor(dfm_h[, sapply(dfm_h, is.numeric)], use="pairwise.complete.obs")
corrplot(cormatrix)
plot(hexplom(dfm_h[, -(1:3)]))

dfm_dt <- (dfm[, -(1:3)] - rmgam[, -(1:3)])/sqrt(rmgamv[, -(1:3)])  
dfm_dt <- cbind(dfm[, c("year", "month", "date")], dfm_dt) 
dfm_dt <- dfm_dt[, c("year", "month", "date", "sic_area", "skt_NA", "skt_EN34", "skt_TN", "skt_Amaz")]
dfm_dt_m  <- melt(dfm_dt, id=c("year", "month", "date"))
dfm_dt_m$value <- as.numeric(dfm_dt_m$value)
ggplot(data=dfm_dt_m, aes(x=date, y=value, group=variable, color=variable))+geom_line()+facet_grid(variable ~ .)+ggtitle("running variance")
cormatrix <- cor(dfm_dt[, sapply(dfm_dt, is.numeric)], use="pairwise.complete.obs")
corrplot(cormatrix)
plot(hexplom(dfm_dt[, -(1:3)]))

# simple test for 2 variables grangers causality
tests_stationarity <- function(df){
  lapply(df, function(x) list(acf=acf(x),
			      pcaf=pacf(x),
			      Box.test=Box.test(x),
			      adf.test=adf.test(x),
			      kpss.test=kpss.test(x)))
}

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
# gmat <- vm_to_granger(varmodel)
# gmat <- gmat <= 0.05
# gmat <- (as(gmat, "graphNEL"))
# plot(gmat)

	

build_ggraph <- function(dat){
  ina <- which(apply(dat, 1, function(x) any(is.na(x))))
  if(length(ina) > 0) dat <- dat[-ina, ] 
  #   stests <- tests_stationarity(dat)
  vs <- VARselect(dat)
  varmodel <- VAR(dat, p=max(vs$selection))
  gmat <- vm_to_granger(varmodel)
  gmat <- gmat <= 0.01
  gmat <- (as(gmat, "graphNEL"))
  list(gmat, varmodel)
}
dat <- rmean[, -(1:3)]
dat <- dfm_dt[, -(1:3)]
dat <- dfm[, -(1:3)]
ina <- which(apply(dat, 1, function(x) any(is.na(x))))
if(length(ina) > 0) dat <- dat[-ina, ] 
dat1 <- dat[1:140, ] 
dat2 <- dat[141:280, ] 
dat3 <- tail(dat, 140) 
# pdf("graph_ts_raw.pdf")
# rlggraphs <- rollapply(data=dat, width=120, by.column=FALSE, function(x) plot(build_ggraph(x)[[1]]))
rlggraphs <- lapply(1:(nrow(dat)-120),function(x){
	 d <- dat[x:(x+119), ] 
	 as(build_ggraph(d)[[1]], "matrix")
})
gunique <- unique(rlggraphs)
gunique <- gunique[order(sapply(gunique, sum))]
matequal <- function(x, y)
    is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
whichgraph <- function(x, lgraph){
  which(sapply(gunique, function(y) matequal(y, x)))
}
gnumber <- sapply(rlggraphs, whichgraph, lgraph=gunique)
plot(gnumber, type="H")
pdf("graph_ts_raw.pdf")
lapply(seq_along(rlggraphs), function(i) plot(as(rlggraphs[[i]], "graphNEL"), main=paste(i, ",  class: ", gnumber[i], sep="")))
dev.off()
# dev.off()
pdf("graph_3periods_dt.pdf")
lapply(list(dat, dat1, dat2, dat3), function(x) plot(build_ggraph(x)[[1]]))
dev.off()

write.table(dat, file="tigramite/dat.txt", row.names=FALSE, sep=",")
write.table(dat1, file="tigramite/dat_p1.txt", row.names=FALSE, sep=",")
write.table(dat2, file="tigramite/dat_p2.txt", row.names=FALSE, sep=",")
write.table(dat3, file="tigramite/dat_p3.txt", row.names=FALSE, sep=",")

L <- lapply(rlggraphs, as.matrix)
garray <- array(unlist(L), dim = c(dim(L[[1]]), length(L)))
dimnames(garray) <- dimnames(L[[1]])
# dimnames(garray)[[3]] <- 
garray_m <- melt(garray)
garray_m <- garray_m[garray_m[[1]] != garray_m[[2]],]
garray_m$link <- paste(garray_m[[1]],"->", garray_m[[2]])
ggplot(data=garray_m, aes(x=Var3, y=value, group=link))+geom_line()+facet_wrap(~link)
writeMat("ggraph_a0.05.mat",  garray_a05 = garray)

drmean <- rmean[, -(1:3)]
ina <- which(apply(drmean, 1, function(x) any(is.na(x))))
if(length(ina) > 0) drmean <- drmean[-ina, ] 
drmean1 <- drmean[1:140, ] 
drmean2 <- drmean[141:280, ] 
drmean3 <- tail(drmean, 140) 
pdf("graph_3periods_drmean.pdf")
lapply(list(drmean, drmean1, drmean2, drmean3), function(x) plot(build_ggraph(x)[[1]]))
dev.off()

write.table(drmean, file="tigramite/drmean.txt", row.names=FALSE, sep=",")
write.table(drmean1, file="tigramite/drmean_p1.txt", row.names=FALSE, sep=",")
write.table(drmean2, file="tigramite/drmean_p2.txt", row.names=FALSE, sep=",")
write.table(drmean3, file="tigramite/drmean_p3.txt", row.names=FALSE, sep=",")


