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
saveRDS(dfm[, -c(1:2)], file="dfm.rds")
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

