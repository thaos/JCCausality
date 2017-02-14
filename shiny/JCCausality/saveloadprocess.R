
radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}

save_allgraphics <- function(window, alpha, ldir, ddir, dfm_m, ldates, lgnumbers, lgraphs, alinks_m){
  lperiods <- seq.int(length(ldates))
  tgnumbers <- table(lgnumbers)
  df_gnumbers <- data.frame(date=ldates[seq_along(lgnumbers)], variable="gnumber", value=lgnumbers)
  glayout <- graph_from_adjacency_matrix(lgraphs[[which.max(tgnumbers)]])
  layout <- layout.circle(glayout)
  lab.locs <- radian.rescale(x=seq.int(ncol(lgraphs[[1]])), direction=-0.5, start=0)
  withProgress(message = 'Creating plots', value = 0, {
     for(period in lperiods){
       #      for(period in 1:1){
       period_min <- period     
       print(period_min)
       period_max <- period + window -1 
       print(head(dfm_m))
       distPlot <- ggplot(data=dfm_m, aes(x=date, y=value, group=variable, color=variable))+geom_line()+facet_grid(variable ~ .)+ggtitle("raw time-series")+ geom_rect(aes(xmin=ldates[period_min], xmax=ldates[period_max], ymin=-Inf, ymax=Inf), color="black", alpha=0.00) + theme(legend.position="bottom")
       ggsave(filename=paste("distPlot_", period, ".png", sep=""), plot=distPlot, path=ldir, units="cm", height=12, width=20, dpi=100)

       label <- paste(ldates[period], "to", ldates[period + window - 1]) 
       graph <- graph_from_adjacency_matrix(lgraphs[[period]])
       png(file=paste(ldir, "/graphPlot_", period, ".png", sep=""), height=12, width=12, units="cm", res=300)
       plot.igraph(graph, layout=layout, vertex.size=2, vertex.label.dist=1, vertex.label.degree=lab.locs, main=paste(label, ",  class: ", lgnumbers[period], sep=""))
       dev.off()

       gnumberPlot <- ggplot(data=df_gnumbers, aes(x=date, y=value, group=variable, color=value)) + geom_line() + facet_grid(variable ~ .) + ggtitle("graph number") + geom_vline(aes(xintercept=as.numeric(ldates[period])), color="black") + coord_cartesian(xlim = range(ldates)) + theme(legend.position="bottom") + scale_color_gradient(low="blue", high="red")
       ggsave(filename=paste("gnumberPlot_", period, ".png", sep=""), plot=gnumberPlot, path=ldir, units="cm", height=8, width=20, dpi=100)

       linkPlot <- ggplot(data=alinks_m, aes(x=date, y=value, group=link)) + geom_step() + facet_wrap(~link) + ggtitle("links time-series") + geom_vline(aes(xintercept=as.numeric(ldates[period])), color="red") + coord_cartesian(xlim = range(ldates)) 
       ggsave(filename=paste("linkPlot_", period, ".png", sep=""), plot=linkPlot, path=ldir, units="cm", height=20, width=20, dpi=100)
       # Increment the progress bar, and update the detail text.
       incProgress(1/max(lperiods), detail = paste("plot #" ,period, "/", max(lperiods), sep=""))
     }
     print(ddir)
     print(ldir)
     lapply(list.files(ldir, full.names=TRUE), drop_upload, dest=ddir) 
  })
  ldir
}

saveorload <- function(window, alpha){
  ddir <- paste("JCCausality/plots_", window, "_", alpha, sep="")
  ldir <- paste("plots/plots_", window, "_", alpha, sep="")
  if(!drop_exists(path = ddir)){
    drop_create(ddir)
  } 
  dat <- readRDS("dfm.rds")
  lperiods <- seq.int(length(unique(dat[, 1])) - window + 1)
  ldates <- sort(unique(dat[, 1]))
  if(nrow(drop_dir(ddir)) < 4*length(lperiods)){
    dir.create(ldir, recursive=TRUE)
    allgraphs <- doallgraphs(dat, date_col=1, window_size=window, alpha=alpha)
    print(summary(allgraphs))
    save_allgraphics(window, alpha, ldir, ddir, allgraphs$dat_m, allgraphs$ldates, allgraphs$lgnumbers, allgraphs$lgraphs, allgraphs$alinks_m)
  }else{
    if(length(list.files(ldir)) < 4*length(lperiods)){
      dir.create(ldir, recursive=TRUE)
      print(drop_dir(ddir)$path)
      print(file.exists(ldir))
      dfpath <- drop_dir(ddir)$path
      lfpath <- gsub("/JCCausality", "plots", dfpath)
      withProgress(message = 'Downloading plots', value = 0, {
	for(i in seq_along(dfpath)){
	  drop_get(path=dfpath[i], local_file=lfpath[i], overwrite=TRUE)
	  incProgress(1/max(lperiods), detail = paste("plot #" , i, "/", length(dfpath), sep=""))
	}
      })
    }
  }
  ans <- list(ldir=ldir, ldates=ldates)
  ans
}



