#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(ggplot2)
library(shiny)
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
library(shinyjs)
library(rdrop2)
library(curl)
library(vars)
source("JCCausality_algo.R")

# read it back with readRDS
token <- readRDS("droptoken.rds")
# Then pass the token to each drop <-  function
drop_acc(dtoken = token)


loaded <- function(){
  shinyjs::hide("loading_page")
  shinyjs::show("main_content")
}
loading <- function(){
  shinyjs::show("loading_page")
  shinyjs::hide("main_content")
}
radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}


save_allgraphics <- function(input, rvalues){
  window <- input$window
  alpha <- input$alpha
  isolate({
  lperiods <- seq.int(length(rvalues$ldates) - input$window + 1)
  tgnumbers <- table(rvalues$lgnumbers)
  glayout <- graph_from_adjacency_matrix(rvalues$lgraphs[[which.max(tgnumbers)]])
  layout <- layout.circle(glayout)
  lab.locs <- radian.rescale(x=seq.int(ncol(rvalues$lgraphs[[1]])), direction=-0.5, start=0)
  ddir <- paste("JCCausality/plots_", input$window, "_", input$alpha, sep="")
  ldir <- paste("plots/plots_", input$window, "_", input$alpha, sep="")
  if(!drop_exists(path = ddir)){
      drop_create(ddir)
  } 
  if(nrow(drop_dir(ddir)) < 4*length(lperiods)){
    dir.create(ldir, recursive=TRUE)
    withProgress(message = 'Creating plots', value = 0, {
       #                  for(period in lperiods){
     for(period in 1:2){
       period_min <- period     
       print(period_min)
       period_max <- period + input$window -1 
       label <- paste(rvalues$ldates[period], "to", rvalues$ldates[period + input$window - 1]) 
       distPlot <- ggplot(data=rvalues$dfm_m, aes(x=date, y=value, group=variable, color=variable))+geom_line()+facet_grid(variable ~ .)+ggtitle("raw time-series")+ geom_rect(aes(xmin=rvalues$ldates[period_min], xmax=rvalues$ldates[period_max], ymin=-Inf, ymax=Inf), color="black", alpha=0.00) + theme(legend.position="bottom")
       ggsave(filename=paste("distPlot_", period, ".png", sep=""), plot=distPlot, path=ldir, units="cm", height=12, width=20, dpi=100)
       graph <- graph_from_adjacency_matrix(rvalues$lgraphs[[period]])
       png(file=paste(ldir, "/graphPlot_", period, ".png", sep=""), height=12, width=12, units="cm", res=300)
       plot.igraph(graph, layout=layout, vertex.size=2, vertex.label.dist=1, vertex.label.degree=lab.locs, main=paste(label, ",  class: ", rvalues$lgnumbers[period], sep=""))
       dev.off()
       gnumberPlot <- ggplot(data=rvalues$df_gnumbers, aes(x=date, y=value, group=variable, color=value)) + geom_line() + facet_grid(variable ~ .) + ggtitle("graph number") + geom_vline(aes(xintercept=as.numeric(rvalues$ldates[period])), color="black") + coord_cartesian(xlim = range(rvalues$ldates)) + theme(legend.position="bottom") + scale_color_gradient(low="blue", high="red")
       ggsave(filename=paste("gnumberPlot_", period, ".png", sep=""), plot=gnumberPlot, path=ldir, units="cm", height=8, width=20, dpi=100)
       linkPlot <- ggplot(data=rvalues$alinks_m, aes(x=date, y=value, group=link)) + geom_step() + facet_wrap(~link) + ggtitle("links time-series") + geom_vline(aes(xintercept=as.numeric(rvalues$ldates[period])), color="red") + coord_cartesian(xlim = range(rvalues$ldates)) 
       ggsave(filename=paste("linkPlot_", period, ".png", sep=""), plot=linkPlot, path=ldir, units="cm", height=20, width=20, dpi=100)
       # Increment the progress bar, and update the detail text.
       incProgress(1/max(lperiods), detail = paste("plot #" ,period, "/", max(lperiods), sep=""))
     }
     print(ddir)
     print(ldir)
     lapply(list.files(ldir, full.names=TRUE), drop_upload, dest=ddir) 
     unlink(ldir)
  })
  }
  })
  ddir
}
dat <- readRDS("dfm.rds")

shinyServer(
function(input, output, session){
  # loading data and itialization
  rvalues <- reactiveValues() 
  observe({
    loading()
    allgraphs <- doallgraphs(dat, date_col=1, window_size=input$window, alpha=input$alpha)
    rvalues$dfm_m <- allgraphs$dat_m 
    rvalues$alinks_m <- allgraphs$alinks_m
    rvalues$lgraphs <- allgraphs$lgraphs 
    rvalues$lgnumbers <- allgraphs$lgnumbers 
    rvalues$ldates <- sort(unique(rvalues$dfm_m$dat))
    rvalues$df_gnumbers <- data.frame(date=rvalues$ldates[seq_along(rvalues$lgnumbers)], variable="gnumber", value=rvalues$lgnumbers)
  # layout <- layout.fruchterman.reingold(glayout)
  # layout <- layout.davidson.harel(glayout)
  # layout <- layout.sugiyama(glayout)$layout
  # layout <- layout.kamada.kawai(glayout)
    isolate({
      label <- paste(rvalues$ldates[input$period], "to", rvalues$ldates[input$period + input$window - 1]) 
      rvalues$ddir <- save_allgraphics(input, rvalues)
      updateSliderInput(session, "period", min = 1 , max = length(rvalues$ldates) - input$window + 1, label = label)
    })
    loaded()
  })
  observe({
    # Step size is 2 when input value is even; 1 when value is odd.
    label <- paste(rvalues$ldates[input$period], "to", rvalues$ldates[input$period + input$window - 1]) 
    updateSliderInput(session, "period", min = 1 , max = length(rvalues$ldates) - input$window + 1, label = label)
  }) 

  #   output$results  <-  renderPrint({
  #      input$nright 
  #   })

  observeEvent(input$nright, {
    isolate(updateSliderInput(session, "period", value = input$period + 1))
  }) 

  observeEvent(input$nleft, {
    isolate(updateSliderInput(session, "period", value = input$period - 1))
  })


  output$distPlot = renderUI({
    filename <- drop_media(paste(rvalues$ddir, "/distPlot_", input$period, ".png", sep=""))$url
    tags$img(src=filename, width=600, height=400)
  })

  output$graphPlot = renderUI({
    filename <- drop_media(paste(rvalues$ddir, "/graphPlot_", input$period, ".png", sep=""))$url
    tags$img(src=filename, width=400, height=400)
  })

  output$gnumberPlot = renderUI({
    filename <- drop_media(paste(rvalues$ddir, "/gnumberPlot_", input$period, ".png", sep=""))$url
    tags$img(src=filename, width=600, height=200)
  })

  output$linkPlot = renderUI({
    filename <- drop_media(paste(rvalues$ddir, "/linkPlot_", input$period, ".png", sep=""))$url
    tags$img(src=filename, width=600, height=600)
  })

})

