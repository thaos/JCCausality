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
library(vars)
source("JCCausality_algo.R")


loaded <- function(){
  hide("loading_page")
  show("main_content")
}
loading <- function(){
  show("loading_page")
  hide("main_content")
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
    updateSliderInput(session, "period", min = 1 , max = length(rvalues$ldates) - input$window + 1, label = label)
    print(summary(rvalues$df_gnumbers))
    loaded()
  })
  tgnumbers <- table(lgnumbers)
  glayout <- graph_from_adjacency_matrix(lgraphs[[which.max(tgnumbers)]])
  # layout <- layout.fruchterman.reingold(glayout)
  # layout <- layout.davidson.harel(glayout)
  # layout <- layout.sugiyama(glayout)$layout
  # layout <- layout.kamada.kawai(glayout)
  layout <- layout.circle(glayout)
  radian.rescale <- function(x, start=0, direction=1) {
    c.rotate <- function(x) (x + start) %% (2 * pi) * direction
    c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
  }
  lab.locs <- radian.rescale(x=seq.int(ncol(lgraphs[[1]])), direction=-0.5, start=0)
  loaded()
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


  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    window <- input$window
    alpha <- input$alpha
    period <- input$period
    period_min <- period     
    print(period_min)
    period_max <- period + input$window -1 
    ggplot(data=dfm_m, aes(x=date, y=value, group=variable, color=variable))+geom_line()+facet_grid(variable ~ .)+ggtitle("raw time-series")+ geom_rect(aes(xmin=rvalues$ldates[period_min], xmax=rvalues$ldates[period_max], ymin=-Inf, ymax=Inf), color="black", alpha=0.00) + theme(legend.position="bottom")
  },
  height = 400, width = 600 )

  output$graphPlot <- renderPlot({
    window <- input$window
    alpha <- input$alpha
    period <- input$period
    label <- paste(rvalues$ldates[input$period], "to", rvalues$ldates[input$period + input$window - 1]) 
    graph <- graph_from_adjacency_matrix(rvalues$lgraphs[[period]])
    plot.igraph(graph, layout=layout, vertex.size=2, vertex.label.dist=1, vertex.label.degree=lab.locs, main=paste(label, ",  class: ", rvalues$lgnumbers[period], sep=""))
  },
  height = 400, width = 600)

  output$gnumberPlot <- renderPlot({
    window <- input$window
    alpha <- input$alpha
    period <- input$period
    label <- paste(rvalues$ldates[period], "to", rvalues$ldates[period + input$window -1]) 
    ggplot(data=rvalues$df_gnumbers, aes(x=date, y=value, group=variable, color=value)) + geom_line() + facet_grid(variable ~ .) + ggtitle("graph number") + geom_vline(aes(xintercept=as.numeric(rvalues$ldates[period])), color="black") + coord_cartesian(xlim = range(rvalues$ldates)) + theme(legend.position="bottom") + scale_color_gradient(low="blue", high="red")
  },
  height = 200, width = 600 )

  output$linkPlot <- renderPlot({
    window <- input$window
    alpha <- input$alpha
    period <- input$period
    ggplot(data=rvalues$alinks_m, aes(x=date, y=value, group=link)) + geom_step() + facet_wrap(~link) + ggtitle("links time-series") + geom_vline(aes(xintercept=as.numeric(rvalues$ldates[period])), color="red") + coord_cartesian(xlim = range(rvalues$ldates)) 
  },
  height = 600, width = 600 )
})

