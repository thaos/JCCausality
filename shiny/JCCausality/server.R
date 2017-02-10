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
library(vars)
dfm_m <- readRDS("dfm_m.rds")
ina <- which(apply(dfm_m, 1, function(x) any(is.na(x))))
if(length(ina) > 0) dfm_m <- dfm_m[-ina, ]
gnumber <- readRDS("gnumber_a0.01.rds")
rlggraphs <- readRDS("rlggraphs_a0.01.rds")
tsplot <- ggplot(data=dfm_m, aes(x=date, y=value, group=variable, color=variable))+geom_line()+facet_grid(variable ~ .)+ggtitle("raw time-series")
# Define server logic required to draw a histogram
ldates <- sort(unique(dfm_m$date))
# max_slider <- max(ldates)
# year(max_slider)  <- year(max_slider) - 10
dfg <- data.frame(date=ldates[seq_along(gnumber)], variable="gnumber", value=gnumber)
tgnumber <- table(gnumber)
glayout <- graph_from_adjacency_matrix(rlggraphs[[which.max(tgnumber)]])
# layout <- layout.fruchterman.reingold(glayout)
# layout <- layout.davidson.harel(glayout)
# layout <- layout.sugiyama(glayout)$layout
# layout <- layout.kamada.kawai(glayout)
layout <- layout.circle(glayout)
radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}
lab.locs <- radian.rescale(x=seq.int(ncol(rlggraphs[[1]])), direction=-0.5, start=0)

shinyServer(
function(input, output, session) {
  observe({
    # Step size is 2 when input value is even; 1 when value is odd.
    label <- paste(ldates[input$period], "to", ldates[input$period + 119]) 
    updateSliderInput(session, "period", min = 1 , max = length(ldates) - 120, label = label)
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
    x    <- faithful[, 2] 
    period <- input$period
    period_min <- period     
    print(period_min)
    period_max <- period + 119
    #     year(period_max) <- year(period_max) + 10
    #     print(period_max)
    #     print(data.frame(period_min=period_min, period_max=period_max))
    # draw the histogram with the specified number of bins
    #     plot(tsplot + geom_rect(aes(xmin=period_min, xmax=period_max, ymin=0, ymax=Inf)))
    ggplot(data=dfm_m, aes(x=date, y=value, group=variable, color=variable))+geom_line()+facet_grid(variable ~ .)+ggtitle("raw time-series")+ geom_rect(aes(xmin=ldates[period_min], xmax=ldates[period_max], ymin=-Inf, ymax=Inf), color="black", alpha=0.00) + theme(legend.position="bottom")
  },
  height = 400, width = 600 )

  output$graphPlot <- renderPlot({
    period <- input$period
    label <- paste(ldates[input$period], "to", ldates[input$period + 119]) 
    graph <- graph_from_adjacency_matrix(rlggraphs[[period]])
    #     plot.igraph(graph, layout=layout)
    plot.igraph(graph, layout=layout, vertex.size=2, vertex.label.dist=1, vertex.label.degree=lab.locs, main=paste(label, ",  class: ", gnumber[period], sep=""))
    #     plot(as(rlggraphs[[period]], "graphNEL"), main=paste(label, ",  class: ", gnumber[period], sep=""))
    #     plot(as(rlggraphs[[period]], "graphNEL"), main=paste(label, ",  class: ", gnumber[period], sep=""))
  },
  height = 400, width = 600)

  output$gnumberPlot <- renderPlot({
    period <- input$period
    label <- paste(ldates[period], "to", ldates[period + 119]) 
    ggplot(data=dfg, aes(x=date, y=gnumber, group=variable, color=gnumber)) + geom_line() + facet_grid(variable ~ .) + ggtitle("graph number") + geom_vline(aes(xintercept=as.numeric(ldates[period])), color=gnumber) + coord_cartesian(xlim = range(ldates)) + theme(legend.position="bottom") + scale_color_gradient(low="blue", high="red")
    # Diverging color scheme+ scale_color_gradientn(colours = rainbow(5))
  },
  height = 200, width = 600 )

  
})

# shinyServer(function(input, output) {
#    
#   output$distPlot <- renderPlot({
#     
#     x    <- faithful[, 2] 
#     period <- input$period
#     period_min <- ldates[period]     
#     print(period_min)
#     period_max <- ldates[period+119] 
#     print(period_max)
#     ggplot(data=dfm_m, aes(x=date, y=value, group=variable, color=variable))+geom_line()+facet_grid(variable ~ .)+ggtitle("raw time-series")+ geom_rect(aes(xmin=period_min, xmax=period_max, ymin=-Inf, ymax=Inf, fill=variable), color="black", alpha=0.00)
#   })
#   
# })
