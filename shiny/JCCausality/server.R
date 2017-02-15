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
source("saveloadprocess.R")

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
lldir <- c()

shinyServer(
function(input, output, session){
  # loading data and itialization
  rvalues <- reactiveValues() 

  observe({
    if(input$window < 70) updateNumericInput("window", value = 70, session=session)
    if(input$window > 420) updateNumericInput("window", value = 420, session=session)
  })
  
  compute_graphs <- reactive({
    input$compute
    print("iaminside")
    isolate({
      # do expensive stuff here and return the result
      loading()
      print(input$window)
      print(input$alpha)
      sol <- saveorload(input$window, input$alpha)
      rvalues$ldates <- sol$ldates
      rvalues$ldir <- sol$ldir
      rvalues$tarfile <- sol$tarfile
      lldir <<- c(lldir, rvalues$ldir, rvalues$tarfile)
      label <- paste(rvalues$ldates[input$period], "to", rvalues$ldates[input$period + input$window - 1]) 
      updateSliderInput(session, "period", min = 1 , max = length(rvalues$ldates) - input$window + 1, label = label)
      loaded()
      rvalues
    })
  })

  observe({ rvalues <- compute_graphs()})

  observe({
    label <- paste(rvalues$ldates[input$period], "to", rvalues$ldates[input$period + input$window - 1]) 
    updateSliderInput(session, "period", min = 1 , max = length(rvalues$ldates) - input$window + 1, label = label)
  }) 


  observeEvent(input$nright, {
    isolate(updateSliderInput(session, "period", value = input$period + 1))
  }) 

  observeEvent(input$nleft, {
    isolate(updateSliderInput(session, "period", value = input$period - 1))
  })

  output$distPlot <- renderImage({
    filename <- paste(rvalues$ldir, "/distPlot_", input$period, ".png", sep="")
    print(filename)
    print(file.exists(filename))
    list(src=filename, width=600, height=400)
  }, deleteFile = FALSE)

  output$graphPlot <- renderImage({
    filename <- paste(rvalues$ldir, "/graphPlot_", input$period, ".png", sep="")
    print(filename)
    print(file.exists(filename))
    list(src=filename, width=400, height=400)
  }, deleteFile = FALSE)

  output$gnumberPlot <- renderImage({
    filename <- paste(rvalues$ldir, "/gnumberPlot_", input$period, ".png", sep="")
    print(filename)
    print(file.exists(filename))
    list(src=filename, width=600, height=200)
  }, deleteFile = FALSE)

  output$linkPlot <- renderImage({
    filename <- paste(rvalues$ldir, "/linkPlot_", input$period, ".png", sep="")
    print(filename)
    print(file.exists(filename))
    list(src=filename, width=600, height=600)
  }, deleteFile = FALSE)
  
  session$onSessionEnded(function(){
    print("Ending Rshiny session")
    print("Removing local files")
    print(lldir)
    lapply(lldir, unlink, recursive=TRUE)
  })
})

