#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Granger Graphs for 10-year Periods"),
  includeScript("leftright.js"),
  verbatimTextOutput("results"),
column(3, align= "center",
	 fluidRow(sliderInput("period",
			    "start of the period",
			    min = 1,
			    max = 420,
			    value = 1,
			    animate = animationOptions(interval = 2500, playButton = icon('play', "fa-3x"), pauseButton = icon('pause', "fa-3x"))
			    #                    min = as.Date("1990-01-01"),
			    #                    max = as.Date("2015-01-01"),
			    #                    value = as.Date("2000-01-01"),
			    #                    timeFormat = "%F"
			    )),
	   fluidRow(plotOutput("graphPlot"))),
  column(6, align = "center",
	 fluidRow(plotOutput("distPlot")),
	 fluidRow(plotOutput("gnumberPlot")))
))
