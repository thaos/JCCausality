#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinyjs)

appCSS <- "
#loading_page {
position: absolute;
background: #000000;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #FFFFFF;
}
"
# Define UI for application that draws a histogram
shinyUI(fluidPage(
   
  # Application title
  titlePanel("Granger Graphs for 10-year Periods"),
  includeScript("leftright.js"),
  inlineCSS(appCSS),
  useShinyjs(),
  div(id = "loading_page", h1("Loading...")),
  hidden(div(id = "main_content",
  column(3, align= "center",
	 fluidRow(sliderInput("period", "start of the period", min = 1, max = 420, value = 1,
			    animate = animationOptions(interval = 1000, playButton = icon('play', "fa-3x"), pauseButton = icon('pause', "fa-3x"))
			    )),
	 # replace by numericInput
	 #      numericInput("num", label = h3("Numeric input"), value = 1))
	 fluidRow(numericInput("window", "window length", min = 70, max = 420, value = 120, step = 1)),
	 fluidRow(numericInput("alpha", "significance level", min = 0, max = 1, value = 0.01, step = 0.01)),
	 fluidRow(actionButton("compute", "Compute")),
	 fluidRow(plotOutput("graphPlot"))),
  column(4, align = "center",
	 fluidRow(plotOutput("distPlot")),
	 fluidRow(plotOutput("gnumberPlot"))),
  column(4, align = "center",
	 fluidRow(plotOutput("linkPlot")))
  ))
  # Loading message
  #   div(id = "loading-content", h2("Loading...")),
  # The main app code goes here
  #   verbatimTextOutput("results"),
  ))
