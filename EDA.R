install.packages("shiny")
install.packages("shinydashboard")
install.packages("shinythemes")
install.packages("DT")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("treemapify")
install.packages("readr")
install.packages("RColorBrewer")
library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)
library(ggplot2)
library(dplyr)
library(treemapify)
library(readr)
library(RColorBrewer)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

server <- function(input, output){}

shinyApp(ui, server)