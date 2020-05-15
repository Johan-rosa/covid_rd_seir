library(shiny)
library(tidyverse)

rcero <- readRDS("rcero.RDS")

fluidPage(
    sidebarPanel(
        selectInput(
            "provincia", "Provincia",
            choices = unique(rcero$provincia),
            selected = "República Dominicana",
            multiple = TRUE)
    ),
    mainPanel(
        h2("Evolución del Factor de contagio"),
        plotly::plotlyOutput("rceroPlot")
    )
  
)

