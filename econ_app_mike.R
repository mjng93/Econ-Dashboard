library(shiny)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(zoo)
library(quantmod)
library(TTR)
library(gridExtra)
library(grid)
library(DT)
library(kableExtra)
library(tables)
library(knitr)
library(rsconnect)
library(shinythemes)
library(shinydashboard)


source("econ_module.r", local = TRUE) 

ui <- shinyUI(
  fluidPage(theme=shinytheme('sandstone'),
            
            
            navbarPage("Economic and Business Data",
                       navbarMenu("Interactive",
                                  sandbox.UI(id="sandbox")
                       )
            )
  )
)


server <- function(input, output, session){
  
  callModule(sandbox.server,id="sandbox",data=fred)
  
}

shinyApp(ui = ui, server = server)
