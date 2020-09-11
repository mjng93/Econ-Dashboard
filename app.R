source("econ_data.r", local=T)
source("econ_module.r", local=T) 


library(rsconnect)
library(shinythemes)
library(shinydashboard)

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
