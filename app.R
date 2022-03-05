source("econ_data.R", local=T)
source("econ_module.R", local=T) 
source("econ_module2.R", local=T) 


library(rsconnect)
library(shinythemes)
library(shinydashboard)

ui <- shinyUI(
  fluidPage(theme=shinytheme('cyborg'),
            
            
           # tags$head(includeHTML("google_analytics_econ_biz.html")),
            
            navbarPage("Economic and Business Data",
                       navbarMenu("Interactive",
                                  sandbox.UI(id="sandbox")
                       ),
                       navbarMenu("Financial Data",
                                  sandbox2.UI(id="sandbox2")
                       )
            )
  )
)


server <- function(input, output, session){
  
  callModule(sandbox.server,id="sandbox",data=fred)
  callModule(sandbox.server2,id="sandbox2",data=all_prices)
  
}

shinyApp(ui = ui, server = server)
