#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Define UI for application that draws a histogram
ui <- fluidPage( 
    titlePanel("My Shiny App"),
                 
     sidebarLayout( 
         sidebarPanel("sidebar panel"),
                    
                      mainPanel(                h1("First level title",align = "center"),
                                                h2("Second level title"),
                                                h3("Third level title"),
                                                h4("Fourth level title"),
                                                h5("Fifth level title"),
                                                h6("Sixth level title")
                                        
                    )
     )

)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
}

# Run the application 
shinyApp(ui = ui, server = server)
