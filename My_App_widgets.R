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
    
    titlePanel("censusVis"),
    
    sidebarLayout(
        sidebarPanel( 
        helpText("Create demographic maps with 
                 information from the 2010 US Census."),
       
        selectInput("var", 
                    label = "Choose a variable to display",
                    choices = list("Percent White", 
                                   "Percent Black",
                                   "Percent Hispanic", 
                                   "Percent Asian"),
                    selected = "Percent White"),
        
        sliderInput("range", 
                    label = "Range of interest:",
                    min = 0, max = 100, value = c(0, 100))
        
    ),
       
    mainPanel()
    
   )
  
 )

# Define server logic required to draw a histogram
server <- function(input, output) {

   
}

# Run the application 
shinyApp(ui = ui, server = server)
