#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# # Define UI for application that makes a button
# ui <- fluidPage(
# 
#     # Copy the line below to make an action button
#     actionButton("action", label = "Action"),
#     
#     hr(),
#     fluidRow(column(2, verbatimTextOutput("value")))
#     
#     
#    
#         )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     output$value <- renderPrint({ input$action })
#     
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)
