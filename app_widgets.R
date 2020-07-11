#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

#Ce fichier répertorie quelques exemples de widgets déjà construits. Il suffit de décommenter et d'éxecuter la partie qu'on veut tester.




# 1. Action button

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


#2. Checkbox group

# ui <- fluidPage(
#   
#   # Copy the chunk below to make a group of checkboxes
#   checkboxGroupInput("checkGroup", label = h3("Secteur"), 
#                      choices = list("Transport" = 1, "Bâtiment" = 2, "Energie" = 3),
#                      selected = 1),
#   
#   
#   hr(),
#   fluidRow(column(3, verbatimTextOutput("value")))
#   
# )
# 
# server <- function(input, output) {
#   
#   # You can access the values of the widget (as a vector)
#   # with input$checkGroup, e.g.
#   output$value <- renderPrint({ input$checkGroup })
#   
# }
# 
# shinyApp(ui = ui, server = server)


#3.Date input

# ui <- fluidPage(
#   
#   # Copy the line below to make a date selector 
#   dateInput("date", label = h3("Date input"), value = "2014-01-01"),
#   
#   hr(),
#   fluidRow(column(3, verbatimTextOutput("value")))
#   
# )
# 
# server <- function(input, output) {
#   
#   # You can access the value of the widget with input$date, e.g.
#   output$value <- renderPrint({ input$date })
#   
# }
# 
# shinyApp(ui = ui, server = server)


#4. Date range

# ui <- fluidPage(
#   
#   # Copy the line below to make a date range selector
#   dateRangeInput("dates", label = h3("Date range")),
#   
#   hr(),
#   fluidRow(column(4, verbatimTextOutput("value")))
#   
# )
# 
# server <- function(input, output) {
#   
#   # You can access the values of the widget (as a vector of Dates)
#   # with input$dates, e.g.
#   output$value <- renderPrint({ input$dates })
#   
# }
# 
# shinyApp(ui = ui, server = server)


# #5. File upload
# 
# ui <- fluidPage(
#   
#   # Copy the line below to make a file upload manager
#   fileInput("file", label = h3("File input")),
#   
#   hr(),
#   fluidRow(column(4, verbatimTextOutput("value")))
#   
# )
# 
# server <- function(input, output) {
#   
#   # You can access the value of the widget with input$file, e.g.
#   output$value <- renderPrint({
#     str(input$file)
#   })
#   
# }
# 
# shinyApp(ui = ui, server = server)
# 

# #6. Text input
# 
# ui <- fluidPage(
#   
#   # Copy the line below to make a text input box
#   textInput("text", label = h3("saisie de texte"), value = "Entrez votre texte..."),
#   
#   hr(),
#   fluidRow(column(3, verbatimTextOutput("value")))
#   
# )
# 
# server <- function(input, output) {
#   
#   # You can access the value of the widget with input$text, e.g.
#   output$value <- renderPrint({ input$text })
#   
# }
# 
# shinyApp(ui = ui, server = server)


