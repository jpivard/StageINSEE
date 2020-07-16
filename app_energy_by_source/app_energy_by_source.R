#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

df3_shiny <- df7



# 
# # Define UI 
# ui <- fluidPage(
#     titlePanel("Evolution des consommations primaires d'énergie par sources en France depuis 1980"),
#     
#     sidebarLayout(
#         sidebarPanel(
#             helpText("Choisir une source d'énergie à observer.
#           L'information est issue de The Shift Data Portal."),
#             textInput("source", "Source", "Pétrole"),
#             
#             dateRangeInput("dates",
#                            "Date range",
#                            start = "1980",
#                            end = "2016"),
#             
#             br(),
#             br(),
#             
#         ),
#         
#         mainPanel(plotOutput("plot"))
#     )
# )
# 
# 
# # Define server logic 
# 
# server <- function(input, output) {
#     
#     dataInput <- reactive({
#         getSectors(input$colnames(df3_shiny),
#                    from = input$dates[1],
#                    to = input$dates[2],
#                    auto.assign = FALSE)
#         
#     })
#     
#     finalInput <- reactive({
#         if (!input$adjust) return (dataInput())
#         adjust(dataInput())
#         
#     })
#     
#     output$plot <- renderPlot({
#         
#         chartSeries(finalInput(), theme = chartTheme("white"),
#                     type = "line", TA = NULL)
#     })
#     
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)


# 
# library(ggplot2)
# library(Cairo)   # For nicer ggplot2 output when deployed on Linux
# 
# ui <- fluidPage(
#     # Some custom CSS for a smaller font for preformatted text
#     tags$head(
#         tags$style(HTML("
#       pre, table.table {
#         font-size: smaller;
#       }
#     "))
#     ),
#     
#     fluidRow(
#         column(width = 4, wellPanel(
#             radioButtons("plot_type", "Type de courbe",
#                          c("base", "ggplot2")
#             )
#         )),
#         column(width = 4,
#                # In a plotOutput, passing values for click, dblclick, hover, or brush
#                # will enable those interactions.
#                plotOutput("plot1", height = 350,
#                           # Equivalent to: click = clickOpts(id = "plot_click")
#                           click = "courbe_cliquer",
#                           dblclick = dblclickOpts(
#                               id = "courbe_dbcliquer"
#                           ),
#                           hover = hoverOpts(
#                               id = "courbe_survoler"
#                           ),
#                           brush = brushOpts(
#                               id = "courbe_balayer"
#                           )
#                )
#         )
#     ),
#     fluidRow(
#         column(width = 3,
#                verbatimTextOutput("clic_info")
#         ),
#         column(width = 3,
#                verbatimTextOutput("dbclic_info")
#         ),
#         column(width = 3,
#                verbatimTextOutput("survol_info")
#         ),
#         column(width = 3,
#                verbatimTextOutput("balai_info")
#         )
#     )
# )
# 
# 
# server <- function(input, output) {
#     output$plot1 <- renderPlot({
#         if (input$plot_type == "base") {
#             plot(df3_shiny$Oil, df3_shiny$Nuclear)
#         } else if (input$plot_type == "ggplot2") {
#             ggplot(df3_shiny, aes(Oil, Nuclear)) + geom_line()
#         }
#     })
#     
#     output$click_info <- renderPrint({
#         cat("stats courbe_cliquer:\n")
#         str(input$plot_click)
#     })
#     output$hover_info <- renderPrint({
#         cat("stats courbe_dbcliquer:\n")
#         str(input$plot_hover)
#     })
#     output$dblclick_info <- renderPrint({
#         cat("stats courbe_survoler:\n")
#         str(input$plot_dblclick)
#     })
#     output$brush_info <- renderPrint({
#         cat("stats courbe_balayer:\n")
#         str(input$plot_brush)
#     })
#     
# }
# 
# 
# shinyApp(ui, server)

