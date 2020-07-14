#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

df_kaya <- readRDS("KAYA identity, France, 1980-2015 (in base 100).rds")

#Je n'arrive pas à transformer le CSV en RDS ou RDA.

#Puis penser à renommer les colonnes (cf noms choisis infra)

source("test_plot_Kaya.R")

# Define UI
ui <- fluidPage(
    titlePanel("Evolution des composantes des émissions de CO2 en France après 1980"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("Illustre les variations des grandeurs influant sur les émissions de CO2, reliées entre elles par l'équation de Kaya"),
            
            selectInput("var",
                        label = "Choisir une grandeur à observer",
                        choices = c("Contenu carbone de l'énergie",
                                    "Intensité énergétique du PIB",
                                    "PIB par tête",
                                    "Population"),
                        selected = "Contenu carbone de l'énergie"),
            
            sliderInput("range",
                        label = "Période observée:",
                        min = 1980, max = 2015, value = c(1980, 2015))
        ),
        
        
        mainPanel(plotOutput("graph"))
    )
)

# Define server logic 

server <- function(input, output) {
    
    output$graph <- renderPlot({
        
        data <- switch(input$var, 
                       "Contenu carbone de l'énergie" = df_kaya$carbone_energie,
                       "Intensité énergétique du PIB" = df_kaya$PIB_energie,
                       "PIB par tête" = df_kaya$PIB_par_tete,
                       "Population" = df_kaya$population )
        
        color <- switch(input$var,
                        "Contenu carbone de l'énergie" = "red",
                        "Intensité énergétique du PIB" = "blue",
                        "PIB par tête" = "orange",
                        "Population" = "black")
        
        legend <- switch (input$var, 
                          "Contenu carbone de l'énergie"= "Contenu carbone de l'énergie",
                          "Intensité énergétique du PIB"= "Intensité énergétique du PIB",
                          "PIB par tête" =  "PIB par tête",
                          "Population" = "Population")
        
        plot_kaya (var = data, color, legend, input$range[1], input$range[2])
        
    })
    
}

#Alternative

# server <- function(input, output) {
#     output$map <- renderPlot({
#         args <- switch(input$var,
#                        "Percent White" = list(counties$white, "darkgreen", "% White"),
#                        "Percent Black" = list(counties$black, "black", "% Black"),
#                        "Percent Hispanic" = list(counties$hispanic, "darkorange", "% Hispanic"),
#                        "Percent Asian" = list(counties$asian, "darkviolet", "% Asian"))
#         
#         args$min <- input$range[1]
#         args$max <- input$range[2]
#         
#         do.call(percent_map, args)
#     })
# }

# Run the application
shinyApp(ui = ui, server = server)


