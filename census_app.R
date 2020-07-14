#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

install.packages('rdbnomics')
install.packages("dplyr")
install.packages("readr")


library(rdbnomics)
library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)



library(shiny)

Kaya <- readRDS("data/counties.rds")

source("helpers.R")

# Define UI
ui <- fluidPage(
    titlePanel("censusVis"),

    sidebarLayout(
        sidebarPanel(
            helpText("Create demographic maps with
               information from the 2010 US Census."),

            selectInput("var",
                        label = "Choose a variable to display",
                        choices = c("Percent White",
                                    "Percent Black",
                                    "Percent Hispanic",
                                    "Percent Asian"),
                        selected = "Percent White"),

            sliderInput("range",
                        label = "Range of interest:",
                        min = 0, max = 100, value = c(0, 100))
        ),

        # mainPanel(
        #     textOutput("selected_var"),
        #     textOutput("min_max")
        # )
        
        mainPanel(plotOutput("map"))
    )
)

# Define server logic required to draw a histogram
# server <- function(input, output) {

    # output$selected_var <- renderText({
    #     paste("You have selected", input$var)
    # })
    # 
    # output$min_max <- renderText({
    #     paste ("You have chosen a range that goes from",
    #            input$range[1], "to", input$range[2])
    # })
    
    server <- function(input, output) {
    
      output$map <- renderPlot({
        
        data <- switch(input$var, 
                       "Percent White" = counties$white,
                       "Percent Black" = counties$black,
                       "Percent Hispanic" = counties$hispanic,
                       "Percent Asian" = counties$asian)
        
        color <- switch(input$var,
                         "Percent White" = "darkgreen",
                         "Percent Black" = "black",
                         "Percent Hispanic" = "darkorange",
                         "Percent Asian" = "darkviolet")
        
        legend <- switch (input$var, 
                          "Percent White" = "% White",
                          "Percent Black" = "% Black",
                          "Percent Hispanic" = "% Hispanic",
                          "Percent Asian" = "% Asian")
        
        percent_map(var = data, color, legend, input$range[1], input$range[2])
        
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












