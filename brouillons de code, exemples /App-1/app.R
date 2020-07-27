#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#runExample("01_hello") Modèle d'application avec histogramme

# Define UI for application that draws a histogram 
ui <- fluidPage(

    # Application title
    titlePanel("Consommation d'électricité"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("period",
                        "Nombre d'années entre 1960 et 2014:",
                        min = 1,
                        max = 55,
                        value = 10)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate years based on input$years from ui.R
        x    <- df2$consommation_electricite_fr
        years <- seq(min(x), max(x), length.out = input$period + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = years, col = '#75AADB', border = 'orange',
        xlab = "Consommation en TOE",
        main = "Histogramme sur la consommation d'électricité en France")
    })
}

# Run the application
shinyApp(ui = ui, server = server)

