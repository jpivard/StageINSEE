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
ui <- bootstrapPage(
    
    selectInput(inputId = "n_breaks",
                label = "Nombre d'années observées depuis 1990 :",
                choices = c(10, 20, 30),
                selected = 20),
    
    checkboxInput(inputId = "individual_obs",
                  label = strong("Voir les observations individuelles"),
                  value = FALSE),
    
    checkboxInput(inputId = "density",
                  label = strong("Voir la densité estimée"),
                  value = FALSE),
    
    plotOutput(outputId = "main_plot", height = "300px"),
    
    # Display this only if the density is shown
    conditionalPanel(condition = "input.density == true",
                     sliderInput(inputId = "bw_adjust",
                                 label = "Ajustement de la largeur de la bande:",
                                 min = 0.2, max = 2, value = 1, step = 0.2)
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$main_plot <- renderPlot({
        
        hist(df2$Energie,
             probability = TRUE,
             breaks = as.numeric(input$n_breaks),
             xlab = "Emissions de CO2(en Mt)",
             main = "Emissions annuelles de CO2 par le secteur de l'énergie")
        
        if (input$individual_obs) {
            rug(df2$Energie)
        }
        
        if (input$density) {
            dens <- density(df2$Energie,
                            adjust = input$bw_adjust)
            lines(dens, col = "green")
        }
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
