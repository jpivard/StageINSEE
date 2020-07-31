#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)


# Define UI for application that draws a histogram
ui <-fluidPage (title = "Emissions par secteurs en France après 1990",
                 
                 plotOutput('plot'),
                 
                 hr(),
                 
                 fluidRow(
                     column(5,
                            h4("Emissions par secteurs en France après 1990"),
                            sliderInput('periode_observee', 'Période observée', 
                                        min=min(df2$Annee), max=max(df2$Annee),
                                        value=c(1990,2000)), 
                            
                            br(),
                            
                     ),
                     
                     column(5,
                            selectInput('y', 'Y', c("Energie","Ind_manuf","Residentiel_Tertiaire","Transports") ),
                            
                     ),
                 ),
                
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    dataset <- reactive({
       df2[ nrow(df2), input$Annee ]
    })
    
    output$plot <- renderPlot({
        
        p <- ggplot(dataset(), aes_string(x=input$Annee, y=input$y)) + geom_line()
        # 
        # if (input$y == "Energie")
        #     p <- p + aes_string(color=input$color)
        # 
        # facets <- paste(input$facet_row, '~', input$facet_col)
        # if (facets != '. ~ .')
        #     p <- p + facet_grid(facets)
        # 
        # if (input$jitter)
        #     p <- p + geom_jitter()
        # if (input$smooth)
        #     p <- p + geom_smooth()
        
        print(p)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
