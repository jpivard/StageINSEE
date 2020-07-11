#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



# runExample("05_sliders")

library(shiny)

# Define UI for slider demo app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Test Curseurs"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar to demonstrate various slider options ----
        sidebarPanel(
            
            # Input: Simple integer interval ----
            sliderInput("integer", "Année",
                        min = 1960, max = 2020,
                        value = 2000),
            
            # Input: Decimal interval with step value ----
            sliderInput("decimal", "Taux d'intérêt",
                        min = 0, max = 3,
                        value = 0.5, step = 0.1),
            
            # Input: Specification of range within an interval ----
            sliderInput("range", "Somme investie dans la R&D verte:",
                        min = 1, max = 100000,
                        value = c(500,5000)),
        
            
            # Input: Animation with custom interval (in ms) ----
            # to control speed, plus looping
            sliderInput("animation", "Nombre d'obligations vertes émises en France:",
                        min = 1, max = 2000,
                        value = 1, step = 10,
                        animate =
                            animationOptions(interval = 300, loop = TRUE)),
            
            # Input: Custom currency format for with basic animation ----
            sliderInput("format", "Montant total des investissements verts en dollars:",
                        min = 0, max = 100000,
                        value = 0, step = 5000,
                        pre = "$", sep = ",",
                        animate = TRUE),
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Table summarizing the values entered ----
            tableOutput("values")
            
        )
    )
)

# Define server logic for slider examples ----
server <- function(input, output) {
    
    # Reactive expression to create data frame of all input values ----
    sliderValues <- reactive({
        
        data.frame(
            Name = c("Année",
                     "Taux d'intérêt",
                     "Somme investie dans la R&D verte:",
                     "Nombre d'obligations vertes émises",
                     "Montant total des investissements verts en dollars:"),
            Value = as.character(c(input$integer,
                                   input$decimal,
                                   paste(input$range, collapse = " "),
                                   input$animation,
                                   input$format)),
                                    
            stringsAsFactors = FALSE)
        
    })
    
    # Show the values in an HTML table ----
    output$values <- renderTable({
        sliderValues()
    })
    
}

# Create Shiny app ----
shinyApp(ui, server)



