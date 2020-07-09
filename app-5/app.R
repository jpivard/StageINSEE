#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# runExample("06_tabsets") 


#Celle-ci ne fonctionne pas pour le moment!
#Est-ce forcément des distributions générées aléatoirement ?



# Define UI for random distribution app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Données sur la consommation et la production d'énergies renouvelables en France et en Allemagne"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select the  distribution type ----
            radioButtons("dist", "Distribution type:",
                         c("Consommation en France" = "conso_fr",
                           "Consommation en Allemagne" = "conso_all",
                           "Production en France" = "prod_fr",
                           "Production en Allemgne" = "prod_all")),
            
            # br() element to introduce extra vertical spacing ----
            br(),
            
            # Input: Slider for the number of observations to generate ----
            sliderInput("n",
                        "Nombre d'années observées entre 1990 et 2016:",
                        value = 5,
                        min = 1,
                        max = 27)
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Courbe", plotOutput("plot")),
                        tabPanel("Résumé", verbatimTextOutput("summary")),
                        tabPanel("Tableau", tableOutput("table"))
            )
            
        )
    )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
    
    # Reactive expression to generate the requested distribution ----
    # This is called whenever the inputs change. The output functions
    # defined below then use the value computed from this expression
    d <- reactive({
        dist <- switch(input$df_groupe,
                       conso_fr= df_groupe$France.x,
                       conso_all= df_groupe$Allemagne.x,
                       prod_fr = df_groupe$France.y,
                       prod_all = df_groupe$Allemagne.y,
                       )
        
    })
    
    # Generate a plot of the data ----
    # Also uses the inputs to build the plot label. Note that the
    # dependencies on the inputs and the data reactive expression are
    # both tracked, and all expressions are called in the sequence
    # implied by the dependency graph.
    output$plot <- renderPlot({
        dist <- input$df_groupe
        n <- input$n
        
        hist(d(),
             main = paste("r", dist, "(", n, ")", sep = ""),
             col = "green", border = "white")
    })
    
    # Generate a summary of the data ----
    output$summary <- renderPrint({
        summary(d())
    })
    
    # Generate an HTML table view of the data ----
    output$table <- renderTable({
        d()
    })
    
}

# Create Shiny app ----
shinyApp(ui, server)

