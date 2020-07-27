#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# runExample("02_text")

# Define UI for dataset viewer app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Quelques données sur la production et la consommation d'énergie renouvelable entre 1990 et 2016"),
    
    # Sidebar layout with a input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Selector for choosing dataset ----
            selectInput(inputId = "df_groupe", 
                        label = "Choisis un jeu de données:",
                        choices = c("Production en France", "Production en Allemagne", "Consommation en France", "Consommation en Allemagne")),
            
            # Input: Numeric entry for number of obs to view ----
            numericInput(inputId = "annee",
                         label = "Nombre d'années observées",
                         value = 10),
            
            # Include clarifying text ----
            helpText("Note:même si l'aperçu des données ne montrera que le nombre d'observations spécifié,",
                     "le résumé sera toujours fondé sur le jeu de données tout entier."),
            
            # Input: actionButton() to defer the rendering of output ----
            # until the user explicitly clicks the button (rather than
            # doing it immediately when inputs change). This is useful if
            # the computations required to render output are inordinately
            # time-consuming.
            actionButton("update", "Mettre à jour")
            
        
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Verbatim text for data summary ----
            verbatimTextOutput("summary"),
            
            # Output: HTML table with requested number of observations ----
            tableOutput("view")
            
        )
    )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
    
    # Return the requested dataset ----
    datasetInput <- reactive({
        switch(input$df_groupe,
               "Consommation en France" = df_groupe$France.x ,
               "Consommation en Allemagne" = df_groupe$Allemagne.x,
               "Production en France" = df_groupe$France.y,
               "Production en Allemagne" = df_groupe$Allemagne.y)
    })
    
    # Generate a summary of the dataset ----
    output$summary <- renderPrint({
        dataset <- datasetInput()
        summary(dataset)
    })
    
    # Show the first "n" observations ----
    output$view <- renderTable({
        head(datasetInput(), n = input$annee)
    })
    
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

