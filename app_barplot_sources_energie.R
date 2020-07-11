#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



#Ici il y a tout pour que ça fonctionne a priori, mais le problème est que les valeurs dans le tableau 
#sont beaucoup trop grandes.

library(shiny)

df_test_shiny <- df_test [,-c(6:11)]

 #Use a fluid Bootstrap layout
 ui <- fluidPage(

     # Give the page a title
     titlePanel("Consommation d'énergie par source (en Mtoe)"),

     # Generate a row with a sidebar
     sidebarLayout(

         # Define the sidebar with one input
         sidebarPanel(
             selectInput("source", "Source:",
                         choices=colnames(df_test_shiny)),
             hr(),
             helpText("Données du (insérer la source)")
         ),

         # Create a spot for the barplot
         mainPanel(
             plotOutput("Courbe_sources_energie")
         )

     )
 )

 # Define a server for the Shiny app
 server <- function(input, output) {
 
     # Fill in the spot we created for a plot
      output$Courbe_sources_energie<- renderPlot({
 
         # Render a barplot
         
          barplot(df_test_shiny[,input$source]*1000, 
                                  main=input$source,
                                  ylab="Production en Mtoe",
                                  xlab="Année")
          
              })
          }

          # Run the application
          shinyApp(ui = ui, server = server)
          
          


