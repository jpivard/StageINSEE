#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# library(datasets)
# view(WorldPhones)
# 
# #J'ai trouvé la cause du problème : dans le tableau df2, les années ne devraient pas être une variable...
# 
 # df2_shiny = read_csv(file='~/données/Emissions par secteurs pour Shiny.csv')
 # 
 # rownames(df2_shiny)<-df2_shiny$X1

 df2_shiny = read.csv(file='~/données/Emissions par secteurs pour Shiny.csv', header =TRUE, row.names = 1)



 #Use a fluid Bootstrap layout
 ui <- fluidPage(

     # Give the page a title
     titlePanel("Emissions de CO2 par secteur en France depuis 1990"),

     # Generate a row with a sidebar
     sidebarLayout(

         # Define the sidebar with one input
         sidebarPanel(
             selectInput("secteur", "Choisissez un secteur à observer:",
                         choices=colnames(df2_shiny)),
             hr(),
             helpText("Données du dernier rapport Secten par le Citepa.")
         ),

         # Create a spot for the barplot
         mainPanel(
             plotOutput("Courbe_secteurs")
         )

     )
 )

 # Define a server for the Shiny app
 server <- function(input, output) {
 
     # Fill in the spot we created for a plot
      output$Courbe_secteurs <- renderPlot({
 
         # Render a barplot
          
          secteur=c("Energie","Ind_manuf","Residentiel_Tertiaire","Transports")
         
          barplot(df2_shiny[,input$secteur]*1000, 
                                  main=input$secteur,
                                  ylab="Emissions en Mtoe",
                                  xlab="Année",
                                   )
          
              })
          }

          # Run the application
          shinyApp(ui = ui, server = server)
          
          


