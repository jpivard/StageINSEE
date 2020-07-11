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
 df2_shiny = read_csv(file='données/Emissions par secteurs pour Shiny.csv')
 
# 
# #Impossible de lui faire comprendre que la première colonne ne doit pas être reconnue comme des nombres
# #Pourtant j'ai tout fait comme dans la base de données WorldPhones et j'ai tout essayé dans le bloc_notes et l'Excel...
# 
# Autre piste
 
 df2_shiny <- df2_shiny[,-1]
 
# # Use a fluid Bootstrap layout
# ui <- fluidPage(    
#     
#     # Give the page a title
#     titlePanel("Emissions de CO2 par secteur"),
#     
#     # Generate a row with a sidebar
#     sidebarLayout(      
#         
#         # Define the sidebar with one input
#         sidebarPanel(
#             selectInput("secteur", "Secteur:", 
#                         choices=colnames(df2[,c(2:5)])),
#             hr(),
#             helpText("Données du dernier rapport Secten par le Citepa.")
#         ),
#         
#         # Create a spot for the barplot
#         mainPanel(
#             plotOutput("Courbe_secteurs")  
#         )
#         
#     )
# )
# 
# # Define a server for the Shiny app
# # server <- function(input, output) {
# #     
# #     # Fill in the spot we created for a plot
# #     output$Courbe_secteurs <- renderPlot({
# #         
#         # Render a barplot
#         
#         # secteur = colnames(df2[,c(2:5)])
#         # 
#         # if  (secteur == "Energie")
#         #     
#         # {barplot(df2$Energie, 
#         #         main=input$Energie,
#         #         ylab="Emissions en Mtoe",
#         #         xlab="Année") }
#         # 
#         # else (secteur == "Ind_manuf")
#         # 
#         # {barplot(df2$Ind_manuf, 
#         #         main=input$Ind_manuf,
#         #         ylab="Emissions en Mtoe",
#         #         xlab="Année") }
#         # 
#         # else (secteur == "Residentiel_Tertiaire")
#         # 
#         # {barplot(df2$Residentiel_Tertiaire, 
#         #         main=input$Residentiel_Tertiaire,
#         #         ylab="Emissions en Mtoe",
#         #         xlab="Année") }
#         # 
#         # 
#         # else (secteur == "Transports")
#         # 
#         # {barplot(df2$Transports, 
#         #         main=input$Transports,
#         #         ylab="Emissions en Mtoe",
#         #         xlab="Année")}
# 
#         
#         # Define a server for the Shiny app
#         server <- function(input, output) {
#             
#             # Fill in the spot we created for a plot
#             output$Courbe_secteurs <- renderPlot({
#         
#         barplot(df2$Energie, 
#                 main=input$Energie,
#                 ylab="Emissions en Mtoe",
#                 xlab="Année")
#         
#     })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)



 #Use a fluid Bootstrap layout
 ui <- fluidPage(

     # Give the page a title
     titlePanel("Emissions de CO2 par secteur"),

     # Generate a row with a sidebar
     sidebarLayout(

         # Define the sidebar with one input
         sidebarPanel(
             selectInput("secteur", "Secteur:",
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
                                  xlab="Année")
          
              })
          }

          # Run the application
          shinyApp(ui = ui, server = server)
          
          


