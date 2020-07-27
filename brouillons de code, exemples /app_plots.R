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
library(dplyr)

df5 = read_tsv(file='~/données/KAYA identity, France, 1980-2015 (in base 100).csv')

dataset <- df5

df5 <- df5[,-1]
colnames(df5)[2]<- 'Contenu_carbone_energie'
colnames(df5)[3]<- 'Intensite_energetique_PIB'
colnames(df5)[4]<- 'PIB_par_tete'

# Define UI 
ui <-fluidPage ( title = "Evolution des composantes des émissions de dioxyde de carbone en France après 1980",
                    
                plotOutput('plot'),
                    
                hr(),
                    
     fluidRow(
         column(5,
         h4("Evolution des composantes des émissions de dioxyde de carbone en France après 1980"),
         sliderInput('periode_observee', 'Période observée', 
                      min=min(df5$Annee), max=max(df5$Annee),
                      value=c(1980,2015)), 
         
        br(),
                             
        ),
                        
        column(5,
            selectInput('y', 'Y', c("Contenu_carbone_energie","Population","Intensite_energetique_PIB","PIB_par_tete") ),
                      
                    ),
     ),
     
)
     
#Ajouter une note sur l'équation de Kaya


# Define server logic 
server <- function(input, output) {
    
    dataset <- reactive({
      df5[input$Annee]
    })
    
    output$plot <- renderPlot({
        
        p <- ggplot(dataset(), aes_string(x=input$periode_observee, y=input$y, color=colnames(df5))) + geom_line() +
            # scale_color_manual(values = c("red","blue","orange","black"),labels = c("Contenu en CO2 de l'énergie", "Intensité énergétique du PIB", "PIB par tête", "Population"))+
            # labs(title="Evolution des composantes des émissions de CO2 \n  en France après 1980 ", x="Année", y="base 100")+
            # theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))
        
        print(p)
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
