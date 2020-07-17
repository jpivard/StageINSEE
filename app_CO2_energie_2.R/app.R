#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(highcharter)


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Quelques données sur les émissions de CO2"),
    
    # Sidebar with a slider input for number of bins 
    # sidebarLayout(
    #     
    #     ),
    
    # Show a plot of the generated distribution
    mainPanel(
        
        h2(paste0("Les émissions de CO2 en France depuis 1980")),
        fluidRow( column( width = 6,h4("Evolution des émissions globales et comparaison avec l'Allemagne", align = 'center'), highchartOutput('courbe_emissions') )
                  # column( width = 6,h4("Evolution des intensités carbone du PIB et de l'énergie", align = 'center'), highchartOutput('') ),
                  # column( width = 6,h4("Evolution des grandeurs reliées aux émissions de CO2", align = 'center'), highchartOutput('') )
        )
        
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #Courbe sur les émissions globales
    
    df1<- df1_fr_all  %>% 
        filter(Annee %in% c(1980:2016))
    
    output$courbe_emissions <-renderHighchart({
        highchart() %>%
            hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
            hc_chart(type = 'line') %>%
            hc_series( list(name = 'France', data =df1$France, color='blue', marker = list(symbol = 'circle') ),
                       list(name = 'Allemagne', data =df1$Allemagne, color = 'red', marker = list(symbol = 'circle') )  )  %>%
            hc_xAxis( categories = unique(df1$annee) ) %>%
            hc_yAxis( title = list(text = "en méga-tonnes d'équivalent CO2")  ) %>%
            hc_plotOptions(column = list(
                dataLabels = list(enabled = F),
                #stacking = "normal",
                enableMouseTracking = T ) 
            )%>%
            hc_tooltip(table = TRUE,
                       sort = TRUE,
                       pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                             " {series.name}: {point.y} MtCO2eq"),
                       headerFormat = '<span style="font-size: 13px">Année {point.key}</span>'
            ) %>%
            hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = 000 )
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
