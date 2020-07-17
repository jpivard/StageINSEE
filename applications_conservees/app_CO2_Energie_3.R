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
        fluidRow( column( width = 12,h4("Evolution des émissions globales et comparaison avec l'Allemagne", align = 'center'), highchartOutput('courbe_emissions')),
                  column( width = 12,h4("Evolution des intensités carbone du PIB et comparaison avec l'Allemagne", align = 'center'), highchartOutput('courbe_intensite_carbone')), 
                 column( width = 12,h4("Evolution des grandeurs reliées aux émissions de CO2", align = 'center'), highchartOutput('courbe_Kaya') )
        )
        
        
    ),
    

)

# Define server logic 
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
            hc_xAxis( categories = unique(df1$Annee) ) %>%
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
    
    
    
    
    df2 <- df3_past %>% mutate(annee=rep(seq(1990,2016),3))%>%
        pivot_wider(names_from =pays, values_from =valeur)
    
    output$courbe_intensite_carbone <-renderHighchart({
        highchart() %>%
            hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
            hc_chart(type = 'line') %>%
            hc_series( list(name = 'France', data =df2$FR, color='blue', marker = list(symbol = 'circle')),
                       list(name = 'Allemagne', data =df2$DE, color = 'red', marker = list(symbol = 'circle')),
                       list(name = 'Union Européenne', data =df2$EU, color = 'green', marker = list(symbol = 'circle') )  )  %>%
            hc_xAxis( categories = unique(df2$annee) ) %>%
            hc_yAxis( title = list(text = "en tonnes d'équivalent CO2 par dollar")  ) %>%
            hc_plotOptions(column = list(
                dataLabels = list(enabled = F),
                #stacking = "normal",
                enableMouseTracking = T ) 
            )%>%
            hc_tooltip(table = TRUE,
                       sort = TRUE,
                       pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                             " {series.name}: {point.y} tCO2eq/$"),
                       headerFormat = '<span style="font-size: 13px">Année {point.key}</span>'
            ) %>%
            hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = 000 )
        
    })
    
    
    df3 <- df_Kaya %>% select(-CO2)
    
    output$courbe_Kaya <-renderHighchart({
        highchart() %>%
            hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
            hc_chart(type = 'line') %>%
            hc_series( list(name = 'Intensité carbone énergie', data =df3$`CO2 per energy`, color='red', marker = list(symbol = 'circle')),
                       list(name = 'Intensité énergétique du PIB', data =df3$`Energy per GDP`, color = 'blue', marker = list(symbol = 'circle')),
                       list(name = 'PIB par tête', data =df3$`GDP per capita`, color = 'orange', marker = list(symbol = 'circle')),
                       list(name = 'Population', data =df3$Population, color = 'black', marker = list(symbol = 'circle') )  )  %>%
            hc_xAxis( categories = unique(df3$Annee) ) %>%
            hc_yAxis( title = list(text = "en base 100 année 1980")  ) %>%
            hc_plotOptions(column = list(
                dataLabels = list(enabled = F),
                #stacking = "normal",
                enableMouseTracking = T ) 
            )%>%
            hc_tooltip(table = TRUE,
                       sort = TRUE,
                       pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                             " {series.name}: {point.y}"),
                       headerFormat = '<span style="font-size: 13px">Année {point.key}</span>'
            ) %>%
            hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = 000 )
        
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)