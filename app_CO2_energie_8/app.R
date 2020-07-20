

install.packages("highcharter")
install.packages("plotly")

library(shiny)
library(highcharter)
library(dplyr)
library(tidyr)
library(readr)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Quelques données sur les émissions de CO2 et l'énergie en France"),
    
    # Sidebar with a slider input for number of bins 
    # sidebarLayout(
    #     
    #     ),
    
    # Show a plot of the generated distribution
    mainPanel(
        
        h2(paste0("Les émissions de CO2 en France depuis 1980")),
        fluidRow( column( width = 12,h4("Evolution des émissions globales et comparaison avec l'Allemagne", align = 'center'), highchartOutput('courbe_emissions')),
                  column( width = 12,h4("Répartition des émissions par secteurs", align = 'center'), plotlyOutput('emissions_secteurs')),
                  column( width = 12,h4("Evolution de l'intensité carbone du PIB", align = 'center'), highchartOutput('courbe_intensite_carbone')),
                  column( width = 12,h4("Evolution de l'intensité carbone de l'énergie",align='center'),highchartOutput('courbe_intensite_carbone_energie'),
                          column( width = 12,h4("Evolution des grandeurs reliées aux émissions de CO2", align = 'center'), highchartOutput('courbe_Kaya')))
        ),
        
        h2(paste0("L'énergie en France depuis 1980")),
        fluidRow( column (width = 12,h4("Comparaison de la consommation d'énergies renouvelables et fossiles", align = 'center'), plotlyOutput('conso_ER_plot')),
                  column (width = 12,h4("Comparaison de la production d'énergie nucléaire en France et en Allemagne", align = 'center'), plotlyOutput('nucl_plot')),
                  column (width = 12,h4("Comparaison de la distribution des niveaux de consommation d'énergies renouvelables en France et en Allemagne", align = 'center'), plotlyOutput('conso_ER_Fr_All_plot')),
                  column (width = 12,h4("Comparaison de la distribution des niveaux de production d'énergies renouvelables en France et en Allemagne", align = 'center'), plotlyOutput('prod_ER_Fr_All_plot')),
                  column (width = 12,h4("Evolution de la part des énergies renouvables dans la consommation finale en France et dans d'autres pays européens", align = 'center'), plotlyOutput('part_conso_ER_plot')),
                  column (width = 12,h4("Evolution de la part des énergies renouvables dans la production primaire en France et dans d'autres pays européens", align = 'center'), plotlyOutput('part_prod_ER_plot'))
        )
        
        
        
        
    ),
    
    
)

# Define server logic 
server <- function(input, output) {
    
    #1. CO2
    
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
    
    
    #Figure sur la répartition des émissions par secteurs
    
    df7 <- df2_long
    
    output$emissions_secteurs <- renderPlotly({
        emissions_secteurs <- ggplot(df7, aes(x=Annee,y= value)) +
            geom_bar(aes (x=Annee, y =value, fill=secteur),stat = "identity", position = "stack")+
            scale_fill_manual(values = c("green","brown","orange","blue"),labels = c("Industrie de l'energie","Industrie manufacturiere et construction", "Residentiel et Tertiaire", "Transports"))+
            labs(x="Année",y="Mtoe")+
            theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))+
            guides(fill=guide_legend(nrow=2,byrow=TRUE))
        
        
    })
    
    #Courbe sur l'intensité carbone du PIB
    
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
    
    #Courbe sur l'intensité carbone de l'énergie
    
    df4 <- df4_past %>% mutate(annee=rep(seq(1990,2015),3))%>%
        pivot_wider(names_from =pays, values_from =valeur)
    
    output$courbe_intensite_carbone_energie <-renderHighchart({
        highchart() %>%
            hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
            hc_chart(type = 'line') %>%
            hc_series( list(name = 'France', data =df4$FR, color='blue', marker = list(symbol = 'triangle')),
                       list(name = 'Allemagne', data =df4$DE, color = 'red', marker = list(symbol = 'triangle')),
                       list(name = 'Union Européenne', data =df4$EU, color = 'green', marker = list(symbol = 'triangle') )  )  %>%
            hc_xAxis( categories = unique(df4$annee) ) %>%
            hc_yAxis( title = list(text = "en tonnes de CO2 par terajoules")  ) %>%
            hc_plotOptions(column = list(
                dataLabels = list(enabled = F),
                #stacking = "normal",
                enableMouseTracking = T ) 
            )%>%
            hc_tooltip(table = TRUE,
                       sort = TRUE,
                       pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                             " {series.name}: {point.y} tCO2/TJ"),
                       headerFormat = '<span style="font-size: 13px">Année {point.key}</span>'
            ) %>%
            hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = 000 )
        
    })
    
    
    
    
    
    #Courbe sur la décomposition comptable des émissions de CO2
    
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
    
    
    #2. Energies renouvelables
    
    #Courbe représentant la consommation d'ENR et d'énergies fossiles en France
    
    output$conso_ER_plot <- renderPlotly({
        df5 <- df_3_long
        conso_ER_plot <- ggplot(df5, aes(x=annee,y=value, color=consommation_finale)) +
            geom_line() +
            scale_color_manual(values = c("brown", "green"),labels = c("Energies fossiles","Energies renouvelables"))+
            theme_gray()
        ggplotly(conso_ER_plot)
        
    })
    
    #Figure comparant la production nucléaire en France et en Allemagne
    
    output$nucl_plot <- renderPlotly({
        df10 <- df_nucl
        nucl_plot <- ggplot(df10, aes(x=annee, y=quantite_produite, fill=pays))+
            geom_bar(width = 1, stat = "identity")+
            scale_fill_manual(values = c("#56B4E9","#E69F00"),labels = c("France","Allemagne"))+
            labs(x="Année", y="Quantité produite(en millions de KwH)")+
            theme_gray()
        ggplotly(nucl_plot)
        
    })
    
    #Comparaison de la consommation et de la production d'ENR en France et en Allemagne ( à associer aux tableaux où on peut choisir les données observées)
    
    output$conso_ER_Fr_All_plot <- renderPlotly({
        df9 <- df_6_long 
        conso_ER_Fr_All_plot <- ggplot(df9, aes(x=consommation_finale_energies_renouvelables,y=value, color=consommation_finale_energies_renouvelables)) +
            scale_color_manual(values = c("#E69F00", "#56B4E9"),labels = c("Allemagne","France"))+
            geom_boxplot()+
            labs(x="Pays", y="Valeur(en TEP)")+
            theme_gray()
        ggplotly(conso_ER_Fr_All_plot)
        
    })
    
    output$prod_ER_Fr_All_plot <- renderPlotly({
        df10 <- df_7_long
        prod_ER_Fr_All_plot <- ggplot(df10, aes(x=production_primaire_energies_renouvelables,y=value, color=production_primaire_energies_renouvelables)) +
            scale_color_manual(values = c("#E69F00", "#56B4E9"),labels = c("Allemagne","France"))+
            geom_boxplot()+
            labs(x="Pays", y="Valeur(en TEP)")+
            theme_gray()
        ggplotly(prod_ER_Fr_All_plot)
        
    })
    
    
    #Figure comparant les parts des ENR dans la consommation en France et dans d'autres pays européens, et leur évolution
    
    output$part_conso_ER_plot <-  renderPlotly({
        df6 <- df_8_long
        part_conso_ER_plot <-  ggplot(df6, aes(x=Annee,y=part_energies_renouvelables_conso_primaire, color=pays),lwd=2) +
            scale_color_manual(values = c("#E69F00", "#56B4E9","red","#009E73"),labels = c("Allemagne","France","Italie","UE"))+
            geom_line()+
            labs( x="Année", y="en pourcentage")+
            theme(plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))
        ggplotly(part_conso_ER_plot)
        
    })
    
    #Figure comparant les parts des ENR dans la production en France et dans d'autres pays européens, et leur évolution
    
    output$part_prod_ER_plot <-  renderPlotly({
        df8 <- df_9_long
        part_prod_ER_plot <-  ggplot(df8, aes(x=Annee,y=part_energies_renouvelables_prod_primaire, color=pays),lwd=2) +
            scale_color_manual(values = c("#E69F00", "#56B4E9","red","#009E73"),labels = c("Allemagne","France","Italie","UE"))+
            geom_line()+
            labs( x="Année", y="en pourcentage")+
            theme(plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))
        ggplotly(part_prod_ER_plot)
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)