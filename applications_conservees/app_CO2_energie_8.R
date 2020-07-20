

# install.packages("highcharter")
# install.packages("plotly")

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
        
        h2(paste0("L'énergie en France et en Europe")),
        fluidRow( column (width = 12,h4("Répartition de la consommation d'énergie entre les principales sources en France", align = 'center'), plotlyOutput('conso_sources_energies_plot')),
                  column (width = 12,h4("Répartition de la production d'énergie entre les principales sources en France", align = 'center'), plotlyOutput('prod_sources_energies_plot')),
                  column (width = 12,h4("Déséquilibres consommation/production pour les principales sources d'énergie en France", align = 'center'), plotlyOutput('deseq_sources_energies_plot')),
                  # column (width = 12,h4("Comparaison de la consommation d'énergies renouvelables et fossiles", align = 'center'), plotlyOutput('conso_ER_plot')),
                  column (width = 12,h4("Comparaison de la production d'énergie nucléaire en France et en Allemagne", align = 'center'), plotlyOutput('nucl_plot')),
                  column (width = 12,h4("Comparaison  des niveaux de consommation d'énergies renouvelables en France et en Allemagne", align = 'center'), plotlyOutput('conso_ER_Fr_All_plot')),
                  column (width = 12,h4("Comparaison  des niveaux de production d'énergies renouvelables en France et en Allemagne", align = 'center'), plotlyOutput('prod_ER_Fr_All_plot')),
                  column (width = 12,h4("Evolution de la part des énergies renouvables dans la consommation finale en France et dans d'autres pays européens", align = 'center'), plotlyOutput('part_conso_ER_plot')),
                  column (width = 12,h4("Evolution de la part des énergies renouvables dans la production primaire en France et dans d'autres pays européens", align = 'center'), plotlyOutput('part_prod_ER_plot'))
        )
        
        
        
        
    ),
    
    
)

# Define server logic 
server <- function(input, output) {
    
    #1. CO2
    
    #Courbe sur les émissions globales
    
    dfshiny1<- df1_fr_all  %>% 
        filter(Annee %in% c(1980:2016))
    
    output$courbe_emissions <-renderHighchart({
        highchart() %>%
            hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
            hc_chart(type = 'line') %>%
            hc_series( list(name = 'France', data =dfshiny1$France, color='blue', marker = list(symbol = 'circle') ),
                       list(name = 'Allemagne', data =dfshiny1$Allemagne, color = 'red', marker = list(symbol = 'circle') )  )  %>%
            hc_xAxis( categories = unique(dfshiny1$Annee) ) %>%
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
    
    dfshiny7 <- df2_long
    
    output$emissions_secteurs <- renderPlotly({
        emissions_secteurs <- ggplot(dfshiny7, aes(x=Annee,y= value)) +
            geom_bar(aes (x=Annee, y =value, fill=secteur),stat = "identity", position = "stack")+
            scale_fill_manual(values = c("green","brown","orange","blue"),labels = c("Industrie de l'energie","Industrie manufacturiere et construction", "Residentiel et Tertiaire", "Transports"))+
            labs(x="Année",y="Mtoe")+
            theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))+
            guides(fill=guide_legend(nrow=2,byrow=TRUE))
        
        
    })
    
    #Courbe sur l'intensité carbone du PIB
    
    dfshiny2 <- df3_past %>% mutate(annee=rep(seq(1990,2016),3))%>%
        pivot_wider(names_from =pays, values_from =valeur)
    
    output$courbe_intensite_carbone <-renderHighchart({
        highchart() %>%
            hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
            hc_chart(type = 'line') %>%
            hc_series( list(name = 'France', data =dfshiny2$FR, color='blue', marker = list(symbol = 'circle')),
                       list(name = 'Allemagne', data =dfshiny2$DE, color = 'red', marker = list(symbol = 'circle')),
                       list(name = 'Union Européenne', data =dfshiny2$EU, color = 'green', marker = list(symbol = 'circle') )  )  %>%
            hc_xAxis( categories = unique(dfshiny2$annee) ) %>%
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
    
    dfshiny4 <- df4_past %>% mutate(annee=rep(seq(1990,2015),3))%>%
        pivot_wider(names_from =pays, values_from =valeur)
    
    output$courbe_intensite_carbone_energie <-renderHighchart({
        highchart() %>%
            hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
            hc_chart(type = 'line') %>%
            hc_series( list(name = 'France', data =dfshiny4$FR, color='blue', marker = list(symbol = 'triangle')),
                       list(name = 'Allemagne', data =dfshiny4$DE, color = 'red', marker = list(symbol = 'triangle')),
                       list(name = 'Union Européenne', data =dfshiny4$EU, color = 'green', marker = list(symbol = 'triangle') )  )  %>%
            hc_xAxis( categories = unique(dfshiny4$annee) ) %>%
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
    
    dfshiny3 <- df_Kaya %>% select(-CO2)
    
    output$courbe_Kaya <-renderHighchart({
        highchart() %>%
            hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
            hc_chart(type = 'line') %>%
            hc_series( list(name = 'Intensité carbone énergie', data =dfshiny3$`CO2 per energy`, color='red', marker = list(symbol = 'circle')),
                       list(name = 'Intensité énergétique du PIB', data =dfshiny3$`Energy per GDP`, color = 'blue', marker = list(symbol = 'circle')),
                       list(name = 'PIB par tête', data =dfshiny3$`GDP per capita`, color = 'orange', marker = list(symbol = 'circle')),
                       list(name = 'Population', data =dfshiny3$Population, color = 'black', marker = list(symbol = 'circle') )  )  %>%
            hc_xAxis( categories = unique(dfshiny3$Annee) ) %>%
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
    
    
    #2. Energies 
    
    
    #Figures sur la répartition de la consommation et de la production d'énergie par sources en France
    
       #Conso
    
    output$conso_sources_energies_plot<- renderPlotly({
        dfshiny12<- df_7_long_bis
        conso_sources_energies_plot <- ggplot(dfshiny12, aes(x=Annee,y=value, fill=source))+ 
        geom_bar(aes (x=Annee, y =value, fill=source),stat = "identity", position = "stack")+
        scale_fill_manual(values = c("brown","blue","green","orange","black"),labels = c("Pétrole","Nucléaire","Charbon","Gaz","Hydroélectricité"))+
        labs(x="Année",y="Mtoe")+
        theme_gray()
       ggplotly(conso_sources_energies_plot)
        
    })
    
    
       #Prod
    
    output$prod_sources_energies_plot<- renderPlotly({
        dfshiny13<- df_8_long_bis
        prod_sources_energies_plot <- ggplot(dfshiny13, aes(x=Annee,y=value, fill=source))+ 
            geom_bar(aes (x=Annee, y =value, fill=source),stat = "identity", position = "stack")+
            scale_fill_manual(values = c("brown","blue","green","orange","black"),labels = c("Pétrole","Nucléaire","Charbon","Gaz","Hydroélectricité"))+
            labs(x="Année",y="Mtoe")+
            theme_gray()
        ggplotly(prod_sources_energies_plot)
        
    })
    
    
      #Déséquilibre conso/prod
    
    #Différence production-consommation des différentes sources d'énergie
    
    df_9_long_bis <- df_7_long_bis %>% left_join(df_8_long_bis, by =c("Annee","source"), copy=FALSE)%>%
        rename(consommation = value.x , production=value.y)%>%
        mutate(desequilibre = consommation - production)%>%
        filter(desequilibre != 0.0)
    
    output$deseq_sources_energies_plot<- renderPlotly({
        dfshiny14<- df_9_long_bis
        deseq_sources_energies_plot <- ggplot(dfshiny14, aes(x=Annee,y=desequilibre, fill=source))+ 
            geom_bar(aes (x=Annee, y =desequilibre, fill=source),stat = "identity", position = "stack")+
            scale_fill_manual(values = c("brown","blue","black"),labels = c("Pétrole","Charbon","Gaz"))+
            labs(x="Année",y="Mtoe")+
            theme_gray()
        ggplotly(deseq_sources_energies_plot)
        
    })
    
    
    #On remarque que pour le nucléaire et l'hydroélectricité, il y a équilibre entre conso et prod, donc on ne les représente pas
    #sur la figure des déséquilibres. (Note à ajouter à la figure sur Shiny)
    
    
    #Courbe représentant la consommation d'ENR et d'énergies fossiles en France
    
    # output$conso_ER_plot <- renderPlotly({
    #     dfshiny5 <- df_3_long
    #     conso_ER_plot <- ggplot(dfshiny5, aes(x=annee,y=value, color=consommation_finale)) +
    #         geom_line() +
    #         scale_color_manual(values = c("brown", "green"),labels = c("Energies fossiles","Energies renouvelables"))+
    #         theme_gray()
    #     ggplotly(conso_ER_plot)
    #     
    # })
    
    #Figure comparant la production nucléaire en France et en Allemagne
    
    output$nucl_plot <- renderPlotly({
        dfshiny10 <- df_nucl
        nucl_plot <- ggplot(dfshiny10, aes(x=annee, y=quantite_produite, fill=pays))+
            geom_bar(width = 1, stat = "identity")+
            scale_fill_manual(values = c("#56B4E9","#E69F00"),labels = c("France","Allemagne"))+
            labs(x="Année", y="Quantité produite(en millions de KwH)")+
            theme_gray()
        ggplotly(nucl_plot)
        
    })
    
    
    
    
    #Comparaison de la consommation et de la production d'ENR en France et en Allemagne ( à associer aux tableaux où on peut choisir les données observées)
    
    
        #Consommation
    
    # output$conso_ER_Fr_All_plot <- renderPlotly({
    #     dfshiny9 <- df_6_long 
    #     conso_ER_Fr_All_plot <- ggplot(dfshiny9, aes(x=consommation_finale_energies_renouvelables,y=value, color=consommation_finale_energies_renouvelables)) +
    #         scale_color_manual(values = c("#E69F00", "#56B4E9"),labels = c("Allemagne","France"))+
    #         geom_boxplot()+
    #         labs(x="Pays", y="Valeur(en TEP)")+
    #         theme_gray()
    #     ggplotly(conso_ER_Fr_All_plot)
    #     
    # })
    
    output$conso_ER_Fr_All_plot <- renderPlotly({
        dfshiny9 <- df_6_long 
        conso_ER_Fr_All_plot <- ggplot(dfshiny9, aes(x=annee,y=value, color=consommation_finale_energies_renouvelables)) +
            scale_color_manual(values = c("#E69F00", "#56B4E9"),labels = c("Allemagne","France"))+
            geom_line()+
            labs(x="Pays", y="Valeur(en TEP)")+
            theme_gray()
        ggplotly(conso_ER_Fr_All_plot)
        
    })
    
         
       #Production
    
    # output$prod_ER_Fr_All_plot <- renderPlotly({
    #     dfshiny11 <- df_7_long
    #     prod_ER_Fr_All_plot <- ggplot(dfshiny11, aes(x=production_primaire_energies_renouvelables,y=value, color=production_primaire_energies_renouvelables)) +
    #         scale_color_manual(values = c("#E69F00", "#56B4E9"),labels = c("Allemagne","France"))+
    #         geom_boxplot()+
    #         labs(x="Pays", y="Valeur(en TEP)")+
    #         theme_gray()
    #     ggplotly(prod_ER_Fr_All_plot)
    #     
    # })
    
    output$prod_ER_Fr_All_plot <- renderPlotly({
        dfshiny11 <- df_7_long
        prod_ER_Fr_All_plot <- ggplot(dfshiny11, aes(x=annee,y=value, color=production_primaire_energies_renouvelables)) +
            scale_color_manual(values = c("#E69F00", "#56B4E9"),labels = c("Allemagne","France"))+
            geom_line()+
            labs(x="Pays", y="Valeur(en TEP)")+
            theme_gray()
        ggplotly(prod_ER_Fr_All_plot)
        
    })
    
    
    #Figure comparant les parts des ENR dans la consommation en France et dans d'autres pays européens, et leur évolution
    
    output$part_conso_ER_plot <-  renderPlotly({
        dfshiny6 <- df_8_long
        part_conso_ER_plot <-  ggplot(dfshiny6, aes(x=Annee,y=part_energies_renouvelables_conso_primaire, color=pays),lwd=2) +
            scale_color_manual(values = c("#E69F00", "#56B4E9","red","#009E73"),labels = c("Allemagne","France","Italie","UE"))+
            geom_line()+
            labs( x="Année", y="en pourcentage")+
            theme(plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))
        ggplotly(part_conso_ER_plot)
        
    })
    
    #Figure comparant les parts des ENR dans la production en France et dans d'autres pays européens, et leur évolution
    
    output$part_prod_ER_plot <-  renderPlotly({
        dfshiny8 <- df_9_long
        part_prod_ER_plot <-  ggplot(dfshiny8, aes(x=Annee,y=part_energies_renouvelables_prod_primaire, color=pays),lwd=2) +
            scale_color_manual(values = c("#E69F00", "#56B4E9","red","#009E73"),labels = c("Allemagne","France","Italie","UE"))+
            geom_line()+
            labs( x="Année", y="en pourcentage")+
            theme(plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))
        ggplotly(part_prod_ER_plot)
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)