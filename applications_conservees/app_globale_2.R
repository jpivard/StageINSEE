library(shiny)
library(highcharter)
library(dplyr)
library(tidyr)
library(readr)
library(plotly)
library(shinydashboard)

# ui <- navbarPage("Quelques données sur les enjeux de la transition écologique",
#                  tabPanel("Emissions de CO2 et énergie"),
#                  tabPanel("Finance verte")
# )


# Define UI for application 
ui <- navbarPage(
    # dashboardHeader(title = "Un aperçu de la finance verte et des investissements pour le climat en France"),
    
    tabPanel("Emissions de CO2 et énergie"),
    tabPanel("Emissions de CO2 et énergie"),
    tabPanel("Finance verte"),
    
    titlePanel(strong("Quelques données sur les enjeux de la transition écologique")),
    
    
    mainPanel(
        
        # dashboardSidebar(),
        # dashboardBody(
        #     
        #     tabItems(
        #         ##  Main dashboard ----------------------------------------------------------
        #         tabItem( tabName = 'dashboard',
        #                  
        #         ),
        
        h1(paste0("Emissions de CO2 et énergie")),
        
        h2(paste0("Les émissions de CO2 en France depuis 1980")),
        fluidRow  (em("Vision globale des émissions"),
                   br(),
                  column( width = 12,h4("Evolution des émissions globales et comparaison avec l'Allemagne", align = 'center'), highchartOutput('courbe_emissions')),
                  em("Equation de Kaya et intensités carbone du PIB et de l'énergie"),
                  br(),
                  column( width = 12,h4("Evolution des grandeurs reliées aux émissions de CO2", align = 'center'), highchartOutput('courbe_Kaya')),
                  column( width = 6,h4("Evolution de l'intensité carbone du PIB", align = 'left'), highchartOutput('courbe_intensite_carbone')),
                  column( width = 6,h4("Evolution de l'intensité carbone de l'énergie",align='right'),highchartOutput('courbe_intensite_carbone_energie')),
                  em("Répartition sectorielle"),
                  br(),
                  column( width = 12,h4("Répartition des émissions par secteurs", align = 'center'), plotlyOutput('emissions_secteurs')),
                  
        
        
        h2(paste0("L'énergie en France et en Europe")),
        fluidRow( em("Analyse par sources d'énergies"),
                  br(),
                  column (width = 6,h4("Répartition de la consommation d'énergie entre les principales sources en France", align = 'center'), plotlyOutput('conso_sources_energies_plot')),
                  column (width = 6,h4("Répartition de la production d'énergie entre les principales sources en France", align = 'center'), plotlyOutput('prod_sources_energies_plot')),
                  column (width = 12,h4("Déséquilibres consommation/production pour les principales sources d'énergie en France", align = 'center'), plotlyOutput('deseq_sources_energies_plot')),
                  # column (width = 12,h4("Comparaison de la consommation d'énergies renouvelables et fossiles", align = 'center'), plotlyOutput('conso_ER_plot')),
                  br(),
                  em("Energies décarbonées : nucléaire et renouvelable"),
                  br(),
                  column (width = 12,h4("Comparaison de la production d'énergie nucléaire en France et en Allemagne", align = 'center'), plotlyOutput('nucl_plot')),
                  column (width = 6,h4("Comparaison  des niveaux de consommation d'énergies renouvelables en France et en Allemagne", align = 'center'), plotlyOutput('conso_ER_Fr_All_plot')),
                  column (width = 6,h4("Comparaison  des niveaux de production d'énergies renouvelables en France et en Allemagne", align = 'center'), plotlyOutput('prod_ER_Fr_All_plot')),
                  column (width = 6,h4("Evolution de la part des énergies renouvables dans la consommation finale en France et dans d'autres pays européens", align = 'center'), plotlyOutput('part_conso_ER_plot')),
                  column (width = 6,h4("Evolution de la part des énergies renouvables dans la production primaire en France et dans d'autres pays européens", align = 'center'), plotlyOutput('part_prod_ER_plot')),
        
        
        br(),
        
        h1(paste0("Un aperçu de la finance verte et des investissements pour le climat en France")),
        
        h2(paste0("Les investissements pour le climat")) ,
        
        # fluidRow(
        #     valueBoxOutput("inv_2016_2018_box"),
        #     valueBoxOutput("inv_2019_2023_box"),
        #     valueBoxOutput("inv_2024_2028_box")
        # ),
        
        h3(paste0("Investissement annuel par secteurs")),
        fluidRow(column( width = 6,h4("Montants d'investissement public actuel", align = 'center'), plotlyOutput('plot_inv_secteurs_1')),
                 column( width = 6,h4("Nouveaux objectifs d'investissement public", align = 'center'), plotlyOutput('plot_inv_secteurs_2')),
                 column( width = 12,h4("Investissements privés supplémentaires attendus", align = 'center'), plotlyOutput('plot_inv_secteurs_3'))
                 ,
                          
        h3(paste0("Investissements par financeurs")),
        fluidRow(column( width = 4,h4("Investissements historiques(2016-2018)", align = 'center'), plotlyOutput('financeurs_plot_1')),
                column( width = 4,h4("Investissements à court terme (jusque 2023)", align = 'center'), plotlyOutput('financeurs_plot_2')),
                column( width = 4,h4("Investissements à moyen terme (2024-2028)", align = 'center'), plotlyOutput('financeurs_plot_3'))
                 ,
                                            
                                            
                                            
      h2(paste0("Quelques données sur la finance verte"),align = 'center') ,  
                                            
     h3(paste0("Financements par financeurs"),align = 'center'),
        fluidRow(column( width = 4,h4("Financements historiques(2016-2018)", align = 'center'), plotlyOutput('financeurs_plot_4')),
                column( width = 4,h4("Financements à court terme (jusque 2023)", align = 'center'), plotlyOutput('financeurs_plot_5')),
                column( width = 4,h4("Financements à moyen terme (2024-2028)", align = 'center'), plotlyOutput('financeurs_plot_6'))
                 ,
                                                              
                                                              
                                                              h3(paste0("Répartition mondiale des obligations vertes par émetteurs"),align = 'center'),
                                                              
                                                              img(src = "Figure répartition obligations vertes par émetteurs.png", height = 140, width = 400),
                                                              
                                                              
                                                              h3(paste0("Répartition géographique du marché des fonds verts"),align = 'center'),
                                                              
                                                              img(src = "Figure répartition  géographique marché fonds verts.png", height = 140, width = 400),
                                                              
                                                              
                                                              h3(paste0("Evolution des encours de fonds verts"),align = 'center'),
                                                              
                                                              img(src = "Figure évolution des encours de fonds verts.png", height = 140, width = 400),
                                                              
                                                              
                                                              h3(paste0("Evolution des encours par types de fonds"),align = 'center'),
                                                              
                                                              img(src = "Figure évolution des encours par types de fonds.png", height = 140, width = 400),
                                                              
                                                              
                                                              h3(paste0("Evolution du marché des fonds verts par adéquation"),align = 'center'),
                                                              
                                                              img(src = "Figure évolution du marché des fonds verts par adéquation.png", height = 140, width = 400),
                                                              
                                                     ) 
                                            )
                                            
                                   )
                                   
                          )
                          
                 )
                 
        )
        
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
        dfshiny10 <- df_nucl%>% 
            filter(annee %in% c(1991:2017))
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
    
    #3.Finance verte
    
    #Figures sur les investissements par secteurs 
    
    output$plot_inv_secteurs_1 <- renderPlotly({
        plot_inv_secteurs_1 <-
            ggplot(dfinv_2, 
                   aes(x= Montant_financement_public_annuel_actuel, 
                       y = 0, 
                       group = Secteur, 
                       text = Secteur
                   )) +
            geom_point(aes(size = Montant_financement_public_annuel_actuel , fill = Secteur), 
                       alpha = 0.6, 
                       color = "black", 
                       shape = 21) +
            coord_cartesian(ylim = c(-2, 2)) +
            scale_size_area(max_size = 25) +
            guides(fill = FALSE, size = FALSE) +
            labs(x = "Montant en milliards d'euros /n (balayer avec la souris et zoomer pour voir les secteurs)") +
            scale_x_continuous(
                name = "Montant en milliards d'euros (balayer avec la souris et zoomer pour voir les secteurs)", 
                trans = "log10", 
                breaks = c(0.5,1,1.5,1.6,1.7,1.8,1.9,2)) +
            scale_fill_viridis_d() + 
            theme(
                panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                panel.grid.major.y = element_blank(),
                panel.background = element_rect(fill = "white"),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 6),
                axis.title.x = element_text(size = 10, margin = margin(t = 10)),
                axis.title.y = element_blank(),
                axis.ticks.y = element_blank(),
                plot.title = element_text(size = 10, hjust = 0),
                panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1)
            )
        
        
        style(
            hide_legend(
                ggplotly(tooltip = c("text", "Count"))), 
            hoverlabel = list(bgcolor = "white")
        )
    })
    
    output$plot_inv_secteurs_2 <- renderPlotly({
        plot_inv_secteurs_2 <-
            ggplot(dfinv_2, 
                   aes(x=Nouvel_objectif_annuel, 
                       y = 0, 
                       group = Secteur, 
                       text = Secteur
                   )) +
            geom_point(aes(size = Nouvel_objectif_annuel , fill = Secteur), 
                       alpha = 0.6, 
                       color = "black", 
                       shape = 21) +
            coord_cartesian(ylim = c(-2, 2)) +
            scale_size_area(max_size = 25) +
            guides(fill = FALSE, size = FALSE) +
            labs(x = "Montant en milliards d'euros /n (balayer avec la souris et zoomer pour voir les secteurs)") +
            scale_x_continuous(
                name = "Montant en milliards d'euros (balayer avec la souris et zoomer pour voir les secteurs)", 
                trans = "log10", 
                breaks = c(0.5,1,1.5,2,2.5,3)) +
            scale_fill_viridis_d() + 
            theme(
                panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                panel.grid.major.y = element_blank(),
                panel.background = element_rect(fill = "white"),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 6),
                axis.title.x = element_text(size = 10, margin = margin(t = 10)),
                axis.title.y = element_blank(),
                axis.ticks.y = element_blank(),
                plot.title = element_text(size = 10, hjust = 0),
                panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1)
            )
        
        
        style(
            hide_legend(
                ggplotly(tooltip = c("text", "Count"))), 
            hoverlabel = list(bgcolor = "white")
        )
    })
    
    
    output$plot_inv_secteurs_3<- renderPlotly({
        plot_inv_secteurs_3 <-
            ggplot(dfinv_2, 
                   aes(x=Investissement_supplementaire_annuel_genere_espere, 
                       y = 0, 
                       group = Secteur, 
                       text = Secteur
                   )) +
            geom_point(aes(size = Investissement_supplementaire_annuel_genere_espere , fill = Secteur), 
                       alpha = 0.6, 
                       color = "black", 
                       shape = 21) +
            coord_cartesian(ylim = c(-2, 2)) +
            scale_size_area(max_size = 25) +
            guides(fill = FALSE, size = FALSE) +
            labs(x = "Montant en milliards d'euros /n (balayer avec la souris et zoomer pour voir les secteurs)") +
            scale_x_continuous(
                name = "Montant en milliards d'euros (balayer avec la souris et zoomer pour voir les secteurs)", 
                trans = "log10", 
                breaks = c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5)) +
            scale_fill_viridis_d() + 
            theme(
                panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                panel.grid.major.y = element_blank(),
                panel.background = element_rect(fill = "white"),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 6),
                axis.title.x = element_text(size = 10, margin = margin(t = 10)),
                axis.title.y = element_blank(),
                axis.ticks.y = element_blank(),
                plot.title = element_text(size = 10, hjust = 0),
                panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1)
            )
        
        
        style(
            hide_legend(
                ggplotly(tooltip = c("text", "Count"))), 
            hoverlabel = list(bgcolor = "white")
        )
    })
    
    
    #Données sur les investissements par financeurs
    
    # output$financeurs_plot_1<- renderPlotly({
    #     
    #     financeurs_plot_1 <- 
    #     ggplot(dfinv_3, 
    #            aes_string(
    #                x = dfinv_3$Financeurs,
    #                y ="Investissements_historiques_2016_2018",
    #                fill = "Investissements_historiques_2016_2018")) +
    #     geom_bar(stat = "identity",
    #              color = "green",
    #              alpha = 0.8) +
    #     scale_fill_continuous("green") +
    #     labs(x = NULL, y = " en milliards d'euros ") +
    #     guides(fill = "none") +
    #     theme_minimal() +
    #     coord_flip() +
    #     ggtitle("Investissements entre 2016 et 2018") +
    #     theme(plot.title = element_text(face = "bold", size = 12))
    # 
    # })
    
    dfinv_3_plot1 <- dfinv_3 %>% filter(!is.na(`Investissements_historiques_2016_2018`))
    
    output$financeurs_plot_1<- renderPlotly({
        financeurs_plot_1 <- ggplot(dfinv_3_plot1, aes(x=Financeurs,y=Investissements_historiques_2016_2018)) +
            geom_bar(aes (x=Financeurs, y =Investissements_historiques_2016_2018, fill=Financeurs),stat = "identity", position = "stack")+
            scale_fill_manual(values = c("green","brown","orange","blue","red","yellow"),labels = c("Etat_et_agences","Collectivites_territoriales","Bailleurs_sociaux","Gestionnaires_infrastructures","Entreprises","Menages"))+
            labs(x="Financeur",y="milliards d'euros")
        
    })
    
    
    dfinv_3_plot2 <- dfinv_3 %>% filter(!is.na(`Investissements_court_terme`))
    
    output$financeurs_plot_2<- renderPlotly({
        financeurs_plot_2 <- ggplot( dfinv_3_plot2, aes(x=Financeurs,y=Investissements_court_terme)) +
            geom_bar(aes (x=Financeurs, y =Investissements_court_terme, fill=Financeurs),stat = "identity", position = "stack")+
            scale_fill_manual(values = c("green","brown","orange","blue","red","yellow"),labels = c("Etat_et_agences","Collectivites_territoriales","Bailleurs_sociaux","Gestionnaires_infrastructures","Entreprises","Menages"))+
            labs(x="Financeur",y="milliards d'euros")
        
    })
    
    
    dfinv_3_plot3 <- dfinv_3 %>% filter(!is.na(`Investissements_moyen_terme`))
    
    output$financeurs_plot_3<- renderPlotly({
        financeurs_plot_3 <- ggplot( dfinv_3_plot3, aes(x=Financeurs,y=Investissements_moyen_terme)) +
            geom_bar(aes (x=Financeurs, y =Investissements_moyen_terme, fill=Financeurs),stat = "identity", position = "stack")+
            scale_fill_manual(values = c("green","brown","orange","blue","red","yellow"),labels = c("Etat_et_agences","Collectivites_territoriales","Bailleurs_sociaux","Gestionnaires_infrastructures","Entreprises","Menages"))+
            labs(x="Financeur",y="milliards d'euros")
        
    })
    
    
    dffin <- dfinv_3 
    
    output$financeurs_plot_4<- renderPlotly({
        financeurs_plot_4 <- ggplot(dffin, aes(x=Financeurs,y=Financements_historiques)) +
            geom_bar(aes (x=Financeurs, y =Financements_historiques, fill=Financeurs),stat = "identity", position = "stack")+
            scale_fill_manual(values = c("grey","green","brown","orange","blue","pink", "purple", "black", "red","yellow"),labels = c("Fonds_européens","Etat_et_agences","Collectivites_territoriales","Bailleurs_sociaux","Gestionnaires_infrastructures","Banques_publiques", "Banques_commerciales","Marches_financiers", "Entreprises","Menages"))+
            labs(x="Financeur",y="milliards d'euros")
        
    })
    
    
    
    output$financeurs_plot_5<- renderPlotly({
        financeurs_plot_5 <- ggplot(dffin, aes(x=Financeurs,y=Financements_court_terme)) +
            geom_bar(aes (x=Financeurs, y =Financements_court_terme, fill=Financeurs),stat = "identity", position = "stack")+
            scale_fill_manual(values = c("grey","green","brown","orange","blue","pink", "purple", "black", "red","yellow"),labels = c("Fonds_européens","Etat_et_agences","Collectivites_territoriales","Bailleurs_sociaux","Gestionnaires_infrastructures","Banques_publiques", "Banques_commerciales","Marches_financiers", "Entreprises","Menages"))+
            labs(x="Financeur",y="milliards d'euros")
        
    })
    
    
    output$financeurs_plot_6<- renderPlotly({
        financeurs_plot_6 <- ggplot(dffin, aes(x=Financeurs,y=Financements_moyen_terme)) +
            geom_bar(aes (x=Financeurs, y =Financements_moyen_terme, fill=Financeurs),stat = "identity", position = "stack")+
            scale_fill_manual(values = c("grey","green","brown","orange","blue","pink", "purple", "black", "red","yellow"),labels = c("Fonds_européens","Etat_et_agences","Collectivites_territoriales","Bailleurs_sociaux","Gestionnaires_infrastructures","Banques_publiques", "Banques_commerciales","Marches_financiers", "Entreprises","Menages"))+
            labs(x="Financeur",y="milliards d'euros")
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
