
required_packages <- c(
    "checkpoint",
    "shinydashboard"
)

# install missing packages

new.packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(new.packages)) {
    install.packages(new.packages)
}

rm(new.packages)

library(checkpoint)
checkpoint(snapshotDate ='2019-12-17')
library(shiny)
library(shinydashboard)

# Define UI for application 
ui <- fluidPage(
    # dashboardHeader(title = "Un aperçu de la finance verte et des investissements pour le climat en France"),
    
    titlePanel("Un aperçu de la finance verte et des investissements pour le climat en France"),
    mainPanel(
        
        # dashboardSidebar(),
        # dashboardBody(
        #     
        #     tabItems(
        #         ##  Main dashboard ----------------------------------------------------------
        #         tabItem( tabName = 'dashboard',
        #                  
        #         ),
        
        h2(paste0("Les investissements pour le climat")) ,
        
        fluidRow(
            valueBoxOutput("inv_2016_2018_box"),
            valueBoxOutput("inv_2019_2023_box"),
            valueBoxOutput("inv_2024_2028_box")
        ),
        
        h3(paste0("Investissements par secteurs")),
        fluidRow(column( width = 12,h4("Montants annuels actuels", align = 'center'), plotlyOutput('plot_inv_secteurs_1')),
                 fluidRow(column( width = 12,h4("Nouveaux objectifs annuels", align = 'center'), plotlyOutput('plot_inv_secteurs_2')),
                 fluidRow(column( width = 12,h4("Investissements annuels supplémentaires générés espérés", align = 'center'), plotlyOutput('plot_inv_secteurs_3'))
                 ),
                          
        h3(paste0("Investissements par financeurs")),
        fluidRow(column( width = 12,h4("Investissements historiques", align = 'center'), plotlyOutput('financeurs_plot_1')),
        fluidRow(column( width = 12,h4("Investissements à court terme (jusque 2023)", align = 'center'), plotlyOutput('financeurs_plot_2')),
        fluidRow(column( width = 12,h4("Investissements à moyen terme (2024-2028)", align = 'center'), plotlyOutput('financeurs_plot_3'))
                ),
                                            
       h3(paste0("Financements par financeurs"),align = 'center'),
       
       
                                            
     h2(paste0("Quelques données sur la finance verte"),align = 'center') ,  
     
          h3(paste0("Répartition des obligations vertes par émetteurs"),align = 'center'),
     
          h3(paste0("Répartition géographique du marché des fonds verts"),align = 'center'),
     
          h3(paste0("Evolution des encours de fonds verts"),align = 'center'),
     
          h3(paste0("Evolution des encours par types de fonds"),align = 'center'),
     
          h3(paste0("Evolution du marché des fonds verts par adéquation"),align = 'center'),
     
        
     
                                   ) 
                          )
                          
                 )
                 
        )
        
    )
    
)
       


# Define server logic 
server <- function(input, output) {
    
    #Données sur les investissements et besoins d'investissements par période
    
    # inv_2016_2018 <- dfinv_1 %>%
    #     filter( annee_debut_periode == 2016) %>%
    #     dplyr::select(Investissements_annuels_en_milliards_euros) %>%
    #     as.numeric
    # 
    # inv_2019_2013 <- dfinv_1 %>%
    #     filter( annee_debut_periode == 2019) %>%
    #     dplyr::select(Investissements_annuels_en_milliards_euros) %>%
    #     as.numeric
    # 
    # inv_2024_2028 <- dfinv_1 %>%
    #     filter( annee_debut_periode == 2024) %>%
    #     dplyr::select(Investissements_annuels_en_milliards_euros) %>%
    #     as.numeric
    # 
    # output$inv_2016_2018_box <- renderValueBox({
    #     valueBox(
    #         VB_style( paste0(format( inv_2016_2018,big.mark=','), " mds€" ),  "font-size: 60%;"  ),
    #         VB_style( paste0("2016-2018")  ), 
    #         color = "green"
    #     )
    # })
    # 
    # output$inv_2019_2023_box <- renderValueBox({
    #     valueBox(
    #         VB_style( paste0(format( inv_2019_2023,big.mark=','), " mds€" ),  "font-size: 60%;"  ),
    #         VB_style( paste0("2019-2023")  ), 
    #         color = "green"
    #     )
    # })
    # 
    # output$inv_2024_2028_box <- renderValueBox({
    #     valueBox(
    #         VB_style( paste0(format( inv_2024_2028,big.mark=','), " mds€" ),  "font-size: 60%;"  ),
    #         VB_style( paste0("2024-2028")  ), 
    #         color = "green"
    #     )
    # })
    # 
    
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
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)