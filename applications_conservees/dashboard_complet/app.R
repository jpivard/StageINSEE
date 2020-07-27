#####Chargement des packages #################

library(shiny)
library(highcharter)
library(dplyr)
library(tidyr)
library(readr)
library(plotly)
library(shinydashboard)


###### Traitement des données #################

# ------ 1. CO2 ---------------------

#Emissions globales
df_CO2= read_tsv(file="~/données/Greenhouse Gas,  1850-2016 (in MtCO2eq).csv")

colnames(df_CO2)[1]<- 'Annee'
colnames(df_CO2)[4]<- 'Allemagne'

df_CO2_fr_all <- df_CO2[,-3]
colonnes2 <- c("France","Allemagne")
df_CO2_fr_all_long<- df_CO2_fr_all %>% pivot_longer(colonnes2, names_to = 'pays', values_to = "value")


#Emissions par secteurs
df_secteurs = read_tsv(file = "~/données/Emissions par secteurs Rapport Secten (en Mt).csv")

#On ne retient que les principaux secteurs émetteurs
df_secteurs <- df_secteurs[,-c(2:4)]
df_secteurs <- df_secteurs[,-5]
df_secteurs <- df_secteurs[,-6]

colnames(df_secteurs)[2] <- "Energie"
colnames(df_secteurs)[3] <- "Ind_manuf"
colnames(df_secteurs)[4] <- "Residentiel_Tertiaire"

colonnes3 = c("Energie","Ind_manuf", "Residentiel_Tertiaire", "Transports")
df_secteurs_long <- df_secteurs %>% pivot_longer(colonnes3, names_to = 'secteur', values_to = "value")


#Intensité carbone du PIB
df_intensite_PIB = read_csv(file='~/données/Emissions intensity of GDP data.csv')

df_intensite_PIB_past <- df_intensite_PIB[-c(83:90),] 
df_intensite_PIB_past <- df_intensite_PIB_past[-28,]
#On retire les colonnes correspondant à des prévisions et la donnée supplémentaire pour l'UE (on prend la dernière pour simplifier)

df_intensite_PIB_past$year<- c(rep(seq(1990,2016),3))

#On supprime les colonnes inutiles et on renomme les colonnes restantes

df_intensite_PIB_past <- df_intensite_PIB_past[,-c(1:2)]
df_intensite_PIB_past <- df_intensite_PIB_past[,-2]
df_intensite_PIB_past <- df_intensite_PIB_past[,-3]

colnames(df_intensite_PIB_past)[1] <- 'pays'
colnames(df_intensite_PIB_past)[2] <- 'valeur'
colnames(df_intensite_PIB_past)[3] <- 'annee'


#Intensité carbone de l'énergie
df_intensite_energie = read_csv(file='~/données/Emissions intensity of primary energy data.csv')

df_intensite_energie_past <- df_intensite_energie[-c(81:86),] 
df_intensite_energie_past <- df_intensite_energie_past[-80,] 
df_intensite_energie_past <- df_intensite_energie_past[-53,] 
#On retire les colonnes correspondant à des prévisions et les données supplémentaires pour l'UE et l'Allemagne (on prend la dernière pour simplifier)

df_intensite_energie_past$year<- c(rep(seq(1990,2015),3))

#On supprime les colonnes inutiles et on renomme les colonnes restantes

df_intensite_energie_past <- df_intensite_energie_past[,-c(1:2)]
df_intensite_energie_past <- df_intensite_energie_past[,-2]
df_intensite_energie_past <- df_intensite_energie_past[,-3]

colnames(df_intensite_energie_past)[1] <- 'pays'
colnames(df_intensite_energie_past)[2] <- 'valeur'
colnames(df_intensite_energie_past)[3] <- 'annee'


#Décomposition comptable des émissions de CO2

df_Kaya = read_tsv(file='~/données/KAYA identity, France, 1980-2015 (in base 100).csv')

colnames(df_Kaya)[2]<- 'Contenu CO2 energie'
colnames(df_Kaya)[3]<- 'Intensite_energetique_PIB'
colnames(df_Kaya)[4]<- 'PIB par tete'





# ------ 2.Energie ----------------------

#Consommation et production par sources

df_conso_energie_source_fr <- read_tsv(file="~/données/Primary Energy Consumption by source, France, 1980-2016 (in Mtoe).csv")
df_conso_energie_source_fr <- df_conso_energie_source_fr[,-c(7:12)]

colnames(df_conso_energie_source_fr)<- c("Annee", "Pétrole","Charbon","Gaz","Nucléaire","Hydroélectricité")

colonnes7 <- c("Pétrole","Charbon","Gaz","Nucléaire","Hydroélectricité")
df_conso_energie_source_fr_long <- df_conso_energie_source_fr%>% pivot_longer(colonnes7, names_to = 'source', values_to = "value")


df_prod_energie_source_fr <- read_tsv(file = "~/données/Primary Energy Production by source, France, 1900-2016 (in Mtoe).csv")
df_prod_energie_source_fr <- df_prod_energie_source_fr[,-c(7:12)]

colnames(df_prod_energie_source_fr)<- c("Annee", "Pétrole","Charbon","Gaz","Nucléaire","Hydroélectricité")

colonnes8 <- c("Pétrole","Charbon","Gaz","Nucléaire","Hydroélectricité")

df_prod_energie_source_fr_long <- df_prod_energie_source_fr %>% pivot_longer(colonnes8, names_to = 'source', values_to = "value")  %>% 
    filter(Annee %in% c(1980:2016))


#Différence production-consommation des différentes sources d'énergie

df_deseq_conso_prod <- df_conso_energie_source_fr_long %>% left_join(df_prod_energie_source_fr_long, by =c("Annee","source"), copy=FALSE)%>%
    rename(consommation = value.x , production=value.y)%>%
    mutate(desequilibre = consommation - production)%>%
    filter(desequilibre != 0.0)


#Energie nucléaire

df_nucl =  read_csv(file='~/données/Prod_energie_nucleaire_France_Allemagne.csv')

df_nucl <-df_nucl [-c(56:57),] #On supprime les deux dernières lignes qui n'apportent pas d'infos supplémentaires.
df_nucl <- df_nucl %>% select(-'Unit')%>% select(-'Quantity Footnotes')

colnames(df_nucl)[1]<-"pays"
colnames(df_nucl)[2]<-"bien"
colnames(df_nucl)[3]<-"annee"
colnames(df_nucl)[4]<-"quantite_produite"

df_nucl<-df_nucl %>% select(-'bien') 


#Energies renouvelables

#Données en valeur

df_conso_enr_fr =read.csv(file='~/données/Consommation_finale_energies_renouvelables_France.csv')

colnames(df_conso_enr_fr )[1] <- 'annee'
colnames(df_conso_enr_fr )[2] <- 'consommation_finale_energies_renouvelables_fr'

df_conso_enr_all = read.csv(file='~/données/conso_finale_energies_renouvelables_all.csv')

colnames(df_conso_enr_all)[1] <- 'annee'
colnames(df_conso_enr_all)[2] <- 'consommation_finale_energies_renouvelables_All'

df_conso_enr_fr_all <- df_conso_enr_fr %>% left_join(df_conso_enr_all, by ="annee", copy=FALSE) 

colnames(df_conso_enr_fr_all)[2] <- 'France'
colnames(df_conso_enr_fr_all)[3] <- 'Allemagne'

col_2_bis <- c("France", "Allemagne")
df_conso_enr_fr_all_long <- df_conso_enr_fr_all %>% pivot_longer(col_2_bis, names_to = "consommation_finale_energies_renouvelables", values_to = "value")


df_prod_enr =read.csv(file='~/données/Production_primaire_energies_renouvelables.csv')

colnames(df_prod_enr)[1] <- 'annee'
colnames(df_prod_enr)[2] <- 'France'
colnames(df_prod_enr)[4] <- 'Allemagne'
colnames(df_prod_enr)[3] <- 'UE' 

colonnes_2<- c("France", "UE","Allemagne") 
df_prod_enr_long <- df_prod_enr %>% pivot_longer(colonnes_2, names_to = "production_primaire_energies_renouvelables", values_to = "value") 

colonnes_2_bis<- c("France","Allemagne") 
df_prod_enr_long <- df_prod_enr %>% pivot_longer(colonnes_2_bis, names_to = "production_primaire_energies_renouvelables", values_to = "value") 
df_prod_enr_long  <-select(df_prod_enr_long ,-UE)


#Données en proportion

df_part_enr_conso =  read_tsv(file='~/données/Renewable_Energy_Consumption_share_of_primary_energy.csv')
df_part_enr_prod = read_tsv(file='~/données/Renewable_Energy_Production_share_of_primary_energy.csv')

df_part_enr_conso <- df_part_enr_conso[,-5]
df_part_enr_prod <- df_part_enr_prod[,-5]

colonnes_3<- c("France", "Allemagne","Italie","UE") 
df_part_enr_conso_long <- df_part_enr_conso %>% pivot_longer(colonnes_3, names_to = "pays", values_to = "part_energies_renouvelables_conso_primaire") 

colonnes_4<- c("France", "Allemagne","Italie","UE") 
df_part_enr_prod_long <- df_part_enr_prod %>% pivot_longer(colonnes_4, names_to = "pays", values_to = "part_energies_renouvelables_prod_primaire") 


# -------- 3.Investissements climat ----------------------

dfinv_2 = read_tsv(file='~/données/Investissements par secteur I4CE.csv')

colnames(dfinv_2)[2] <- 'Montant_financement_public_annuel_actuel'
colnames(dfinv_2)[3] <- 'Nouvel_objectif_annuel'
colnames(dfinv_2)[4] <- 'Investissement_supplementaire_annuel_genere_attendu'

dfinv_3 = read_tsv(file = '~/données/Investissements Plan I4CE.csv')

colnames(dfinv_3)[2] <- 'Investissements_historiques_2016_2018'
colnames(dfinv_3)[3] <- 'Investissements_court_terme'
colnames(dfinv_3)[4] <- 'Investissements_moyen_terme'


# -------- 4.Finance verte -------------------------------

#Utilise les données du tableau précédent.



##### Définition de l'interface utilisateur ####################

ui <-  dashboardPage(
    
    
    
    dashboardHeader(
        title = "Visualisation de données sur la transition écologique",
        titleWidth = 800
        
    ),
    
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Emissions de CO2", tabName = "CO2", icon = icon("dashboard")),
            menuItem("L'énergie en France", tabName = "energie", icon = icon("list-ol")),
            menuItem("Investissements climat", tabName = "investissements", icon = icon("dashboard")),
            menuItem("Finance verte", tabName = "fin_verte", icon = icon("list-ol")),
            menuItem("Sources et autres informations", tabName="infos", icon = icon("database")),
            menuItem("Les ingrédients de cette appli", icon = icon("github"), href = "https://github.com/jpivard/StageINSEE")
        )
    ),
    
    
    
    dashboardBody(
        
        tabItems(
            
            tabItem(
                "CO2",
                
                box(
                    title = "Emissions de CO2 globales",
                    footer = "",
                    status = "info",
                    solidHeader = TRUE,
                    width = 8,
                    highchartOutput('courbe_emissions')
                ),
                
                box(
                    title = "Evolution des grandeurs associées aux émissions de CO2",
                    footer = "L'équation de Kaya relie quatre grandeurs : contenu en CO2 de l'énergie, intensité énergétique du PIB, PIB par tête, et population. Elle est obtenue par un procédé très simple de multiplication et de division par un même nombre de chaque côté d'une égalité, ce qui permet d'en déduire une décomposition comptable des émissions de CO2.",
                    status = "info",
                    solidHeader = TRUE,
                    width = 8,
                    highchartOutput('courbe_Kaya')
                ),
                
                tabBox(
                    title = "Intensités carbone de la production",
                    width = 8,
                    tabPanel(title = "Intensité carbone du PIB", highchartOutput('courbe_intensite_carbone')),
                    tabPanel(title = "Intensité carbone de l'énergie", highchartOutput('courbe_intensite_carbone_energie'))
                ),
                
                
                box(
                    title = "Répartition sectorielle des émissions",
                    footer = " ",
                    status = "info",
                    solidHeader = TRUE,
                    width = 8,
                    plotlyOutput('emissions_secteurs')
                ),
                
                box(
                    width = 4,
                    selectInput("secteur", "Secteur choisi", 
                                choices = c("Tous les secteurs", unique(dfshiny2$secteur)))
                ),
                
                
            ),
            
            
            tabItem(
                "energie",
                
                tabBox(
                    title = "Analyse par sources d'énergie",
                    width = 8,
                    tabPanel(title = "Répartition de la consommation par sources", plotlyOutput('conso_sources_energies_plot')),
                    tabPanel(title = "Répartition de la production par sources",plotlyOutput('prod_sources_energies_plot')),
                    tabPanel(title = "Déséquilibre consommation/production", plotlyOutput('deseq_sources_energies_plot'))
                ),
                
                box(
                    width = 4,
                    selectInput("source", "Source d'énergie choisie", 
                                choices = c("Toutes", unique(dfshiny7$source)))
                ),
                
                
                box(
                    title = "Production d'énergie nucléaire en France et comparaison avec l'Allemagne",
                    status = "info",
                    solidHeader = TRUE,
                    width = 8,
                    plotlyOutput('nucl_plot')
                ),
                
                
                tabBox(
                    title = "Energies renouvelables",
                    width = 8,
                    tabPanel(title = "Consommation d'énergies renouvelables (en valeur)",plotlyOutput('conso_ER_Fr_All_plot')),
                    tabPanel(title = "Consommation d'énergies renouvelables (en proportion de l'énergie consommée totale)",plotlyOutput('part_conso_ER_plot')),
                    tabPanel(title = "Production d'énergies renouvelables (en valeur)",plotlyOutput('prod_ER_Fr_All_plot')),
                    tabPanel(title = "Production d'énergies renouvelables (en proportion de l'énergie produite totale)", plotlyOutput('part_prod_ER_plot'))
                ),
                
                
            ),
            
            
            
            tabItem(
                "investissements",
                
                tabBox(
                    title = "Investissements annuels par secteurs",
                    width = 8,
                    tabPanel(title = "Montants d'investissement public actuel", plotlyOutput('plot_inv_secteurs_1')),
                    tabPanel(title = "Nouveaux objectifs d'investissement public",plotlyOutput('plot_inv_secteurs_2')),
                    tabPanel(title = "Investissements privés supplémentaires attendus", plotlyOutput('plot_inv_secteurs_3'))
                ),
                
                
                valueBox(
                    value = "6,741 milliards",
                    subtitle = "Dépenses d'investissements dans les énergies renouvelables",
                    icon = icon("euro"),
                    color = "green",
                    width = 4
                ),
                
                valueBox(
                    value = "4,590 milliards",
                    subtitle = "Subventions aux énergies renouvelables",
                    icon = icon("euro"),
                    color = "green",
                    width = 4
                ),
                
                valueBox(
                    value = "126 millions",
                    subtitle = "Dépenses publiques de R&D dans les énergies renouvelables",
                    icon = icon("euro"),
                    color = "green",
                    width = 4
                ),
                
                
                
                tabBox(
                    title = "Investissements par financeurs",
                    width = 8,
                    tabPanel(title = "Investissements historiques(2016-2018)", plotlyOutput('financeurs_plot_1')),
                    tabPanel(title = "Investissements à court terme (jusque 2023)",plotlyOutput('financeurs_plot_2')),
                    tabPanel(title = "Investissements à moyen terme (2024-2028)", plotlyOutput('financeurs_plot_3'))
                ),
                
                
            ),
            
            tabItem(
                "fin_verte",
                
                
                
                tabBox(
                    title = "Financements par financeurs",
                    width = 12,
                    tabPanel(title = "Financements historiques(2016-2018)", plotlyOutput('financeurs_plot_4')),
                    tabPanel(title = "Financements à court terme (jusque 2023)",plotlyOutput('financeurs_plot_5')),
                    tabPanel(title = "Financements à moyen terme (2024-2028)", plotlyOutput('financeurs_plot_6'))
                ),
                
                
                
                
                infoBox(  
                    title = "Investissements climat",
                    value = "33 milliards d'euros annuels",
                    subtitle = "Entre 2016 et 2018",
                    icon = icon("line-chart"),
                    fill = TRUE,
                    color = "green",
                    width = 4
                ),
                
                infoBox(  
                    title = "Besoins d'investissements climat à court terme",
                    value = "50 milliards d'euros annuels",
                    subtitle = "Entre 2019 et 2023",
                    icon = icon("line-chart"),
                    fill = TRUE,
                    color = "green",
                    width = 4
                ),
                
                infoBox(  
                    title = "Besoins d'investissements climat à moyen terme",
                    value = "70 milliards d'euros annuels",
                    subtitle = "Entre 2024 et 2028",
                    icon = icon("line-chart"),
                    fill = TRUE,
                    color = "green",
                    width = 4
                ),
                
                infoBox(  
                    title = "Proportion d'obligations vertes",
                    value = "Moins d'1%",
                    subtitle = "des obligations mondiales en 2016",
                    icon = icon("line-chart"),
                    fill = TRUE,
                    color = "green",
                    width = 4
                ),
                
                
                infoBox(  
                    title = "Proportion de prêts verts",
                    value = "Entre 5 et 10%",
                    subtitle = "des prêts bancaires mondiaux en 2016",
                    icon = icon("line-chart"),
                    fill = TRUE,
                    color = "green",
                    width = 4
                ),
                
                infoBox(  
                    title = "Participation dans des actifs d'infrastructure verte",
                    value = "Moins d'1%",
                    subtitle = "des participations d'investisseurs institutionnels mondiaux en 2016",
                    icon = icon("line-chart"),
                    fill = TRUE,
                    color = "green",
                    width = 4
                ),
                
                infoBox(  
                    title = "Principal domaine d'investissement vert cité",
                    value = "Energies renouvelables, 75%",
                    subtitle = "des fonds européens du panel étudié par Novethic en 2016",
                    icon = icon("line-chart"),
                    fill = TRUE,
                    color = "green",
                    width = 4
                ),
                
                
                infoBox(  
                    title = "Encours total des fonds verts français",
                    value = "4,6 milliards d'euros",
                    subtitle = "Soit 20% des fonds européenns du panel étudié par Novethic en 2016",
                    icon = icon("line-chart"),
                    fill = TRUE,
                    color = "green",
                    width = 4
                ),
                
                
                infoBox(  
                    title = "Fonds durables au 31 décembre 2019",
                    value = "208 milliards d'euros",
                    subtitle = "Montant de l'encours mondial selon Novethic",
                    icon = icon("line-chart"),
                    fill = TRUE,
                    color = "green",
                    width = 4
                ),
                
                
                
                
                
                
                
                h3(paste0("Répartition mondiale des obligations vertes par émetteurs"),align = 'center'),
                img(src = "Figure répartition obligations vertes par émetteurs.png", height = 400, width = 400),
                
                
                h3(paste0("Répartition géographique du marché des fonds verts"),align = 'center'),
                img(src = "Figure répartition géographique marché fonds verts.png", height = 400, width = 1000),
                
                
                h3(paste0("Evolution des encours de fonds verts"),align = 'center'),
                img(src = "Figure évolution des encours de fonds verts.png", height = 400, width = 800),
                
                
                h3(paste0("Evolution des encours par types de fonds"),align = 'center'),
                img(src = "Figure évolution des encours par types de fonds.png", height = 400, width = 400),
                
                
                h3(paste0("Evolution du marché des fonds verts par adéquation"),align = 'center'),
                img(src = "Figure évolution du marché des fonds verts par adéquation.png", height = 400, width = 400),
                
                
                
                
                
                
                tabItem(
                    "infos",
                    
                    #Ici : ajouter les sources des bases de données
                    #Ainsi que quelques explications/définitions si besoin
                    #Et mettre le lien vers le document Overleaf
                    
                    
                )
                
                
            )
            
        )
        
    ),
    
    title = "Quelques données sur les enjeux de la transition écologique",
    skin = "green"
    
    
)



######### Définition de la logique du serveur #######

server <- function(input, output) {
    
    # ------- 1. Emissions de CO2 -------------------
    
    #Courbe sur les émissions globales
    
    dfshiny1<- df_CO2_fr_all  %>% 
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
    
    #Courbe sur la décomposition comptable des émissions de CO2
    
    dfshiny5 <- df_Kaya %>% select(-CO2)
    
    output$courbe_Kaya <-renderHighchart({
        highchart() %>%
            hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
            hc_chart(type = 'line') %>%
            hc_series( list(name = 'Intensité carbone énergie', data =dfshiny5$`Contenu CO2 energie`, color='red', marker = list(symbol = 'circle')),
                       list(name = 'Intensité énergétique du PIB', data =dfshiny5$`Intensite_energetique_PIB`, color = 'blue', marker = list(symbol = 'circle')),
                       list(name = 'PIB par tête', data =dfshiny5$`PIB par tete`, color = 'orange', marker = list(symbol = 'circle')),
                       list(name = 'Population', data =dfshiny5$Population, color = 'black', marker = list(symbol = 'circle') )  )  %>%
            hc_xAxis( categories = unique(dfshiny5$Annee) ) %>%
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
   
    
    #Courbe sur l'intensité carbone du PIB
    
    dfshiny3 <- df_intensite_PIB_past %>% mutate(annee=rep(seq(1990,2016),3))%>%
        pivot_wider(names_from =pays, values_from =valeur)
    
    output$courbe_intensite_carbone <-renderHighchart({
        highchart() %>%
            hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
            hc_chart(type = 'line') %>%
            hc_series( list(name = 'France', data =dfshiny3$FR, color='blue', marker = list(symbol = 'circle')),
                       list(name = 'Allemagne', data =dfshiny3$DE, color = 'red', marker = list(symbol = 'circle')),
                       list(name = 'Union Européenne', data =dfshiny3$EU, color = 'green', marker = list(symbol = 'circle') )  )  %>%
            hc_xAxis( categories = unique(dfshiny3$annee) ) %>%
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
    
    dfshiny4 <- df_intensite_energie_past %>% mutate(annee=rep(seq(1990,2015),3))%>%
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
    
    
    
    
    #Figure sur la répartition des émissions par secteurs
    
    dfshiny2 <- df_secteurs_long
    
    output$emissions_secteurs <- renderPlotly({
        
        if(input$secteur == "Tous les secteurs"){
            data = dfshiny2
        } else {
            data = dfshiny2 %>%
                filter(secteur== input$secteur)%>%
                group_by(Annee)
        }
        
        emissions_secteurs <- ggplot(data, aes(x=Annee,y= value)) +
            geom_bar(aes (x=Annee, y =value, fill=secteur),stat = "identity", position = "stack")+
            scale_fill_manual(values = c("green","brown","orange","blue"),labels = c("Industrie de l'energie","Industrie manufacturiere et construction", "Residentiel et Tertiaire", "Transports"))+
            labs(x="Année",y="Mtoe")+
            theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))+
            guides(fill=guide_legend(nrow=2,byrow=TRUE))
        
        
    })
    
    # ------- 2. L'énergie en France ------------------------------
    
    
    #Figures sur la répartition de la consommation et de la production d'énergie par sources en France
    
    dfshiny6<- df_conso_energie_source_fr_long 
    
    dfshiny7<- df_prod_energie_source_fr_long
    
    #Consommation
    
    donnees_sources_energie_conso <- reactive( {
        if (input$source == "Toutes") {
            data_conso = dfshiny6
            
        } else {
            data_conso = dfshiny6 %>%
                filter(source == input$source) %>%
                group_by(Annee)
            
        }
        
        
    })
    
    output$conso_sources_energies_plot<- renderPlotly({
        conso_sources_energies_plot <- ggplot( donnees_sources_energie_conso(), aes(x=Annee,y=value, fill=source))+ 
            geom_bar(aes (x=Annee, y =value, fill=source),stat = "identity", position = "stack")+
            scale_fill_manual(values = c("brown","blue","green","orange","black"),labels = c("Pétrole","Nucléaire","Charbon","Gaz","Hydroélectricité"))+
            labs(x="Année",y="Mtoe")+
            theme_gray()
        ggplotly(conso_sources_energies_plot)
        
    })
    
    
    #Prodution
    
    donnees_sources_energie_prod <- reactive( {
        if (input$source == "Toutes") {
            data_prod = dfshiny7 
            
        } else {
            
            data_prod = dfshiny7 %>%
                filter(source == input$source) %>%
                group_by(Annee)
            
        }
        
        
    })
    
    
    output$prod_sources_energies_plot<- renderPlotly({
        
        prod_sources_energies_plot <- ggplot(donnees_sources_energie_prod(), aes(x=Annee,y=value, fill=source))+ 
            geom_bar(stat = "identity", position = "stack")+
            scale_fill_manual(values = c("brown","blue","green","orange","black"),labels = c("Pétrole","Nucléaire","Charbon","Gaz","Hydroélectricité"))+
            labs(x="Année",y="Mtoe")+
            theme_gray()
        ggplotly(prod_sources_energies_plot)
        
    })
    
    
    #Déséquilibre consommation/production
    
    
    output$deseq_sources_energies_plot<- renderPlotly({
        dfshiny8<-df_deseq_conso_prod
        deseq_sources_energies_plot <- ggplot(dfshiny8, aes(x=Annee,y=desequilibre, fill=source))+ 
            geom_bar(aes (x=Annee, y =desequilibre, fill=source),stat = "identity", position = "stack")+
            scale_fill_manual(values = c("brown","blue","black"),labels = c("Pétrole","Charbon","Gaz"))+
            labs(x="Année",y="Mtoe")+
            theme_gray()
        ggplotly(deseq_sources_energies_plot)
        
    })
    
    
    #On remarque que pour le nucléaire et l'hydroélectricité, il y a équilibre entre conso et prod, donc on ne les représente pas
    #sur la figure des déséquilibres. (Note à ajouter à la figure sur Shiny)
    
    
    
    #Figure comparant la production nucléaire en France et en Allemagne
    
    
    output$nucl_plot <- renderPlotly({
        dfshiny9 <- df_nucl%>% 
            filter(annee %in% c(1991:2017))
        nucl_plot <- ggplot(dfshiny9, aes(x=annee, y=quantite_produite, fill=pays))+
            geom_bar(width = 1, stat = "identity")+
            scale_fill_manual(values = c("#56B4E9","#E69F00"),labels = c("France","Allemagne"))+
            labs(x="Année", y="Quantité produite(en millions de KwH)")+
            theme_gray()
        ggplotly(nucl_plot)
        
    })
    
    
    #Comparaison de la consommation et de la production d'ENR en France et en Allemagne 
    
    
    #Consommation
    
    output$conso_ER_Fr_All_plot <- renderPlotly({
        dfshiny10 <- df_conso_enr_fr_all_long
        conso_ER_Fr_All_plot <- ggplot(dfshiny10, aes(x=annee,y=value, color=consommation_finale_energies_renouvelables)) +
            scale_color_manual(values = c("#E69F00", "#56B4E9"),labels = c("Allemagne","France"))+
            geom_line()+
            labs(x="Pays", y="Valeur(en TEP)")+
            theme_gray()
        ggplotly(conso_ER_Fr_All_plot)
        
    })
    
    
    #Production
    
    output$prod_ER_Fr_All_plot <- renderPlotly({
        dfshiny11 <- df_prod_enr_long
        prod_ER_Fr_All_plot <- ggplot(dfshiny11, aes(x=annee,y=value, color=production_primaire_energies_renouvelables)) +
            scale_color_manual(values = c("#E69F00", "#56B4E9"),labels = c("Allemagne","France"))+
            geom_line()+
            labs(x="Pays", y="Valeur(en TEP)")+
            theme_gray()
        ggplotly(prod_ER_Fr_All_plot)
        
    })
    
    
    #Figure comparant les parts des ENR dans la consommation en France et dans d'autres pays européens, et leur évolution
    
    output$part_conso_ER_plot <-  renderPlotly({
        dfshiny12 <- df_part_enr_conso_long
        part_conso_ER_plot <-  ggplot(dfshiny12, aes(x=Annee,y=part_energies_renouvelables_conso_primaire, color=pays),lwd=2) +
            scale_color_manual(values = c("#E69F00", "#56B4E9","red","#009E73"),labels = c("Allemagne","France","Italie","UE"))+
            geom_line()+
            labs( x="Année", y="en pourcentage")+
            theme(plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))
        ggplotly(part_conso_ER_plot)
        
    })
    
    #Figure comparant les parts des ENR dans la production en France et dans d'autres pays européens, et leur évolution
    
    output$part_prod_ER_plot <-  renderPlotly({
        dfshiny13 <- df_part_enr_prod_long
        part_prod_ER_plot <-  ggplot(dfshiny13, aes(x=Annee,y=part_energies_renouvelables_prod_primaire, color=pays),lwd=2) +
            scale_color_manual(values = c("#E69F00", "#56B4E9","red","#009E73"),labels = c("Allemagne","France","Italie","UE"))+
            geom_line()+
            labs( x="Année", y="en pourcentage")+
            theme(plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))
        ggplotly(part_prod_ER_plot)
        
    })
    
    
    # -------- 3.Investissments climat ------------------
    
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
                   aes(x=Investissement_supplementaire_annuel_genere_attendu, 
                       y = 0, 
                       group = Secteur, 
                       text = Secteur
                   )) +
            geom_point(aes(size = Investissement_supplementaire_annuel_genere_attendu , fill = Secteur), 
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
    
    dfinv_3_plot1 <- dfinv_3 %>% filter(!is.na(`Investissements_historiques_2016_2018`))
    
    output$financeurs_plot_1<- renderPlotly({
        financeurs_plot_1 <- ggplot(dfinv_3_plot1, aes(x=Financeurs,y=Investissements_historiques_2016_2018)) +
            geom_bar(aes (x=Financeurs, y =Investissements_historiques_2016_2018, fill=Financeurs),stat = "identity", position = "stack")+
            scale_fill_manual(values = c("green","brown","orange","blue","red","yellow"),labels = c("Etat_et_agences","Collectivites_territoriales","Bailleurs_sociaux","Gestionnaires_infrastructures","Entreprises","Menages"))+
            theme(legend.position='none') +
            labs(x="Financeur",y="milliards d'euros")
        
    })
    
    
    dfinv_3_plot2 <- dfinv_3 %>% filter(!is.na(`Investissements_court_terme`))
    
    output$financeurs_plot_2<- renderPlotly({
        financeurs_plot_2 <- ggplot( dfinv_3_plot2, aes(x=Financeurs,y=Investissements_court_terme)) +
            geom_bar(aes (x=Financeurs, y =Investissements_court_terme, fill=Financeurs),stat = "identity", position = "stack")+
            scale_fill_manual(values = c("green","brown","orange","blue","red","yellow"),labels = c("Etat_et_agences","Collectivites_territoriales","Bailleurs_sociaux","Gestionnaires_infrastructures","Entreprises","Menages"))+
            theme(legend.position='none') +
            labs(x="Financeur",y="milliards d'euros")
        
    })
    
    
    dfinv_3_plot3 <- dfinv_3 %>% filter(!is.na(`Investissements_moyen_terme`))
    
    output$financeurs_plot_3<- renderPlotly({
        financeurs_plot_3 <- ggplot( dfinv_3_plot3, aes(x=Financeurs,y=Investissements_moyen_terme)) +
            geom_bar(aes (x=Financeurs, y =Investissements_moyen_terme, fill=Financeurs),stat = "identity", position = "stack")+
            scale_fill_manual(values = c("green","brown","orange","blue","red","yellow"),labels = c("Etat_et_agences","Collectivites_territoriales","Bailleurs_sociaux","Gestionnaires_infrastructures","Entreprises","Menages"))+
            theme(legend.position='none') +
            labs(x="Financeur",y="milliards d'euros")
        
    })
    
    
    # --------- 4. Finance verte -----------------------
    
    dffin <- dfinv_3 
    
    output$financeurs_plot_4<- renderPlotly({
        financeurs_plot_4 <- ggplot(dffin, aes(x=Financeurs,y=Financements_historiques)) +
            geom_bar(aes (x=Financeurs, y =Financements_historiques, fill=Financeurs),stat = "identity", position = "stack")+
            scale_fill_manual(values = c("grey","green","brown","orange","blue","pink", "purple", "black", "red","yellow"),labels = c("Fonds_européens","Etat_et_agences","Collectivites_territoriales","Bailleurs_sociaux","Gestionnaires_infrastructures","Banques_publiques", "Banques_commerciales","Marches_financiers", "Entreprises","Menages"))+
            theme(legend.position='none') +
            labs(x="Financeur",y="milliards d'euros")
        
    })
    
    
    
    output$financeurs_plot_5<- renderPlotly({
        financeurs_plot_5 <- ggplot(dffin, aes(x=Financeurs,y=Financements_court_terme)) +
            geom_bar(aes (x=Financeurs, y =Financements_court_terme, fill=Financeurs),stat = "identity", position = "stack")+
            scale_fill_manual(values = c("grey","green","brown","orange","blue","pink", "purple", "black", "red","yellow"),labels = c("Fonds_européens","Etat_et_agences","Collectivites_territoriales","Bailleurs_sociaux","Gestionnaires_infrastructures","Banques_publiques", "Banques_commerciales","Marches_financiers", "Entreprises","Menages"))+
            theme(legend.position='none') +
            labs(x="Financeur",y="milliards d'euros")
        
    })
    
    
    output$financeurs_plot_6<- renderPlotly({
        financeurs_plot_6 <- ggplot(dffin, aes(x=Financeurs,y=Financements_moyen_terme)) +
            geom_bar(aes (x=Financeurs, y =Financements_moyen_terme, fill=Financeurs),stat = "identity", position = "stack")+
            scale_fill_manual(values = c("grey","green","brown","orange","blue","pink", "purple", "black", "red","yellow"),labels = c("Fonds_européens","Etat_et_agences","Collectivites_territoriales","Bailleurs_sociaux","Gestionnaires_infrastructures","Banques_publiques", "Banques_commerciales","Marches_financiers", "Entreprises","Menages"))+
            theme(legend.position='none') +
            labs(x="Financeur",y="milliards d'euros")
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)