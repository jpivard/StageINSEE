

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(highcharter)
library(stringr)
library(withr)
library(treemap)
library(DT)
library(shinyBS)
library(shinyjs)
library(WDI)
library(geosphere)
library(magrittr)
library(shinycssloaders)
library(timevis)

options(spinner.color="#006272")

#Charger les données en rds ou rda 


## setup global variables : pas utile je pense
# maxYear <- tolower(paste0(dtf_shiny_commodity_service_ex$Note[1],' ', max(dtf_shiny_commodity_service_ex$Year)))
# maxYear <- gsub('q1', 'March', maxYear)
# maxYear <- gsub('q2', 'June', maxYear)
# maxYear <- gsub('q3', 'September', maxYear)
# maxYear <- gsub('q4', 'December', maxYear)
# 
# maxYear_lb <- gsub('year ended March', 'Mar', maxYear)
# maxYear_lb <- gsub('year ended June', 'Jun', maxYear_lb )
# maxYear_lb <- gsub('year ended September', 'Sep', maxYear_lb )
# maxYear_lb <- gsub('year ended December', 'Dec', maxYear_lb )
# 
# maxYear_lb <- paste0( substr(maxYear_lb, 1, 3 ),
#                       " ",
#                       substr(maxYear_lb, nchar(maxYear_lb)-1, nchar(maxYear_lb) ))  


## load functions
# source('helper_funs.R')

## Présentation des sources de données ( à adapter ) ---------------------
data_source <- function(){
    fluidRow(
        h2(paste0('Quelles sont les sources de données')),
        #tags$ol(
        tags$li( "Total goods and services exports and imports are sourced from ", 
                 tags$a(tags$i("BPM6 Quarterly, Balance of payments major components (Qrtly-Mar/Jun/Sep/Dec)"),
                        href = "http://archive.stats.govt.nz/infoshare/SelectVariables.aspx?pxID=aa7f4009-2651-404c-b3b6-e24ea781d803",
                        target = "_blank"), 
                 ", a table under Economic indicators and Balance of Payments - BOP from Inforshare Statistics New Zealand."
        ),
        tags$li("Goods exports and imports by country and commodity are sourced and compiled from ",
                tags$a("the overseas merchiandise trade datasets",
                       href = "http://archive.stats.govt.nz/browse_for_stats/industry_sectors/imports_and_exports/overseas-merchandise-trade/HS10-by-country.aspx",
                       target = "_blank"),
                " from Statistics New Zealand."
        ),
        tags$li("Services exports and imports by country are sourced from ",
                tags$a(tags$i("BPM6 Services by country, year ended in quarter (Qrtly-Mar/Jun/Sep/Dec)") ,
                       href = "http://archive.stats.govt.nz/infoshare/SelectVariables.aspx?pxID=e17bdb70-8fff-4f11-baf1-eb732a963099",
                       target = "_blank"),
                ", a table under Economic indicators and Balance of Payments - BOP from Inforshare Statistics New Zealand. For countries whose data are not available from this source, ",
                tags$a( tags$i("Goods and services trade by country: Year ended Qtr Year map data CSV"),
                        href = "https://www.stats.govt.nz/information-releases/goods-and-services-trade-by-country-year-ended-june-2018",
                        target = "_blank" ),
                "is then used."
        ),
        tags$li("Data used in the global trade analysis are sourced from ",
                tags$a(tags$i("UN Comtrade, International Trade Statistics Database") ,
                       href = "https://comtrade.un.org/",
                       target = "_blank"),
                ", by using its ", 
                tags$a(tags$i("API"),
                       href = "https://comtrade.un.org/data/dev/portal",
                       target = "_blank"),
                " via an R package called ",
                tags$a(tags$i("comtradr"),
                       href = "https://cran.r-project.org/web/packages/comtradr/index.html",
                       target = "_blank"),
                ". Please note that the maximum number of queries is 100 per hour."
        ),
        tags$li("Directional basis stock of direct investment are sourced from ",
                tags$a(tags$i("BPM6 Annual, Directional basis stock of direct investment by country (Annual-Mar)") ,
                       href = "http://archive.stats.govt.nz/infoshare/SelectVariables.aspx?pxID=e17bdb70-8fff-4f11-baf1-eb732a963099",
                       target = "_blank"),
                ", a table under Economic indicators and International Investment Position - IIP from Inforshare Statistics New Zealand."),
        tags$li("New Zealand visitor travelling overseas data is sourced from ",
                tags$a(tags$i("NZ-resident traveller departures by EVERY country of main dest and purpose (Qrtly-Mar/Jun/Sep/Dec)") ,
                       href = "http://archive.stats.govt.nz/infoshare/SelectVariables.aspx?pxID=e17bdb70-8fff-4f11-baf1-eb732a963099",
                       target = "_blank"),
                ", a table under Tourism and International Travel and Migration - ITM from Inforshare Statistics New Zealand."),
        tags$li("Foreign visitor travelling to New Zealand data is sourced from ",
                tags$a(tags$i("Visitor arrivals by EVERY country of residence and purpose (Qrtly-Mar/Jun/Sep/Dec)") ,
                       href = "http://archive.stats.govt.nz/infoshare/SelectVariables.aspx?pxID=e17bdb70-8fff-4f11-baf1-eb732a963099",
                       target = "_blank"),
                ", a table under Tourism and International Travel and Migration - ITM from Inforshare Statistics New Zealand.")
        #)
    )
}


#Ajouter un court texte de présentation dans le cadre du stage et renvoyer à l'Overleaf
#Eventuellement préciser quelques définitions ?
#S'inspirer du code du fichier 'helper_funs' 

#Rq : ils ont regroupé tous les traitements sur les données ( et les fonctions associées ) dans un fichier à part qu'ils sourcent ensuite 
#juste avant l'ui, voir si je fais pareil


# Define UI 

## 1. header -------------------------------

# header <- 
#     dashboardHeader(  title = HTML("Quelques données sur les enjeux de la transition écologique"), 
#                       disable = FALSE, 
#                       titleWidth  = 550,
#                       dropdownMenuCustom( type = 'message',
#                                           customSentence = customSentence_share,
#                                           icon = icon("question"),
#                                           messageItem(
#                                               from = 'GitHub',
#                                               message = "",
#                                               icon = icon("GitHub"),
#                                               href = "https://github.com/jpivard/StageINSEE"
#                            ),
#                       
#                       
#                        )
    
    
   # )

## 2. siderbar ------------------------------


siderbar <- 
    dashboardSidebar( width = 200,
                     
                       sidebarMenu(
                          id = 'sidebar',
                          style = "position: relative; overflow: visible;",
                          #style = "position: relative; overflow: visible; overflow-y:scroll",
                          #style = 'height: 90vh; overflow-y: auto;',
                          
                     ## 1st tab shows the CO2/energy dashboard -----------
                     menuItem( "CO2 et Energie", tabName = 'first_dashboard', icon = icon('dashboard') ),
                     
                     div( id = 'menu_deroulant_1',
                          conditionalPanel("input.sidebar === 'CO2_energie'",
                             selectizeInput("Choisissez les données /n que vous souhaitez observer",
                                            choices = list ( "Emissions globales" = 1, 
                                                             "Décomposition comptable des émissions" = 2,
                                                             "Emissions par secteurs"=3,
                                                             "Production et consommation d'énergie par sources"=4,
                                                             "Focus sur les énergies décarbonées" =5 ),
                                                          selected = 1,  width = "200px",
                                                          multiple = T),  
                             
                             actionButton('btn_selection_1', 
                                          paste0('Sélectionner'),
                         
                             )
                     ),
                     
                     ## Show panel only when Commodity intelligence sidebar is selected
                     useShinyjs(),
                     
                     ## 2nd tab shows the green finance dashboard --------------
                     menuItem("Finance verte", tabName = 'second_dashboard', icon = icon('globe') ),
                     
                     div( id = 'menu_deroulant_2',
                          conditionalPanel("input.sidebar === 'finance_verte'",
                                           selectizeInput("Choisissez les données /n que vous souhaitez observer",
                                                          choices = list ( "Investissements climat par secteurs" = 1, 
                                                                           "Investissements climat par financeurs" = 2,
                                                                           "Financements par financeurs"=3,
                                                                           "Fonds d'investissements verts"=4 ),
                                                          selected = 1,  width = "200px",
                                                          multiple = T),  
                                           
                                           actionButton('btn_selection_2', 
                                                        paste0('Sélectionner'),
                                                        
                                           )
                          ),
        
                      
                     ## Show panel only when  sidebar is selected
                     useShinyjs(),
        
                     
                     ## 3rd tab Data source, definition , i.e., help ---------------
                     menuItem( "FAQs", tabName = 'help', icon = icon('question-circle') ),
                
                
                     ),
                
                ## 3. body --------------------------------
                body <- dashboardBody( 
                    
                    # ## 3.0. CSS styles in header ----------------------------
                    # tags$head(
                    #     # ## JS codes
                    #     # tags$script(src = "fixedElement.js" ),
                    #     # tags$style(HTML(".scroller_anchor{height:0px; margin:0; padding:0;}; 
                    #     #                  .scroller{background: white; 
                    #     #                   border: 1px solid #CCC; 
                    #     #                   margin:0 0 10px; 
                    #     #                   z-index:100; 
                    #     #                   height:50px; 
                    #     #                   font-size:18px; 
                    #     #                   font-weight:bold; 
                    #     #                   text-align:center; 
                    #     #                  width:500px;}")),
                    #     
                    #     #tags$script(src = "world.js" ),
                    #     tags$script("document.title = 'Données sur les enjeux de la transition écologique'"),
                    #     
                    #     ### Styles 
                    #     tags$style(HTML(".small-box {height: 65px}")),
                    #     tags$style(HTML(".fa { font-size: 35px; }")),
                    #     tags$style(HTML(".glyphicon { font-size: 33px; }")),  ## use glyphicon package
                    #     tags$style(HTML(".fa-dashboard { font-size: 20px; }")),
                    #     tags$style(HTML(".fa-globe { font-size: 20px; }")),
                    #     tags$style(HTML(".fa-barcode { font-size: 20px; }")),
                    #     tags$style(HTML(".tab-content { padding-left: 20px; padding-right: 30px; }")) ,
                    #     tags$style(HTML(".fa-wrench { font-size: 15px; }")),
                    #     tags$style(HTML(".fa-refresh { font-size: 15px; }")),
                    #     tags$style(HTML(".fa-search { font-size: 15px; }")),
                    #     tags$style(HTML(".fa-comment { font-size: 20px; }")),
                    #     tags$style(HTML(".fa-share-alt { font-size: 20px; }")),
                    #     tags$style(HTML(".fa-envelope { font-size: 20px; }")),
                    #     tags$style(HTML(".fa-question-circle { font-size: 20px; }")),
                    #     tags$style(HTML(".fa-chevron-circle-down { font-size: 15px; }")),
                    #     tags$style(HTML(".fa-bell { font-size: 17px; }")),
                    #     tags$style(HTML(".fa-check { font-size: 14px; }")),
                    #     tags$style(HTML(".fa-times { font-size: 14px; }")),
                    #     
                    #     #tags$style(HTML(".fa-twitter { font-size: 10px; color:red;}")),
                    #     #tags$style(HTML(".fa-facebook { font-size: 10px; color:red;}")),
                    #     #tags$style(HTML(".fa-google-plus { font-size: 10px; color:red;}")),
                    #     #tags$style(HTML(".fa-pinterest-p { font-size: 10px; color:red;}")),
                    #     #tags$style(HTML(".fa-linkedin { font-size: 10px; color:red;}")),
                    #     #tags$style(HTML(".fa-tumblr { font-size: 10px; color:red;}")),
                    #     tags$style(HTML(".fa-github { font-size: 10px; color:red;}")),
                    #     
                    #     ## modify the dashboard's skin color
                    #     tags$style(HTML('
                    #    /* logo */
                    #    .skin-blue .main-header .logo {
                    #    background-color: #006272;
                    #    }
                    #    /* logo when hovered */
                    #    .skin-blue .main-header .logo:hover {
                    #    background-color: #006272;
                    #    }
                    #    /* navbar (rest of the header) */
                    #    .skin-blue .main-header .navbar {
                    #    background-color: #006272;
                    #    }
                    #    /* active selected tab in the sidebarmenu */
                    #    .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                    #    background-color: #006272;
                    #              }
                    #    ')
                    #     ),
                    #     
                    #     ## modify icon size in the sub side bar menu
                    #     tags$style(HTML('
                    #    /* change size of icons in sub-menu items */
                    #   .sidebar .sidebar-menu .treeview-menu>li>a>.fa {
                    #   font-size: 15px;
                    #   }
                    #   .sidebar .sidebar-menu .treeview-menu>li>a>.glyphicon {
                    #   font-size: 13px;
                    #   }
                    #   /* Hide icons in sub-menu items */
                    #   .sidebar .sidebar-menu .treeview>a>.fa-angle-left {
                    #   display: none;
                    #   } 
                    #   '
                    #     )) ,
                    #     
                    #     tags$style( HTML("hr {border-top: 1px solid #000000;}") ),
                    #     
                    #     ## to not show error message in shiny
                    #     tags$style( HTML(".shiny-output-error { visibility: hidden; }") ),
                    #     tags$style( HTML(".shiny-output-error:before { visibility: hidden; }") ),
                    #     
                    #     ## heand dropdown menu size
                    #     #tags$style(HTML('.navbar-custom-menu>.navbar-nav>li>.dropdown-menu { width:100px;}'))
                    #     tags$style(HTML('.navbar-custom-menu>.navbar-nav>li:last-child>.dropdown-menu { width:10px; font-size:10px; padding:1px; margin:1px;}')),
                    #     tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > h4 {width:0px; font-size:0px; padding:0px; margin:0px;}')),
                    #     tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > p {width:0px; font-size:0px; padding:0px; margin:0px;}'))
                    # ),
                    
                  
                      ## 3.1 Dashboard body --------------
                    tabItems( 
                        
                        tabItem( tabName = 'CO2_energie_dashboard',
                                 
                                 ## contents for the dashboard tab
                                 div(id = 'main_wait_message',
                                     h1('Veuillez patienter quelques secondes',
                                        style = "color:darkblue" , align = "center" ) ,
                                     tags$hr()
                                 ),
                                 
                                     
                                 h1(paste0("Emissions de CO2 et énergie")),
                                 
                                 div ( id ='emissions_CO2',
                                 
                                 h2(paste0("Les émissions de CO2 en France depuis 1980")),
                                 
                                 fluidRow  (em("Vision globale des émissions"),
                                            br(),
                                            column( width = 12,h4("Evolution des émissions globales et comparaison avec l'Allemagne", align = 'center'), highchartOutput('courbe_emissions')),
                                            em("Equation de Kaya et intensités carbone du PIB et de l'énergie"),
                                            br(),
                                            p("L'",strong("équation de Kaya"), "relie quatre grandeurs : contenu en CO2 de l'énergie, intensité énergétique du PIB, PIB par tête, et population. Elle est obtenue par un procédé très simple de multiplication et de division par un même nombre de chaque côté d'une égalité, ce qui permet d'en déduire une décomposition comptable des émissions de CO2. "),
                                            column( width = 12,h4("Evolution des grandeurs reliées aux émissions de CO2", align = 'center'), highchartOutput('courbe_Kaya')),
                                            column( width = 6,h4("Evolution de l'intensité carbone du PIB", align = 'left'), highchartOutput('courbe_intensite_carbone')),
                                            column( width = 6,h4("Evolution de l'intensité carbone de l'énergie",align='right'),highchartOutput('courbe_intensite_carbone_energie')),
                                            em("Répartition sectorielle"),
                                            br(),
                                 column( width = 12,h4("Répartition des émissions par secteurs", align = 'center'), plotlyOutput('emissions_secteurs')),
                                         
                                 ),
                                 
                                 div ( id ='energie', 
                                       
                                            h2(paste0("L'énergie en France et en Europe")),
                                            fluidRow( em("Analyse par sources d'énergies"),
                                                      br(),
                                                      column (width = 6,h4("Répartition de la consommation d'énergie entre les principales sources en France", align = 'center'), plotlyOutput('conso_sources_energies_plot')),
                                                      column (width = 6,h4("Répartition de la production d'énergie entre les principales sources en France", align = 'center'), plotlyOutput('prod_sources_energies_plot')),
                                                      column (width = 12,h4("Déséquilibres consommation/production pour les principales sources d'énergie en France", align = 'center'), plotlyOutput('deseq_sources_energies_plot')),
                                                      p("On remarque que pour le nucléaire et l'hydroélectricité, il y a équilibre entre consommation et production, donc on ne les observe pas
                                                     sur la figure des déséquilibres"),
                                                      # column (width = 12,h4("Comparaison de la consommation d'énergies renouvelables et fossiles", align = 'center'), plotlyOutput('conso_ER_plot')),
                                                      br(),
                                                      em("Energies décarbonées : nucléaire et renouvelable"),
                                                      br(),
                                                      column (width = 12,h4("Comparaison de la production d'énergie nucléaire en France et en Allemagne", align = 'center'), plotlyOutput('nucl_plot')),
                                                      column (width = 6,h4("Comparaison  des niveaux de consommation d'énergies renouvelables en France et en Allemagne", align = 'center'), plotlyOutput('conso_ER_Fr_All_plot')),
                                                      column (width = 6,h4("Comparaison  des niveaux de production d'énergies renouvelables en France et en Allemagne", align = 'center'), plotlyOutput('prod_ER_Fr_All_plot')),
                                                      column (width = 6,h4("Evolution de la part des énergies renouvables dans la consommation finale en France et dans d'autres pays européens", align = 'center'), plotlyOutput('part_conso_ER_plot')),
                                                      column (width = 6,h4("Evolution de la part des énergies renouvables dans la production primaire en France et dans d'autres pays européens", align = 'center'), plotlyOutput('part_prod_ER_plot')),
                                            ),
                                            
                                      )
                    
                                 )
                                 
                                 
                                 
                                 
                           ),
        
        
                 tabItem( tabName = 'finance_verte_dashboard',
             
                 h1(paste0("Un aperçu de la finance verte et des investissements pour le climat en France")),
                 
                 div ( id ='investissements_climat',
                       
                       h2(paste0("Les investissements pour le climat")) ,
                       
                       # fluidRow(
                       #     valueBoxOutput("inv_2016_2018_box"),
                       #     valueBoxOutput("inv_2019_2023_box"),
                       #     valueBoxOutput("inv_2024_2028_box")
                       # ),
                       
                       h3(paste0("Investissement annuel par secteurs")),
                       fluidRow(column( width = 6,h4("Montants d'investissement public actuel", align = 'center'), plotlyOutput('plot_inv_secteurs_1')),
                                column( width = 6,h4("Nouveaux objectifs d'investissement public", align = 'center'), plotlyOutput('plot_inv_secteurs_2')),
                                column( width = 12,h4("Investissements privés supplémentaires attendus", align = 'center'), plotlyOutput('plot_inv_secteurs_3'))),
                       
                       
                       h3(paste0("Investissements par financeurs")),
                       fluidRow(column( width = 4,h4("Investissements historiques(2016-2018)", align = 'center'), plotlyOutput('financeurs_plot_1')),
                                column( width = 4,h4("Investissements à court terme (jusque 2023)", align = 'center'), plotlyOutput('financeurs_plot_2')),
                                column( width = 4,h4("Investissements à moyen terme (2024-2028)", align = 'center'), plotlyOutput('financeurs_plot_3'))),
                       
                 ),
                 
                 
                 div ( id ='finance_verte',
                       
                       h2(paste0("Quelques données sur la finance verte"),align = 'center') ,  
                       
                       h3(paste0("Financements par financeurs"),align = 'center'),
                       fluidRow(column( width = 4,h4("Financements historiques(2016-2018)", align = 'center'), plotlyOutput('financeurs_plot_4')),
                                column( width = 4,h4("Financements à court terme (jusque 2023)", align = 'center'), plotlyOutput('financeurs_plot_5')),
                                column( width = 4,h4("Financements à moyen terme (2024-2028)", align = 'center'), plotlyOutput('financeurs_plot_6'))),
                       
                       
                       
                 h3(paste0("Répartition mondiale des obligations vertes par émetteurs"),align = 'center'),
                       
                img(src = "Figure répartition obligations vertes par émetteurs.png", height = 400, width = 400)),
                 
                 
                 h3(paste0("Répartition géographique du marché des fonds verts"),align = 'center'),
                 
                 img(src = "Figure répartition  géographique marché fonds verts.png", height = 400, width = 400)),
                 
                 
                 h3(paste0("Evolution des encours de fonds verts"),align = 'center'),
                 
                 img(src = "Figure évolution des encours de fonds verts.png", height = 400, width = 800)),
                 
                 
                 h3(paste0("Evolution des encours par types de fonds"),align = 'center'),
                 
                 img(src = "Figure évolution des encours par types de fonds.png", height = 400, width = 400)),
                
                
                h3(paste0("Evolution du marché des fonds verts par adéquation"),align = 'center'),
                
                img(src = "Figure évolution du marché des fonds verts par adéquation.png", height = 400, width = 400))
                
            
        
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


