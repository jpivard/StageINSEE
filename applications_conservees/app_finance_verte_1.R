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
ui <- dashboardPage(
    dashboardHeader(title = "Un aperçu de la finance verte et des investissements pour le climat en France"),
    # titlePanel("Un aperçu de la finance verte et des investissements pour le climat en France"),
    
    # mainPanel(
        dashboardSidebar(),
        dashboardBody(
            
            tabItems(
                ## 3.1 Main dashboard ----------------------------------------------------------
                tabItem( tabName = 'dashboard',
                         
                         ),
            
        h1(paste0("Les investissements pour le climat")) ,
        fluidRow(
            valueBoxOutput("inv_2016_2018_box"),
            valueBoxOutput("inv_2019_2023_box"),
            valueBoxOutput("inv_2024_2028_box")
        ),
        
   ) 
)
        
)

# Define server logic 
server <- function(input, output) {

    # output$box_inv <- renderUI({
    #     div(
    #         style = "position: relative; backgroundColor: #ecf0f5",
    #         tabBox(
    #             id = "box_pat",
    #             width = NULL,
    #             height = 320,
    #             tabPanel(
    #                 title = "Montant total des investissements pour le climat par périodes",
    #                 div(
    #                     style = "position: absolute; left: 0.5em; bottom: 0.5em;",
    #                     introBox(data = dfinv_1,
    #                              dropdown(
    #                                  radioGroupButtons(
    #                                      inputId = "box_pat1",
    #                                      label = NULL, 
    #                                      choices = c("Show all", "Show top 10 only"), 
    #                                      selected = "Show all", 
    #                                      direction = "vertical"
    #                                  ),
    #                                  size = "xs",
    #                                  icon = icon("gear", class = "opt"), 
    #                                  up = TRUE
    #                              )
    #                     )
    #                 ),
    #                 withSpinner(
    #                     plotlyOutput("plot_pat_select", height = 230),
    #                     type = 4,
    #                     color = "#d33724", 
    #                     size = 0.7 
    #                 )
    #             )
    #         )
    #     )
    # })
   

    # output$inv_totaux <- renderPlotly({
    #   inv_totaux <- ggplot(dfinv_1, aes(x=annee_debut_periode,y=Investissements_annuels_en_milliards_euros)) +
    #         geom_bar(aes (x=annee_debut_periode,y=Investissements_annuels_en_milliards_euros, fill=annee_debut_periode),stat = "identity", position = "stack")+
    #         scale_fill_manual(values = c("orange","blue","green"),labels = c("2016-2018","2019-2023", "2024-2028"))+
    #         labs(x="Année",y="milliard d'euros")
    #          theme_gray()
    #   ggplotly(inv_totaux)
    # })
    
    
    #Données sur les investissements et besoins d'investissements par période
    
    inv_2016_2018 <- dfinv_1 %>%
        filter( annee_debut_periode == 2016) %>%
        dplyr::select(Investissements_annuels_en_milliards_euros) %>%
        as.numeric
     
    inv_2019_2013 <- dfinv_1 %>%
        filter( annee_debut_periode == 2019) %>%
        dplyr::select(Investissements_annuels_en_milliards_euros) %>%
        as.numeric
    
    inv_2024_2028 <- dfinv_1 %>%
        filter( annee_debut_periode == 2024) %>%
        dplyr::select(Investissements_annuels_en_milliards_euros) %>%
        as.numeric
    
    output$inv_2016_2018_box <- renderValueBox({
        valueBox(
            VB_style( paste0(format( inv_2016_2018,big.mark=','), " mds€" ),  "font-size: 60%;"  ),
            VB_style( paste0("2016-2018")  ), 
            color = "green"
        )
    })
    
    output$inv_2019_2023_box <- renderValueBox({
        valueBox(
            VB_style( paste0(format( inv_2019_2023,big.mark=','), " mds€" ),  "font-size: 60%;"  ),
            VB_style( paste0("2019-2023")  ), 
            color = "green"
        )
    })
    
    output$inv_2024_2028_box <- renderValueBox({
        valueBox(
            VB_style( paste0(format( inv_2024_2028,big.mark=','), " mds€" ),  "font-size: 60%;"  ),
            VB_style( paste0("2024-2028")  ), 
            color = "green"
        )
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
