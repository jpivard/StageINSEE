#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(highcharter)
library(lubridate)
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
options(spinner.color="#006272")
library(timevis)

#Charger les données 
load("list_snz_commodity_ex.rda") ## pre-defined commodity list form SNZ
load("list_snz_commodity_im.rda") ## pre-defined commodity list form SNZ
load("list_country.rda") ## Country grouped by region
load("dtf_shiny_commodity_service_ex.rda") ## principle commodity from StatsNZ -- exports

## setup global variables
maxYear <- tolower(paste0(dtf_shiny_commodity_service_ex$Note[1],' ', max(dtf_shiny_commodity_service_ex$Year)))
maxYear <- gsub('q1', 'March', maxYear)
maxYear <- gsub('q2', 'June', maxYear)
maxYear <- gsub('q3', 'September', maxYear)
maxYear <- gsub('q4', 'December', maxYear)

maxYear_lb <- gsub('year ended March', 'Mar', maxYear)
maxYear_lb <- gsub('year ended June', 'Jun', maxYear_lb )
maxYear_lb <- gsub('year ended September', 'Sep', maxYear_lb )
maxYear_lb <- gsub('year ended December', 'Dec', maxYear_lb )

maxYear_lb <- paste0( substr(maxYear_lb, 1, 3 ),
                      " ",
                      substr(maxYear_lb, nchar(maxYear_lb)-1, nchar(maxYear_lb) ))  


## load functions
source('helper_funs.R')

# Define UI 

header <- 
    dashboardHeader(  title = HTML("New Zealand Trade Intelligence"), 
                      disable = FALSE, 
                      titleWidth  = 550,
                      dropdownMenuCustom(
                          
                          
                          
                      ),
                      
header$children[[2]]$children[[1]] <- tags$a(href='http://www.mbie.govt.nz',
                                     tags$img(src='MBIELogo/logo_reserve_small_corp1.png'),
                                    target = '_blank') #,height='67',width='228.6', align = 'left'
        
        
    )


siderbar <- 
    dashboardSidebar(   dashboardSidebar( 
        width = 200,
        sidebarMenu(
            id = 'sidebar',
            style = "position: relative; overflow: visible;",
            #style = "position: relative; overflow: visible; overflow-y:scroll",
            #style = 'height: 90vh; overflow-y: auto;',
            
            ## 1st tab show the Main dashboard -----------
            menuItem( "Main Dashboard", tabName = 'dashboard', icon = icon('dashboard'),
                      badgeLabel = maxYear_lb, badgeColor = "green" ),
            
            ## add conditional panel to show more
            # conditionalPanel( "input.sidebar === 'dashboard'",
            #                   actionButton("btn_show_more",
            #                                paste0(' Show more details'),
            #                                icon = icon('chevron-circle-down'),
            #                                style='padding-top:0px; padding-bottom:0px;padding-left:3px;padding-right:3px; '
            #                                ) 
            #                   ),
            
            
            #Les deux parties suivantes ne nous intéressent pas en tant que telles mais on peut voir comment structurer
            #l'application en plusieurs onglets 
            
            
            ## 2nd Second tab shows the country/region level tab --------------
            menuItem("Market Intelligence", tabName = 'country_intel', icon = icon('globe') ),
            div( id = 'sidebar_cr',
                 conditionalPanel("input.sidebar === 'country_intel'",
                                  selectizeInput("select_country",
                                                 "Select or search for one or multiple markets", 
                                                 choices =  list_country, 
                                                 selected = NULL,  width = "200px",
                                                 multiple = T), #,
                                  #actionButton('btn_country','Submit')
                                  
                                  ## action button to build report
                                  actionButton('btn_build_country_report', 
                                               paste0('Build Report'),
                                               icon = icon('wrench')),
                                  
                                  ## reset side bar selectoin
                                  actionButton('btn_reset_cr',
                                               'Reset',
                                               icon = icon('refresh') )
                                  
                 )),
            
            ## 3rd tab shows commodity intel ----------
            menuItem( "Commodity Intelligence", tabName = "commodity_intel", icon = icon('barcode'), startExpanded = F,
                      menuSubItem('Exports', tabName = "ci_exports", icon = icon('export', lib = 'glyphicon')),
                      menuSubItem('Imports', tabName = "ci_imports", icon = icon('import', lib = 'glyphicon')),
                      menuSubItem('Intelligence by HS code', tabName = "ci_intel_by_hs", icon = icon("bolt") )
            ),
            
            ## Show panel only when Commodity intelligence sidebar is selected
            useShinyjs(),
            
            ## give sidebar inputs a id so that it can be manipulated by css
            div( id = 'sidebar_ci_exports',
                 conditionalPanel("input.sidebar === 'ci_exports'",
                                  
                                  ## radio buttons to ask user to choose prebuilt commodity groups or build their owns
                                  radioButtons("rbtn_prebuilt_diy_ex",
                                               tags$p("Step 1:",tags$br(),"Select commodities:"), 
                                               choices = c("Pre-defined", 'Self-defined'),
                                               selected = 'Pre-defined',
                                               inline = F,
                                               width = "200px"),
                                  
                                  ## conditional on select pre-built ones 
                                  conditionalPanel( "input.rbtn_prebuilt_diy_ex == 'Pre-defined'",
                                                    selectizeInput("select_comodity_ex",
                                                                   tags$p("Step 2:",tags$br(),"Select or search commodities"), 
                                                                   choices =  list_snz_commodity_ex, 
                                                                   selected = NULL,  width = "200px",
                                                                   multiple = T)
                                  ),
                                  ## conditonal on build your own report
                                  conditionalPanel( "input.rbtn_prebuilt_diy_ex == 'Self-defined'",
                                                    fileInput("file_comodity_ex",
                                                              tags$p("Step 2:",tags$br(),"Upload self-defined HS codes groupings"), 
                                                              accept = c(".csv"),  
                                                              width = "200px",
                                                              multiple = F,
                                                              buttonLabel = 'Upload CSV'
                                                    )
                                  ),
                                  ## action button to build report
                                  actionButton('btn_build_commodity_report_ex', 
                                               paste0('Build Report'),
                                               icon = icon('wrench')),
                                  
                                  ## reset side bar selectoin
                                  actionButton('btn_reset_ci_ex',
                                               'Reset',
                                               icon = icon('refresh') )
                                  
                 )),
            
            ## Show panel only when Commodity intelligence sidebar is selected
            div( id = 'sidebar_ci_imports',
                 conditionalPanel("input.sidebar === 'ci_imports'",
                                  
                                  ## radio buttons to ask user to choose prebuilt commodity groups or build their owns
                                  radioButtons("rbtn_prebuilt_diy_im",
                                               tags$p("Step 1:",tags$br(),"Select commodities:"), 
                                               choices = c("Pre-defined", 'Self-defined'),
                                               selected = 'Pre-defined',
                                               inline = F,
                                               width = "200px"),
                                  
                                  ## conditional on select pre-built ones 
                                  conditionalPanel( "input.rbtn_prebuilt_diy_im == 'Pre-defined'",
                                                    selectizeInput("select_comodity_im",
                                                                   tags$p("Step 2:",tags$br(),"Select or search commodities"), 
                                                                   choices =  list_snz_commodity_im, 
                                                                   selected = NULL,  width = "200px",
                                                                   multiple = T)
                                  ),
                                  ## conditonal on build your own report
                                  conditionalPanel( "input.rbtn_prebuilt_diy_im == 'Self-defined'",
                                                    fileInput("file_comodity_im",
                                                              tags$p("Step 2:",tags$br(),"Upload self-defined HS codes groupings"), 
                                                              accept = c(".csv"),  
                                                              width = "200px",
                                                              multiple = F,
                                                              buttonLabel = 'Upload CSV'
                                                    )
                                  ),
                                  
                                  ## action button to build report
                                  actionButton('btn_build_commodity_report_im', 
                                               paste0('Build Report'),
                                               icon = icon('wrench')),
                                  
                                  ## reset side bar selectoin
                                  actionButton('btn_reset_ci_im',
                                               'Reset',
                                               icon = icon('refresh') )
                 )),
            
            ## Show panel only when Commodity intelligence sidebar is selected
            div( id = 'sidebar_ci_intel_by_hs',
                 conditionalPanel("input.sidebar === 'ci_intel_by_hs'",
                                  ## radio buttons to ask user to choose prebuilt commodity groups or build their owns
                                  radioButtons("rbtn_intel_by_hs",
                                               tags$p("Intelligence reported on:"), 
                                               choices = c("Exports", 'Imports'),
                                               selected = 'Exports',
                                               inline = F,
                                               width = "200px")
                 )),
            
            ## 4th tab HS finder -------------------------
            #menuItem("HS code finder", tabName = 'hs_finder', icon = icon('search') ),
            
            ## 5th tab Data source, definition , i.e., help ---------------
            menuItem( "FAQs", tabName = 'help', icon = icon('question-circle') ),
            
            ## 6th tab monthly update ----------------------
            menuItem( "Stats NZ Releases", tabName = 'monthly_update', icon = icon('bell'),
                      badgeLabel = "new", badgeColor = "green" )
        )
    )
        
        
    )

body <- dashboardBody(   ## 3.0. CSS styles in header ----------------------------
                         tags$head(
                             # ## JS codes
                             # tags$script(src = "fixedElement.js" ),
                             # tags$style(HTML(".scroller_anchor{height:0px; margin:0; padding:0;}; 
                             #                  .scroller{background: white; 
                             #                   border: 1px solid #CCC; 
                             #                   margin:0 0 10px; 
                             #                   z-index:100; 
                             #                   height:50px; 
                             #                   font-size:18px; 
                             #                   font-weight:bold; 
                             #                   text-align:center; 
                             #                  width:500px;}")),
                             
                             #tags$script(src = "world.js" ),
                             tags$script("document.title = 'New Zealand Trade Intelligence Dashboard'"),
                             
                             ### Styles 
                             tags$style(HTML(".small-box {height: 65px}")),
                             tags$style(HTML(".fa { font-size: 35px; }")),
                             tags$style(HTML(".glyphicon { font-size: 33px; }")),  ## use glyphicon package
                             tags$style(HTML(".fa-dashboard { font-size: 20px; }")),
                             tags$style(HTML(".fa-globe { font-size: 20px; }")),
                             tags$style(HTML(".fa-barcode { font-size: 20px; }")),
                             tags$style(HTML(".tab-content { padding-left: 20px; padding-right: 30px; }")) ,
                             tags$style(HTML(".fa-wrench { font-size: 15px; }")),
                             tags$style(HTML(".fa-refresh { font-size: 15px; }")),
                             tags$style(HTML(".fa-search { font-size: 15px; }")),
                             tags$style(HTML(".fa-comment { font-size: 20px; }")),
                             tags$style(HTML(".fa-share-alt { font-size: 20px; }")),
                             tags$style(HTML(".fa-envelope { font-size: 20px; }")),
                             tags$style(HTML(".fa-question-circle { font-size: 20px; }")),
                             tags$style(HTML(".fa-chevron-circle-down { font-size: 15px; }")),
                             tags$style(HTML(".fa-bell { font-size: 17px; }")),
                             tags$style(HTML(".fa-check { font-size: 14px; }")),
                             tags$style(HTML(".fa-times { font-size: 14px; }")),
                             
                             #tags$style(HTML(".fa-twitter { font-size: 10px; color:red;}")),
                             #tags$style(HTML(".fa-facebook { font-size: 10px; color:red;}")),
                             #tags$style(HTML(".fa-google-plus { font-size: 10px; color:red;}")),
                             #tags$style(HTML(".fa-pinterest-p { font-size: 10px; color:red;}")),
                             #tags$style(HTML(".fa-linkedin { font-size: 10px; color:red;}")),
                             #tags$style(HTML(".fa-tumblr { font-size: 10px; color:red;}")),
                             
                             ## modify the dashboard's skin color
                             tags$style(HTML('
                       /* logo */
                       .skin-blue .main-header .logo {
                       background-color: #006272;
                       }
                       /* logo when hovered */
                       .skin-blue .main-header .logo:hover {
                       background-color: #006272;
                       }
                       /* navbar (rest of the header) */
                       .skin-blue .main-header .navbar {
                       background-color: #006272;
                       }
                       /* active selected tab in the sidebarmenu */
                       .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                       background-color: #006272;
                                 }
                       ')
                             ),
                             
                             ## modify icon size in the sub side bar menu
                             tags$style(HTML('
                       /* change size of icons in sub-menu items */
                      .sidebar .sidebar-menu .treeview-menu>li>a>.fa {
                      font-size: 15px;
                      }
                      .sidebar .sidebar-menu .treeview-menu>li>a>.glyphicon {
                      font-size: 13px;
                      }
                      /* Hide icons in sub-menu items */
                      .sidebar .sidebar-menu .treeview>a>.fa-angle-left {
                      display: none;
                      } 
                      '
                             )) ,
                             
                             tags$style( HTML("hr {border-top: 1px solid #000000;}") ),
                             
                             ## to not show error message in shiny
                             tags$style( HTML(".shiny-output-error { visibility: hidden; }") ),
                             tags$style( HTML(".shiny-output-error:before { visibility: hidden; }") ),
                             
                             ## heand dropdown menu size
                             #tags$style(HTML('.navbar-custom-menu>.navbar-nav>li>.dropdown-menu { width:100px;}'))
                             tags$style(HTML('.navbar-custom-menu>.navbar-nav>li:last-child>.dropdown-menu { width:10px; font-size:10px; padding:1px; margin:1px;}')),
                             tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > h4 {width:0px; font-size:0px; padding:0px; margin:0px;}')),
                             tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > p {width:0px; font-size:0px; padding:0px; margin:0px;}'))
                         ),
        
    ## 3.1 Dashboard body --------------
    tabItems(
        
        ## 3.1 Main dashboard ----------------------------------------------------------
        tabItem( tabName = 'dashboard',
                 ## contents for the dashboard tab
                 div(id = 'main_wait_message',
                     h1('Note, initial load may take up to 10 seconds.',
                        style = "color:darkblue" , align = "center" ) ,
                     tags$hr()
                 ),
                 # 1.1 Export/import board ---------------------------
                 #div(class = 'scroller_anchor'),
                 #div(class = 'scroller', ) ,
                 
                 h1(paste0("New Zealand trade for the ", maxYear)) ,
                 fluidRow(
                     valueBoxOutput("ExTotBox") %>% withSpinner(type=4),
                     valueBoxOutput("ImTotBox"),
                     valueBoxOutput("BlTotBox")
                 ),
                 
                 h2(paste0("Goods")),
                 fluidRow(
                     valueBoxOutput("ExGBox") ,
                     valueBoxOutput("ImGBox") ,
                     valueBoxOutput("BlGBox")
                 ),
                 
                 h2(paste0("Services")),
                 fluidRow(
                     valueBoxOutput("ExSBox") ,
                     valueBoxOutput("ImSBox") ,
                     valueBoxOutput("BlSBox")
                 ) ,
                 
                 ## 1.2 Time series plot : la partie importante ----------------------------------------
                 h2(paste0("New Zealand trade over the past 20 years")),
                 fluidRow( column( width = 6,h4("Goods and services trade", align = 'center'), highchartOutput('IEGSLineHc') ),
                           column( width = 6,h4("Trade balance", align = 'center'), highchartOutput('GSTotalBalanceLineHc') )
                 ),
                 
                 
                
        
           ) 
 
        ),
    
)


ui <- 
    dashboardPage(header, siderbar, body)



## load
source('share_load.R')


# Define server logic 
server <-  function(input, output, session) {
    
    ## I. Main dashboard -----------------------------
    
    # 1. Value boxes (cases rouges et vertes) 
    tmp_ex_g <-
        dtf_shiny_full %>%
        filter( Country == 'World',
                Year == max(Year),
                Type_ie == 'Exports',
                Type_gs == 'Goods'
        ) %>%
        group_by( Year ) %>%
        summarise( Value = round(sum(Value/10^6),0) ) %>%
        dplyr::ungroup() %>%
        dplyr::select(Value) %>%
        as.numeric
    
    ###
    tmp_ex_s <-
        dtf_shiny_full %>%
        filter( Country == 'World',
                Year == max(Year),
                Type_ie == 'Exports',
                Type_gs == 'Services'
        ) %>%
        group_by( Year ) %>%
        summarise( Value = round(sum(Value/10^6),0) ) %>%
        dplyr::ungroup() %>%
        dplyr::select(Value) %>%
        as.numeric
    
    ###
    tmp_ex_tot <- tmp_ex_g + tmp_ex_s
    
    ###
    tmp_im_g <-
        dtf_shiny_full %>%
        filter( Country == 'World',
                Year == max(Year),
                Type_ie == 'Imports',
                Type_gs == 'Goods'
        ) %>%
        group_by( Year ) %>%
        summarise( Value = round(sum(Value/10^6),0) ) %>%
        dplyr::ungroup() %>%
        dplyr::select(Value) %>%
        as.numeric
    
    ###
    tmp_im_s <-
        dtf_shiny_full %>%
        filter( Country == 'World',
                Year == max(Year),
                Type_ie == 'Imports',
                Type_gs == 'Services'
        ) %>%
        group_by( Year ) %>%
        summarise( Value = round(sum(Value/10^6),0) ) %>%
        dplyr::ungroup() %>%
        dplyr::select(Value) %>%
        as.numeric
    
    ###
    tmp_im_tot <- tmp_im_g + tmp_im_s
    
    ###
    tmp_balance_g <- tmp_ex_g - tmp_im_g
    tmp_balance_s <- tmp_ex_s - tmp_im_s
    tmp_balance_tot <- tmp_balance_g + tmp_balance_s
    
    # 2. Total Trade a line chart : la partie importante -----------------------------------------------------------------
    
    tmp_dtf <-
        dtf_shiny_full %>%
        filter( Country == 'World',
                #Type_ie == 'Imports',
                Year >= (max(Year) - 20) ) %>%
        mutate( Value = round(Value/10^6) )
    
    output$IEGSLineHc <-renderHighchart({
        highchart() %>%
            hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
            hc_chart(type = 'line') %>%
            hc_series( list(name = 'Goods exports', data =tmp_dtf$Value[tmp_dtf$Type_gs=='Goods'&tmp_dtf$Type_ie=='Exports'], color='green', marker = list(symbol = 'circle') ),
                       list(name = 'Services exports', data =tmp_dtf$Value[tmp_dtf$Type_gs=='Services'&tmp_dtf$Type_ie=='Exports'], color = 'green', dashStyle = 'shortDot', marker = list(symbol = 'triangle') ),
                       list(name = 'Goods imports', data =tmp_dtf$Value[tmp_dtf$Type_gs=='Goods'&tmp_dtf$Type_ie=='Imports'], color = 'red', marker = list(symbol = 'circle') ),
                       list(name = 'Services imports', data =tmp_dtf$Value[tmp_dtf$Type_gs=='Services'&tmp_dtf$Type_ie=='Imports'], color = 'red', dashStyle = 'shortDot', marker = list(symbol = 'triangle')  )
            )%>%
            hc_xAxis( categories = unique(tmp_dtf$Year) ) %>%
            hc_yAxis( title = list(text = "$ million, NZD"),
                      labels = list( format = "${value:,.0f} m")  ) %>%
            hc_plotOptions(column = list(
                dataLabels = list(enabled = F),
                #stacking = "normal",
                enableMouseTracking = T ) 
            )%>%
            hc_tooltip(table = TRUE,
                       sort = TRUE,
                       pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                             " {series.name}: ${point.y} m"),
                       headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
            ) %>%
            hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = 000 )
    })
    
    # 2.1 Total Trade balance a line chart : important aussi -----------------------------------------------------------------
    
    tmp_dtf_balance <-
        dtf_shiny_full %>%
        filter( Country == 'World',
                #Type_ie == 'Imports',
                Year >= (max(Year) - 20) ) %>%
        group_by( Year, Country, Type_gs ) %>%
        mutate( Value = Value[ Type_ie == 'Exports'] - Value[ Type_ie == 'Imports']  ) %>%
        ungroup %>%
        filter( Type_ie == 'Exports' ) %>%
        mutate(  Type_gs = paste0(Type_gs, ' balance') )
    
    tmp_dtf_balance_tot <-
        tmp_dtf_balance %>%
        group_by( Year, Country, Type_ie ) %>%
        summarise( Value = sum(Value, na.rm=T) ) %>%
        ungroup %>%
        mutate( Type_gs = 'Trade balance' )
    
    tmp_dtf_balance %<>%
        bind_rows( tmp_dtf_balance_tot  ) %>%
        mutate( Value = round(Value/10^6) )
    
    output$GSTotalBalanceLineHc <-renderHighchart({
        highchart() %>%
            hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
            hc_chart(type = 'line') %>%
            hc_series( list(name = 'Trade balance', data =tmp_dtf_balance$Value[tmp_dtf_balance$Type_gs=='Trade balance'], color='brown' , marker = list(enabled = F), lineWidth = 3 ),
                       list(name = 'Goods balance', data =tmp_dtf_balance$Value[tmp_dtf_balance$Type_gs=='Goods balance'], color = 'darkgreen', dashStyle = 'shortDot', marker = list(symbol = 'circle') ),
                       list(name = 'Services balance', data =tmp_dtf_balance$Value[tmp_dtf_balance$Type_gs=='Services balance'], color = 'darkblue', dashStyle = 'shortDot',  marker = list(symbol = 'triangle') )
            )%>%
            hc_xAxis( categories = unique(tmp_dtf_balance$Year) ) %>%
            hc_yAxis( title = list(text = "$ million, NZD"),
                      labels = list( format = "${value:,.0f} m"),
                      plotLines = list(
                          list(#label = list(text = "This is a plotLine"),
                              color = "#ff0000",
                              #dashStyle = 'shortDot',
                              width = 2,
                              value = 0 ) )
            ) %>%
            hc_plotOptions(column = list(
                dataLabels = list(enabled = F),
                #stacking = "normal",
                enableMouseTracking = T ) 
            )%>%
            hc_tooltip(table = TRUE,
                       sort = TRUE,
                       pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                             " {series.name}: ${point.y} m"),
                       headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
            ) %>%
            hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = 000 )
    })
    
    
}
    

# Run the application 
shinyApp(ui = ui, server = server)

#####################################################

#A mettre dans un fichier à part (sourcé à la fois par ui et server)

### Share load should be sourced by both ui and server.

##  load library --------------------
library(rjson)
library(shinydashboard)
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(highcharter)
library(lubridate)
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
options(spinner.color="#006272")
library(timevis)
#library(RCurl)
#library(jsonlite)
library(comtradr)
library(memoise)
library(networkD3)
library(promises)
library(future)
plan(multiprocess)

### use memoise package for ct_search in comtradr ----
m_ct_search <- memoise::memoise(ct_search)

## load functions
source('helper_funs.R')

## load concordance
load('concord_hs24.rda')
load('concord_snz_eg.rda')
load('concord_snz_ig.rda')
load('concord_country.rda')
load('concord_country_group.rda')
load('concord_country_member.rda')
load('concord_country_iso_latlon_raw.rda')
load('flag_table.rda')
load('concord_eu28.rda')


## load data
load("dtf_shiny_full.rda")
#load("dtf_shiny.rda")
#source("groom_data_full_HS_levels_country.R")
load("dtf_shiny_commodity_service_ex.rda") ## principle commodity from StatsNZ -- exports
load("dtf_shiny_commodity_service_im.rda") ## principle commodity from StatsNZ -- imports
load("dtf_shiny_country_gs.rda") ## commodity by country data
load("dtf_country_group.rda") ## Country grouped by region
load("list_country.rda") ## Country grouped by region
load("list_snz_commodity_ex.rda") ## pre-defined commodity list form SNZ
load("list_snz_commodity_im.rda") ## pre-defined commodity list form SNZ
#load("list_snz_commodity.rda") ## pre-defined commodity list form SNZ
load("dtf_fdi_odi.rda") ## FDI and ODI data
load("dtf_in_out.rda") ## ppl movement visitor in and out
load("concord_uncomtrade_country.rda")

## setup global variables
maxYear <- tolower(paste0(dtf_shiny_full$Note[1],' ', max(dtf_shiny_full$Year)))
maxYear <- gsub('q1', 'March', maxYear)
maxYear <- gsub('q2', 'June', maxYear)
maxYear <- gsub('q3', 'September', maxYear)
maxYear <- gsub('q4', 'December', maxYear)

## Stats NZ's monthly update link -- update very month
SNZ_link <- "https://www.stats.govt.nz/information-releases/?filters=Balance%20of%20payments%2CImports%20and%20exports%2CTourism"
