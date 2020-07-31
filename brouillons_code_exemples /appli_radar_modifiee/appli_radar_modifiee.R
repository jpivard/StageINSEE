#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Fichier 1 : LIST OF REQUIRED PACKAGES -----------------------------------------------

required_packages <- c(
    "checkpoint"
)

# install missing packages

new.packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(new.packages)) {
    install.packages(new.packages)
}

rm(new.packages)

library(checkpoint)
checkpoint(snapshotDate ='2019-12-17')
library(AMR)
library(data.table)
library(DT)
library(ggridges)
library(lubridate)
library(plotly)
library(qicharts2)
library(rintrojs)
library(shiny)
library(shinyBS)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(survival)
library(ggpubr)
library(survminer)
library(tidyverse)
library(viridis)
library(zoo)


#Fichier global

# INSTALL DEPENDENCIES ----------------------------------------------------

source('dependencies.R')
# load all packages
lapply(required_packages, require, character.only = TRUE)

# DATA TRANSFORMATION AND NEW VARIABLES -----------------------------------

admissions <- read_csv("data/admissions.csv")
antimicrobials <- read_csv("data/antimicrobials.csv")
microbiology <- read_csv("data/microbiology.csv")


# antimicrobial count for select input in ui.R

ab <- antimicrobials %>%
    filter(!is.na(ab_type)) %>%
    group_by(ab_type) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    filter(!is.na(ab_type)) %>%
    distinct()

ab_groups <- antimicrobials %>%
    filter(!is.na(ab_group)) %>%
    select(ab_group) %>%
    arrange(ab_group) %>%
    distinct()


update_ab <- antimicrobials %>%
    select(ab_type, ab_group) %>%
    distinct(.keep_all = TRUE)


# HELP & INTRO DATA ---------------------------------------------------------------

steps <- read_csv2("help.csv")
intro <- read_csv2("intro.csv")


# FLUID DESIGN FUNCTION ---------------------------------------------------

fluid_design <- function(id, w, x, y, z) {
    fluidRow(
        div(
            id = id,
            column(
                width = 6,
                uiOutput(w),
                uiOutput(y)
            ),
            column(
                width = 6,
                uiOutput(x),
                uiOutput(z)
            )
        )
    )
}

#Fichier UI

ui <- dashboardPage(
    skin = "black",
    title = "RadaR",
    
    # HEADER ------------------------------------------------------------------
    
    dashboardHeader(
        title = span(img(src = "radar.svg", height = 35), "RadaR"),
        titleWidth = 300,
        dropdownMenu(
            type = "notifications", 
            headerText = strong("HELP"), 
            icon = icon("question"), 
            badgeStatus = NULL,
            notificationItem(
                text = (steps$text[1]),
                icon = icon("spinner")
            ),
            notificationItem(
                text = steps$text[2],
                icon = icon("address-card")
            ),
            notificationItem(
                text = steps$text[3],
                icon = icon("calendar")
            ),
            notificationItem(
                text = steps$text[4],
                icon = icon("user-md")
            ),
            notificationItem(
                text = steps$text[5],
                icon = icon("ambulance")
            ),
            notificationItem(
                text = steps$text[6],
                icon = icon("flask")
            ),
            notificationItem(
                text = strong(steps$text[7]),
                icon = icon("exclamation")
            )
        ),
        tags$li(
            a(
                strong("ABOUT RadaR"),
                height = 40,
                href = "https://github.com/ceefluz/radar/blob/master/README.md",
                title = "",
                target = "_blank"
            ),
            class = "dropdown"
        )
    ),
    
    # SIDEBAR -----------------------------------------------------------------
    
    dashboardSidebar(
        width = 300,
        introBox(data.step = 3, data.intro = intro$text[3], #  intro tour
                 div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
                 sidebarMenu(
                     introBox(data.step = 1, data.intro = intro$text[1], # intro tour
                              div(id = "sidebar_button",
                                  bsButton(inputId = "confirm", 
                                           label = "START RADAR", 
                                           icon = icon("play-circle"), 
                                           style = "danger")
                              )
                     ),
                     div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
                     menuItem(
                         "ANTIMICROBIALS",
                         tabName = "antimicrobials",
                         icon = icon("spinner"),
                         menuItem(
                             "START OF ANTIMICROBIALS \n (IN RELATION TO START OF ADMISSION)",
                             sliderInput(
                                 inputId = "ab_timingInput",
                                 label = "",
                                 min = 0,
                                 max = max(antimicrobials$ab_timing, na.rm = TRUE),
                                 value = c(0, max(antimicrobials$ab_timing, na.rm = TRUE)),
                                 step = 1
                             ),
                             switchInput(
                                 inputId = "ab_anyInput",
                                 label = "SELECTION",
                                 value = FALSE, 
                                 onLabel = "SLIDER", 
                                 offLabel = "ANY", 
                                 inline = TRUE, 
                                 size = "mini", 
                                 width = "100%", 
                                 offStatus = "danger"
                             )
                         ),
                         menuItem(
                             "MINIMUM TREATMENT DURATION (DAYS) \n ALL ANTIMICROBIALS",
                             sliderInput(
                                 inputId = "ab_allInput",
                                 label = "",
                                 value = 2L,
                                 min = 1L,
                                 max = 10L,
                                 step = 1L
                             ),
                             switchInput(
                                 inputId = "ab_any_allInput",
                                 label = "SELECTION",
                                 value = FALSE, 
                                 onLabel = "SLIDER", 
                                 offLabel = "ANY", 
                                 inline = TRUE, 
                                 size = "mini", 
                                 width = "100%", 
                                 offStatus = "danger"
                             )
                         ),
                         menuItem(
                             "MINIMUM DURATION OF USE (DAYS) \n SINGLE ANTIMICROBIAL",
                             sliderInput(
                                 inputId = "ab_singleInput",
                                 label = "",
                                 value = 2L,
                                 min = 1L,
                                 max = 10L,
                                 step = 1L
                             ),
                             switchInput(
                                 inputId = "ab_any_singleInput",
                                 label = "SELECTION",
                                 value = FALSE, 
                                 onLabel = "SLIDER", 
                                 offLabel = "ANY", 
                                 inline = TRUE, 
                                 size = "mini", 
                                 width = "100%", 
                                 offStatus = "danger"
                             )
                         ),
                         menuItem(
                             "ADMINISTRATION ROUTE",
                             checkboxGroupInput(
                                 inputId = "adminInput",
                                 label = "",
                                 choices = unique(antimicrobials$ab_route[!is.na(antimicrobials$ab_route)]),
                                 selected = unique(antimicrobials$ab_route[!is.na(antimicrobials$ab_route)])
                             )
                         ),
                         menuItem(
                             "FIRST ANTIMICROBIALS ONLY",
                             switchInput(
                                 inputId = "firstInput",
                                 label = "",
                                 value = FALSE, 
                                 onLabel = "FIRST", 
                                 offLabel = "ALL", 
                                 size = "mini", 
                                 width = "100%", 
                                 offStatus = "danger"
                             )
                         ),
                         checkboxGroupButtons(
                             inputId = "allInput",
                             label = "CHOOSE GROUPS OF ANTIMICROBIALS",
                             choices = "ALL / NONE",
                             size = "sm",
                             selected = "ALL / NONE"
                         ),
                         checkboxGroupInput(
                             inputId = "abGroupInput",
                             label = "",
                             choices = sort(ab_groups$ab_group)
                         ),
                         checkboxGroupInput(
                             inputId = "abInput",
                             label = "ANTIMICROBIALS",
                             choices = ab$ab_type,
                             selected = ab$ab_type
                         )
                     )
                     ,
                     br(),
                     br(),
                     menuItem(
                         "PATIENTS",
                         tabName = "patients",
                         icon = icon("address-card"),
                         checkboxGroupInput(
                             inputId = "genderInput",
                             label = "",
                             choices = unique(admissions$gender),
                             selected = unique(admissions$gender),
                             inline = TRUE
                         ),
                         sliderInput(
                             inputId = "ageInput",
                             label = "Age",
                             value = c(min(admissions$age, na.rm = TRUE), max(admissions$age, na.rm = TRUE)),
                             min = min(admissions$age, na.rm = TRUE),
                             max = max(admissions$age, na.rm = TRUE),
                             step = 1,
                             sep = ""
                         )
                     ),
                     br(),
                     br(),
                     menuItem(
                         "YEAR",
                         tabName = "year",
                         icon = icon("calendar"),
                         sliderInput(
                             inputId = "yearInput",
                             label = "Year",
                             value = c(min(antimicrobials$year, na.rm = TRUE), max(antimicrobials$year, na.rm = TRUE)),
                             min = min(antimicrobials$year, na.rm = TRUE),
                             max = max(antimicrobials$year, na.rm = TRUE),
                             step = 1L,
                             sep = ""
                         )
                     ),
                     br(),
                     br(),
                     menuItem(
                         "SPECIALTY",
                         tabName = "specialty",
                         icon = icon("user-md"),
                         checkboxGroupInput(
                             inputId = "specInput",
                             label = "SPECIALTY",
                             choices = unique(admissions$specialty),
                             selected = unique(admissions$specialty)
                         ),
                         sliderTextInput(
                             inputId = "nInput",
                             label = "MINIMUM NUMBER OF PATIENTS PER SUBSPECIALTY",
                             choices = c("0", "10", "100", "1000", "10000"),
                             selected = "0",
                             grid = TRUE
                         ),
                         selectizeInput(
                             inputId = "inInput",
                             label = "INCLUDE ONLY THIS SUBSPECIALTY:",
                             choices = c(levels(as.factor(admissions$sub_specialty))),
                             multiple = TRUE
                         ),
                         selectizeInput(
                             inputId = "exInput",
                             label = "EXCLUDE SUBSPECIALTY",
                             choices = c(levels(as.factor(admissions$sub_specialty))),
                             multiple = TRUE
                         )
                     ),
                     br(),
                     br(),
                     menuItem(
                         "ORIGIN",
                         tabName = "admission",
                         icon = icon("ambulance"),
                         checkboxGroupInput(
                             inputId = "admissionInput",
                             label = "",
                             choices = levels(as.factor(admissions$adm_route)),
                             selected = levels(as.factor(admissions$adm_route))
                         )
                     ),
                     br(),
                     br(),
                     menuItem(
                         "DIAGNOSTICS",
                         tabName = "diagnostics",
                         icon = icon("flask"),
                         selectInput(
                             inputId = "diagnosticsInput",
                             label = "",
                             choices = list("Blood cultures" = "bc_timing", "Urine cultures" = "uc_timing")
                         ),
                         sliderInput(
                             inputId = "checkInput",
                             label = "DAYS TO FIRST TEST (IN RELATION TO START OF ANTIMICROBIALS)",
                             value = c(-1L, 1L),
                             min = min(c(admissions$bc_timing, admissions$uc_timing), na.rm = TRUE),
                             max = max(c(admissions$bc_timing, admissions$uc_timing), na.rm = TRUE),
                             step = 1L
                         )
                     ),
                     br(),
                     br(),
                     menuItem(
                         "DOWNLOAD SELECTION",
                         tabName = "download",
                         icon = icon("download"),
                         textInput(
                             inputId = "filename",
                             placeholder = "Name download file",
                             label = ""
                         ),
                         div(
                             downloadButton(
                                 outputId = "downloadData",
                                 label = "Save Antimicrobial/Admission Data",
                                 icon = icon("download"),
                                 style = "color: black; margin-left: 15px; margin-bottom: 5px;"
                             )
                         ),
                         div(
                             downloadButton(
                                 outputId = "downloadMicroData",
                                 label = "Save Microbiology Data",
                                 icon = icon("download"),
                                 style = "color: black; margin-left: 15px; margin-bottom: 5px;"
                             )
                         )
                     ),
                     br()
                     
                 )
        )),
    
    
    # BODY --------------------------------------------------------------------
    
    dashboardBody(
        tags$head(
            tags$link(
                rel = "stylesheet", 
                type = "text/css", 
                href = "radar_style.css")
        ),
        
        useShinyjs(),
        introjsUI(),
        
        # MAIN BODY ---------------------------------------------------------------
        
        fluidRow(
            column(
                width = 12,
                introBox(
                    bsButton("patients", 
                             label = "PATIENTS", 
                             icon = icon("user"), 
                             style = "success"),
                    bsButton("antimicrobials", 
                             label = "ANTIMICROBIALS", 
                             icon = icon("spinner", class = "spinner-box"), 
                             style = "success"),
                    bsButton("diagnostics", 
                             label = "DIAGNOSTICS", 
                             icon = icon("flask", class = "flask-box"), 
                             style = "success"),
                    bsButton("outcome", 
                             label = "OUTCOME", 
                             icon = icon("thumbs-o-up"), 
                             style = "success"),
                    data.step = 2, data.intro = intro$text[2])
            )
        ),
        
        fluid_design("antimicrobials_panel", "box1", "box2", "box3", "box4"),
        fluid_design("diagnostics_panel", "box5", "box6", "box7", "box8"),
        fluid_design("outcome_panel", "box_los1", "box_los2", "box_los3", NULL),
        
        fluidRow(
            div(
                id = "patients_panel", 
                column(
                    width = 12,
                    introBox(data.step = 4, data.intro = intro$text[4],
                             uiOutput("box_pat")
                    )
                ),
                column(
                    width = 6,
                    uiOutput("box_pat2")
                ),
                column(
                    width = 6,
                    uiOutput("box_year")
                )
            )
        )
    )
)

server <- function(input, output, session) {
    
    
    # DEFINE SETS -------------------------------------------------
    
    # UI - GENERAL --------------------------------------------------------------
    
    #show intro modal
    observeEvent("", {
        showModal(modalDialog(
            includeHTML("intro_text.html"),
            easyClose = TRUE,
            footer = tagList(
                actionButton(inputId = "intro", label = "INTRODUCTION TOUR", icon = icon("info-circle"))
            )
        ))
    })
    
    observeEvent(input$intro,{
        removeModal()
    })
    
    # show intro tour
    observeEvent(input$intro,
                 introjs(session, options = list("nextLabel" = "Continue",
                                                 "prevLabel" = "Previous",
                                                 "doneLabel" = "Alright. Let's go"))
    )
    
    # use action buttons as tab selectors
    update_all <- function(x) {
        updateSelectInput(session, "tab",
                          choices = c("", "Patients", "Antimicrobial consumption", "Diagnostics", "Outcome"),
                          label = "",
                          selected = x
        )
    }
    
    observeEvent(input$patients, {
        update_all("Patients")
    })
    observeEvent(input$antimicrobials, {
        update_all("Antimicrobial consumption")
    })
    observeEvent(input$diagnostics, {
        update_all("Diagnostics")
    })
    observeEvent(input$outcome, {
        update_all("Outcome")
    })
    
    # update confirm button
    
    observeEvent(input$confirm, {
        updateButton(
            session, 
            inputId = "confirm", 
            label = "CONFIRM SELECTION", 
            icon = icon("bar-chart-o"), 
            style = "primary")
    })
    
    # DYNAMIC RENDER RULES ----------------------------------------------------
    
    observeEvent("", {
        show("patients_panel")
        hide("antimicrobials_panel")
        hide("diagnostics_panel")
        hide("outcome_panel")
    }, once = TRUE)
    
    observeEvent(input$patients, {
        show("patients_panel")
        hide("diagnostics_panel")
        hide("antimicrobials_panel")
        hide("outcome_panel")
    })
    observeEvent(input$antimicrobials, {
        show("antimicrobials_panel")
        hide("diagnostics_panel")
        hide("outcome_panel")
        hide("patients_panel")
    })
    observeEvent(input$diagnostics, {
        show("diagnostics_panel")
        hide("antimicrobials_panel")
        hide("outcome_panel")
        hide("patients_panel")
    })
    observeEvent(input$outcome, {
        show("outcome_panel")
        hide("diagnostics_panel")
        hide("antimicrobials_panel")
        hide("patients_panel")
    })
    
    # UI - PATIENTS - 1 ----------------------------------------------------------
    
    output$box_pat <- renderUI({
        div(
            style = "position: relative; backgroundColor: #ecf0f5",
            tabBox(
                id = "box_pat",
                width = NULL,
                height = 320,
                tabPanel(
                    title = "Subspecialties in selection",
                    div(
                        style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                        introBox(data.step = 5, data.intro = intro$text[5],
                                 dropdown(
                                     radioGroupButtons(
                                         inputId = "box_pat1",
                                         label = NULL, 
                                         choices = c("Show all", "Show top 10 only"), 
                                         selected = "Show all", 
                                         direction = "vertical"
                                     ),
                                     size = "xs",
                                     icon = icon("gear", class = "opt"), 
                                     up = TRUE
                                 )
                        )
                    ),
                    withSpinner(
                        plotlyOutput("plot_pat_select", height = 230),
                        type = 4,
                        color = "#d33724", 
                        size = 0.7 
                    )
                )
            )
        )
    })
    
    # UI - PATIENTS - 3 -------------------------------------------------------
    
    output$box_year <- renderUI({
        div(
            style = "position: relative",
            tabBox(
                id = "box_year",
                width = NULL,
                height = 400,
                tabPanel(
                    title = "Number of patients per year",
                    div(
                        style = "position: absolute; left:0.5em; bottom: 0.5em;",
                        dropdown(
                            radioGroupButtons(
                                inputId = "box_year1",
                                label = "Select time period", 
                                choiceNames = c("Years", "Quarter", "Months"),
                                choiceValues = c("years", "yearquarter_adm", "yearmonth_adm"), 
                                selected = "years", 
                                direction = "vertical"
                            ),
                            size = "xs",
                            icon = icon("gear", class = "opt"), 
                            up = TRUE
                        )
                    ),
                    div(
                        style = "position: absolute; left: 4em; bottom: 0.5em;",
                        dropdown( 
                            downloadButton(outputId = "down_year_select", label = "Download plot"),
                            size = "xs",
                            icon = icon("download", class = "opt"), 
                            up = TRUE
                        )
                    ),
                    withSpinner(
                        plotOutput("plot_year_select", height = 300),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                )
            )
        )
    })
    
    # UI - AB - 1 ------------------------------------------------------------------
    output$box1 <- renderUI({
        div(
            style = "position: relative",
            tabBox(
                id = "box1",
                width = NULL,
                height = 400,
                tabPanel(
                    title = "Antimicrobials",
                    div(
                        style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                        dropdown(
                            radioGroupButtons(
                                inputId = "box1.0",
                                label = "Choose groups", 
                                choiceNames = c("Antimicrobial - Groups", "Antimicrobials"), 
                                choiceValues = c("ab_group", "ab_type"), 
                                direction = "vertical"
                            ),
                            radioGroupButtons(
                                inputId = "box1.1",
                                label = "Show", 
                                choiceNames = c("Prescriptions", "DDD per 100 bed days", "DOT per 100 bed days"),
                                choiceValues = c("prescriptions", "DDD_100", "DOT_100"),
                                selected = "prescriptions", 
                                direction = "vertical"
                            ),
                            size = "xs",
                            icon = icon("gear", class = "opt"), 
                            up = TRUE
                        )
                    ),
                    withSpinner(
                        plotOutput("plot_ab", height = 300),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                ),
                div(
                    style = "position: absolute; right: 0.5em; bottom: 0.5em;",
                    conditionalPanel(
                        "input.box1 == 'Antimicrobials'",
                        actionBttn(
                            inputId = "ab",
                            icon = icon("search-plus", class = "opt"),
                            style = "fill",
                            color = "danger",
                            size = "xs"
                        )
                    )
                ),
                div(
                    style = "position: absolute; left: 4em; bottom: 0.5em;",
                    dropdown(
                        downloadButton(outputId = "down_box_1", label = "Download plot"),
                        size = "xs",
                        icon = icon("download", class = "opt"), 
                        up = TRUE
                    )
                )
            )
        )
    })
    
    observeEvent((input$ab), {
        showModal(modalDialog(
            renderPlot({
                plot_ab() + theme(
                    axis.title = element_text(size = 20),
                    text = element_text(size = 20),
                    plot.title = element_text(size = 26)
                )
            }, height = 600),
            easyClose = TRUE,
            size = "l",
            footer = NULL
        ))
    })
    
    # UI - DIAGNOSTICS - 1 ------------------------------------------------------------------
    
    output$box5 <- renderUI({
        div(
            style = "position: relative",
            tabBox(
                id = "box5",
                width = NULL,
                height = 400,
                tabPanel(
                    title = "Diagnostics in selected patients",
                    div(
                        style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                        dropdown(
                            radioGroupButtons(
                                inputId = "box5.1",
                                label = "Change time", 
                                choiceNames = c("Year", "Quarter", "Month"), 
                                choiceValues = c("year", "yearquarter_adm", "yearmonth_adm"), 
                                selected = "year", 
                                direction = "vertical"
                            ),
                            radioGroupButtons(
                                inputId = "box5.2",
                                label = "Change plot", 
                                choiceNames = c("Count", "Proportion"), 
                                choiceValues = c("dodge", "fill"), 
                                selected = "dodge", 
                                direction = "vertical"
                            ),
                            size = "xs",
                            icon = icon("gear", class = "opt"), 
                            up = TRUE
                        )
                    ),
                    div(
                        style = "position: absolute; left: 4em;bottom: 0.5em;",
                        dropdown(
                            downloadButton(outputId = "down_box_5", label = "Download plot"),
                            size = "xs",
                            icon = icon("download", class = "opt"), 
                            up = TRUE
                        )
                    ),
                    withSpinner(
                        plotOutput("plot_dia_adm", height = 300),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                ),
                tabPanel(
                    title = "Timing of selected diagnostics",
                    div(
                        style = "position:absolute;left:0.5em;bottom: 0.5em;",
                        dropdown(
                            downloadButton(outputId = "down_box_6", label = "Download plot"),
                            size = "xs",
                            icon = icon("download", class = "opt"), 
                            up = TRUE
                        )
                    ),
                    withSpinner(
                        plotOutput("plot_dia_timing", height = 300),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                ),
                div(
                    style = "position: absolute; right: 0.5em; bottom: 0.5em;",
                    conditionalPanel(
                        "input.box5 == 'Diagnostics in selected patients'",
                        actionBttn(
                            inputId = "dia_adm",
                            icon = icon("search-plus", class = "opt"),
                            style = "fill",
                            color = "danger",
                            size = "xs"
                        )
                    )
                ),
                div(
                    style = "position: absolute; right: 0.5em; bottom: 0.5em;",
                    conditionalPanel(
                        "input.box5 == 'Timing of selected diagnostics'",
                        actionBttn(
                            inputId = "dia_timing",
                            icon = icon("search-plus", class = "opt"),
                            style = "fill",
                            color = "danger",
                            size = "xs"
                        )
                    )
                )
            )
        )
    })

    # UI - OUTCOME - 1 -----------------------------------------------------
    
    # BOX PATIENTS - 1 ------------------------------------------------------------
    
    plot_pat_select <- reactive({
        
        pat_select <- set_reac_1() %>%
            group_by(sub_specialty) %>%
            summarise(n = n()) %>% 
            arrange(desc(n))
        
        if (input$box_pat1 == "Show top 10 only") {
            pat_select <- pat_select[1:10,]
        } else{
            pat_select
        }
        
        plot <- 
            ggplot(pat_select, 
                   aes(n, 
                       y = 0, 
                       group = sub_specialty, 
                       text = sub_specialty, 
                       count = n)) +
            geom_point(aes(size = n, fill = sub_specialty), 
                       alpha = 0.6, 
                       color = "black", 
                       shape = 21) +
            coord_cartesian(ylim = c(-2, 2)) +
            scale_size_area(max_size = 25) +
            guides(fill = FALSE, size = FALSE) +
            labs(x = "Number of patients (use mouse to identify subspecialties)") +
            scale_x_continuous(
                name = "Number of patients (use mouse to identify subspecialties)", 
                trans = "log10", 
                breaks = c(1, 10, 100, 1000, 10000)) +
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
    
    output$plot_pat_select <- renderPlotly({
        plot_pat_select()
    })
    
    # BOX PATIENTS - 3 ----------------------------------------------------------
    
    plot_year_select <- reactive({
        
        years <- set_reac_1() %>% 
            count(year)
        
        months <- set_reac_1() %>% 
            count(yearmonth_adm)
        
        quarter <- set_reac_1() %>% 
            count(yearquarter_adm)
        
        if (input$box_year1 == "years") {
            plot <- qic(year, n,
                        data = years, 
                        agg.fun = "sum",
                        decimals = 2,
                        xlab = "Year",
                        ylab = "Count",
                        title = "Admissions per year") +
                scale_x_continuous(breaks = 
                                       c(min(set_reac_2()$year, na.rm = TRUE):max(set_reac_2()$year, na.rm = TRUE)))
            
        } else {
            if (input$box_year1 == "yearquarter_adm") {
                plot <- qic(yearquarter_adm, n,
                            data = quarter,
                            agg.fun = "sum",
                            decimals = 2,
                            xlab = "Quarter",
                            ylab = "Count",
                            title = "Admissions per quarter") +
                    scale_x_yearqtr(n = length(quarter$yearquarter_adm)/4) 
                
            } else {
                plot <- qic(yearmonth_adm, n,
                            data = months,
                            agg.fun = "sum",
                            decimals = 2,
                            xlab = "Month",
                            ylab = "Count",
                            title = "Admissions per month") +
                    scale_x_yearmon(n = length(months$yearmonth_adm)/12)
            }
        }
        
        plot +
            labs(caption = "(Line represents median; if red-dotted = signal for non-random variation)") +
            theme(plot.caption = element_text(size = 10, colour = "darkgrey"), 
                  plot.title = element_text(face = "bold", size = 12))
        
    })
    
    output$plot_year_select <- renderPlot({
        plot_year_select()
    })
    
    # BOX AB - 2 --------------------------------------------------------------
    
    # DDD & DOT calculation
    
    ddd_dot <- reactive({
        
        set_reac_2() %>% 
            group_by(id, adm_id, year, 
                     yearmonth_adm, yearquarter_adm, specialty, sub_specialty, adm_route) %>% 
            summarise(
                DDD_sum = sum(ddd_per_prescription, na.rm = TRUE),
                DOT_sum = sum(ab_days, na.rm = TRUE)
            ) %>% 
            ungroup() %>% 
            left_join(
                set_reac_2() %>% select(id, adm_id, LOS)
            ) %>% 
            distinct() %>% 
            mutate(DDD_per_day_100 = DDD_sum/LOS/100,
                   DOT_per_day_100 = DOT_sum/LOS/100)
        
    })
    
    # Defined daily doses
    
    ddd_ts <- reactive({
        
        plot_year <- qic(x = year, y = DDD_per_day_100, 
                         data = ddd_dot(),
                         agg.fun = "sum",
                         decimals = 2,
                         xlab = "Year",
                         ylab = "DDD per 100 bed days",
                         title = "Defined daily doses (DDD) / 100 bed days per year",
                         facets = 
                             if (input$box2.3 == "specialty") {
                                 ~ specialty
                             } else { 
                                 if (input$box2.3 == "sub_specialty") {
                                     ~ sub_specialty
                                 } else {
                                     if (input$box2.3 == "adm_route") {
                                         ~ adm_route
                                     } else {
                                         NULL
                                     }
                                 }
                             }
        ) + scale_x_continuous(breaks = 
                                   c(min(set_reac_2()$year, na.rm = TRUE):max(set_reac_2()$year, na.rm = TRUE)))
        
        plot_month <- qic(x = yearmonth_adm, y = DDD_per_day_100,
                          data = ddd_dot(), 
                          agg.fun = "sum",
                          decimals = 2,
                          xlab = "Months",
                          ylab = "DDD per 100 bed days",
                          title = "Defined daily doses (DDD) / 100 bed days per month",
                          facets =
                              if (input$box2.3 == "specialty") {
                                  ~ specialty
                              } else {
                                  if (input$box2.3 == "sub_specialty") {
                                      ~ sub_specialty
                                  } else {
                                      if (input$box2.3 == "adm_route") {
                                          ~ adm_route
                                      } else {
                                          NULL
                                      }
                                  }
                              }
        ) + scale_x_yearmon(n = length(unique(set_reac_2()$yearmonth_adm))/4)
        
        plot_quarter <- qic(x = yearquarter_adm, y = DDD_per_day_100,
                            data = ddd_dot(),
                            agg.fun = "sum",
                            decimals = 2,
                            xlab = "Quarter",
                            ylab = "DDD per 100 bed days",
                            title = "Defined daily doses (DDD) / 100 bed days per quarter",
                            facets =
                                if (input$box2.3 == "specialty") {
                                    ~ specialty
                                } else {
                                    if (input$box2.3 == "sub_specialty") {
                                        ~ sub_specialty
                                    } else {
                                        if (input$box2.3 == "adm_route") {
                                            ~ adm_route
                                        } else {
                                            NULL
                                        }
                                    }
                                }
        ) + scale_x_yearqtr(n = length(unique(set_reac_2()$yearquarter_adm))/4)
        
        if (input$box2.2 == "yearmonth_adm") {
            plot <- plot_month
        } else { 
            if (input$box2.2 == "yearquarter_adm") {
                plot <- plot_quarter
            } else {
                plot <- plot_year
            }
        }
        
        plot + 
            labs(caption = "(Line represents median; if red-dotted = signal for non-random variation)") +
            theme(plot.caption = element_text(size = 10, colour = "darkgrey"), 
                  plot.title = element_text(face = "bold", size = 12),
                  axis.text.x = element_text(angle = 90))
        
    })
    
    output$ddd_ts <- renderPlot({
        ddd_ts()
    })
    
    
    # BOX AB - 3 --------------------------------------------------------------
    
    # Days of therapy
    
    dot_ts <- reactive({
        
        plot_year <- qic(x = year, y = DOT_per_day_100,
                         data = ddd_dot(),
                         agg.fun = "sum",
                         decimals = 2,
                         xlab = "Year",
                         ylab = "DOT per 100 bed days",
                         title = "Days of therapy (DOT) / 100 bed days per year",
                         facets = 
                             if (input$box3.3 == "specialty") {
                                 ~ specialty
                             } else { 
                                 if (input$box3.3 == "sub_specialty") {
                                     ~ sub_specialty
                                 } else {
                                     if (input$box3.3 == "adm_route") {
                                         ~ adm_route
                                     } else {
                                         NULL
                                     }
                                 }
                             }
        ) + scale_x_continuous(breaks = 
                                   c(min(set_reac_2()$year, na.rm = TRUE):max(set_reac_2()$year, na.rm = TRUE)))
        
        plot_month <- qic(x = yearmonth_adm, y = DOT_per_day_100,
                          data = ddd_dot(),
                          agg.fun = "sum",
                          decimals = 2,
                          xlab = "Months",
                          ylab = "DOT per 100 bed days",
                          title = "Days of therapie (DOT) / 100 bed days per month",
                          facets = 
                              if (input$box3.3 == "specialty") {
                                  ~ specialty
                              } else { 
                                  if (input$box3.3 == "sub_specialty") {
                                      ~ sub_specialty
                                  } else {
                                      if (input$box3.3 == "adm_route") {
                                          ~ adm_route
                                      } else {
                                          NULL
                                      }
                                  }
                              }
        ) + scale_x_yearmon(n = length(unique(set_reac_2()$yearmonth_adm))/4)
        
        plot_quarter <- qic(x = yearquarter_adm, y = DOT_per_day_100,
                            data = ddd_dot(),
                            agg.fun = "sum",
                            decimals = 2,
                            xlab = "Quarter",
                            ylab = "DOT per 100 bed days",
                            title = "Days of therapy (DOT) / 100 bed days per quarter",
                            facets = 
                                if (input$box3.3 == "specialty") {
                                    ~ specialty
                                } else { 
                                    if (input$box3.3 == "sub_specialty") {
                                        ~ sub_specialty
                                    } else {
                                        if (input$box3.3 == "adm_route") {
                                            ~ adm_route
                                        } else {
                                            NULL
                                        }
                                    }
                                }
        ) + scale_x_yearqtr(n = length(unique(set_reac_2()$yearquarter_adm))/4)
        
        if (input$box3.2 == "yearmonth_adm") {
            plot <- plot_month
        } else { 
            if (input$box3.2 == "yearquarter_adm") {
                plot <- plot_quarter
            } else {
                plot <- plot_year
            }
        }
        
        plot + 
            labs(caption = "(Line represents median; if red-dotted = signal for non-random variation)") +
            theme(plot.caption = element_text(size = 10, colour = "darkgrey"), 
                  plot.title = element_text(face = "bold", size = 12),
                  axis.text.x = element_text(angle = 90))
        
    })
    
    output$dot_ts <- renderPlot({
        dot_ts()
    })
    
    
    
    # BOX DIAGNOSTICS - 1  ------------------------------------------------------------------
    
    dia_adm <- reactive({
        input$confirm # confirm buttons needs to be pressed to initiate this code
        isolate({
            ts <- set_reac_1() %>% 
                group_by(id, adm_id) %>% 
                distinct(check, .keep_all = TRUE) %>% 
                ungroup() %>% 
                group_by_(input$box5.1, "check") %>% 
                summarise(n = n())
            
            plot <-
                ggplot(ts, aes_string(input$box5.1, "n", fill = "check")) +
                theme_minimal() +
                theme(plot.title = element_text(face = "bold", size = 12)) +
                ggtitle(
                    paste0("Diagnostic performed within ",
                           min(input$checkInput),
                           " and ",
                           max(input$checkInput),
                           " days \nfrom start of antimicrobials")) +
                labs(x = if (input$box5.1 == "year") {
                    paste("Per year")
                } else {
                    if (input$box5.1 == "yearquarter_adm") {
                        paste("Per quarter")
                    } else {
                        paste("Per month")
                    }
                }, y = if (input$box5.2 == "dodge") {
                    paste("Count")
                } else {
                    paste("Proportion")
                }) +
                if (input$box5.1 == "yearquarter_adm") { 
                    scale_x_yearqtr(n = 12)
                } else {
                    if (input$box5.1 == "yearmonth_adm") {
                        scale_x_yearmon(n = 12)
                    }
                }
            
            plot <- plot + 
                if (input$diagnosticsInput == "bc_timing") {
                    scale_fill_manual(
                        values = c("#a6a6a6", "#d1351b"),
                        name = "Blood cultures"
                    )
                }
            else {
                scale_fill_manual(
                    values = c("#a6a6a6", "#f39c12"),
                    name = "Urine cultures"
                )
            }
            
            plot + 
                if(input$box5.1 == "year") {
                    geom_col(position = input$box5.2,
                             color = "black",
                             alpha = 0.8)
                } else {
                    geom_area(position = input$box5.2,
                              color = "black",
                              alpha = 0.8)
                }
        })
    })
    
    output$plot_dia_adm <- renderPlot({
        dia_adm()
    })
    
    # timing plot
    
    plot_dia_timing <- reactive({
        input$confirm # confirm buttons needs to be pressed to initiate this code
        isolate({
            test_timing <- set_reac_1() %>%
                count(uc_timing, check) 
            
            ggplot(test_timing, aes(x = uc_timing, fill = check)) +
                geom_bar(color = "black", 
                         alpha = 0.8) +
                scale_fill_manual(
                    breaks = c("Not taken", "Taken"),
                    values =
                        if (input$diagnosticsInput == "bc_timing") {
                            c("#a6a6a6", "#d1351b")
                        } else {
                            c("#a6a6a6", "#f39c12")
                        },
                    labels = c("Not selected", "Selected"),
                    name = " ") +
                labs(x = "Days", y = "Count") +
                theme_minimal() +
                theme(plot.title = element_text(face = "bold", size = 12)) +
                ggtitle("Timing in relation to start of antimicrobials")
        })
    })
    
    output$plot_dia_timing <- renderPlot({
        plot_dia_timing()
        
    })
    
    
    # BOX DIAGNOSTICS - 2 --------------------------------------------------------
    
    plot_dia_perform <- reactive({
        input$confirm # confirm buttons needs to be pressed to initiate this code
        isolate({
            perform_all <- set_reac_1() %>%
                mutate(dia_perform_all = round(sum(check == "Taken") / n() * 100, 1)) %>%
                dplyr::select(input$box6.1, dia_perform_all)
            
            perform_group <- set_reac_1() %>%
                group_by_(input$box6.1) %>%
                summarise(dia_perform = round(sum(check == "Taken") / n() * 100, 1)) %>%
                left_join(perform_all) %>%
                mutate(dia_perform_diff = dia_perform - dia_perform_all) %>%
                arrange(-dia_perform_diff) %>%
                distinct(.keep_all = TRUE)
            
            ggplot(perform_group, 
                   aes_string(
                       x = paste0("reorder(", input$box6.1, ", dia_perform_diff)"), 
                       y = "dia_perform_diff", 
                       fill = "dia_perform_diff", 
                       text = paste0(input$box6.1))) +
                geom_hline(
                    yintercept = 0, 
                    linetype = 2, 
                    color = "darkgrey") +
                geom_bar(
                    stat = "identity", 
                    color = "black",
                    alpha = 0.8) +
                scale_fill_continuous(high = "#706f6f", low = "#cccccc")+
                labs(
                    y = paste0("Absolute diff. from average [", perform_all[1, 2], "%]"), 
                    x = " ") +
                guides(fill = FALSE) +
                coord_flip() +
                theme_minimal() +
                theme(plot.title = element_text(face = "bold", size = 12)) +
                ggtitle(
                    paste0("Diagnostic performed within ",
                           min(input$checkInput),
                           " and ",
                           max(input$checkInput),
                           " days \nfrom start of antimicrobials - comparison"))
        })
    })
    
    output$plot_dia_perform <- renderPlot({
        plot_dia_perform()
    })
    
    # performance table
    
    output$dia_table <- DT::renderDataTable({
        datatable(
            set_reac_1() %>%
                group_by_(input$box6.3, "check") %>%
                tally() %>%
                mutate(Proportion = paste0(round(n / sum(n) * 100, 1), "%")),
            rownames = FALSE,
            colnames =
                c("Group",
                  if (input$diagnosticsInput == "bc_timing") {
                      paste("Blood cultures")
                  } else {
                      paste("Urine cultures")
                  },
                  "n",
                  "Proportion"),
            extensions = "Buttons",
            options = list(
                dom = 'Bfrtp',
                buttons = c('csv', 'excel', 'pdf'),
                style = "bootstrap",
                lengthMenu = c(seq(5, 150, 5))
            )
        )
        
    })
    
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(input$filename, "_anti_add_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write_csv(set_base(), file)
        }
    )
    
    output$downloadMicroData <- downloadHandler(
        filename = function() {
            paste(input$filename, "_microbiology_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write_csv(test_results(), file)
        }
    )
    
    download_box <- function(exportname, plot) {
        downloadHandler(
            filename = function() {
                paste(exportname, Sys.Date(), ".png", sep = "")
            },
            content = function(file) {
                ggsave(file, plot = plot, device = "png", width = 8)
            }
        )
    }
    
    
    
    output$down_age_select <- download_box("pat_age", plot_age_select())
    output$down_year_select <- download_box("pat_year", plot_year_select())
    output$down_box_1 <- download_box("antimicrobial_use", plot_ab())
    output$down_box_2 <- download_box("DDD_groups", plot_DDD_all())
    output$down_box_3 <- download_box("DOT_groups", plot_DOT_all())
    output$down_box_5 <- download_box("diagnostics_year", dia_adm())
    output$down_box_6 <- download_box("diagnostics_timing", plot_dia_timing())
    output$down_box_7 <- download_box("diagnostics_perform", plot_dia_perform())
    output$down_box_micro <- download_box("first_isolates", micro_plot())
    output$down_box_res <- download_box("resistance", isolate_plot())
    output$down_box_res_ts <- download_box("resistance_time", isolate_ts())
    output$down_box_ddd_ts <- download_box("ddd time", ddd_ts())
    output$down_box_dot_ts <- download_box("dot time", dot_ts())
    output$down_box_los1.0 <- download_box("los_groups", plot_los()) 
    output$down_box_los2 <- download_box("km-curve", kaplan_los()$plot)
    
    
    
    
}