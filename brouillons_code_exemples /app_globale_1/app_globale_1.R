

library(shiny)
library(highcharter)
library(dplyr)
library(tidyr)
library(readr)
library(plotly)
library(shinydashboard)

ui <- navbarPage("Quelques données sur les enjeux de la transition écologique",
                 tabPanel("Emissions de CO2 et énergie"),
                 tabPanel("Finance verte")
)