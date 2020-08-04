
# Liste des packages requis -----------------------------------------------

PACKAGES <- c("ggplot2","tidyr","devtools","shiny","shinydashboard","highcharter","plotly","readr","rsconnect")

devtools::install_github("tidyverse/dplyr")

#  Installation des packages

inst <- match(PACKAGES, .packages(all=TRUE))
need <- which(is.na(inst))
if (length(need) > 0) install.packages(PACKAGES[need],dependencies = TRUE)

lapply(PACKAGES, require, character.only=T)

