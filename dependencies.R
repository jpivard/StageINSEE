
# Liste des packages requis -----------------------------------------------

PACKAGES <- c("ggplot2","tidyr","dplyr","shiny","shinydashboard","highcharter","plotly","readr","rsconnect")

#  Installation des packages

inst <- match(PACKAGES, .packages(all=TRUE))
need <- which(is.na(inst))
if (length(need) > 0) install.packages(PACKAGES[need],repos = "https://cran.univ-paris1.fr/")

lapply(PACKAGES, require, character.only=T)

