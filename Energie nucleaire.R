
install.packages('rdbnomics')
install.packages("dplyr")
install.packages("readr")

library(rdbnomics)
library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)

#I. La place de l'énergie nucléaire dans l'énergie produite (en millions de KwH): comparaison France-Allemagne

df6 =  read_csv(file='~/données/Conso_energie_nucleaire_France_Allemagne.csv')

df6 <- df6 [-c(56:57),] #On supprime les deux dernières lignes qui n'apportent pas d'infos supplémentaires.

#Pour supprimer les colonnes, on peut passer par la syntaxe tidyverse 
df6 <- df6 %>% select(-'Unit')%>% select(-'Quantity Footnotes')

colnames(df6)[1]<-"pays"
colnames(df6)[2]<-"bien"
colnames(df6)[3]<-"annee"
colnames(df6)[4]<-"quantite produite"

df6 <- df6 %>% select(-'bien')

#Exemple pour ne garder que certaines valeurs
df6_test <- df6 %>% filter(annee %in% c(2000:2017))



