#Quelques données sur les investissements verts 

install.packages('rdbnomics')
install.packages("dplyr")
install.packages("readr")

library(rdbnomics)
library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)

#0. Données sur les dépenses en recherche et développement (générale,pas forcément verte) en % du PIB

df_rechdev = read_csv(file="~/données/Chercheurs en R_et_D.csv")
#Pas fonctionné mais pas forcément très pertinent car sur la R&D en général, pas de distinction de secteurs, ni de R&D verte

df_depenses_rechdev = read_csv(file="~/données/Dépenses en R_et_D.csv")

#Supprimons les colonnes inutiles

df_depenses_rechdev <- df_depenses_rechdev [,-c(2:4)]

# df_depenses_rechdev <- df_depenses_rechdev %>% rename(pays=Country Name)%>%
#                        filter(!is.na(c(1960:2008)))

colnames(df_depenses_rechdev)[1]<- "pays"

#On ne garde que quelques pays : France, Allemagne, UE, Etats-Unis et Chine par exemple.

# df_depenses_rechdev <- df_depenses_rechdev %>% filter(pays==xor('France','Germany','European Union','United States','China'))

df_depenses_rechdev_fr <- df_depenses_rechdev %>% filter(pays=='France')
df_depenses_rechdev_all <- df_depenses_rechdev %>% filter(pays=='Germany')
df_depenses_rechdev_UE <- df_depenses_rechdev %>% filter(pays=='European Union')
df_depenses_rechdev_US <- df_depenses_rechdev %>% filter(pays=='United States')
df_depenses_rechdev_chi <- df_depenses_rechdev %>% filter(pays=='China')


df_depenses_rechdev <- df_depenses_rechdev_fr %>% full_join(df_depenses_rechdev_all ,by=NULL)%>%
                       full_join(df_depenses_rechdev_UE ,by=NULL) %>%
                       full_join(df_depenses_rechdev_US ,by=NULL) %>% 
                       full_join(df_depenses_rechdev_chi ,by=NULL)

#On essaie de mettre les données au format tidy.

#On transpose
df_depenses_rechdev <- t(df_depenses_rechdev)

# colnames(df_depenses_rechdev)[0]<- "annee"

df_depenses_rechdev <- df_depenses_rechdev [-1,]

# colnames(df_depenses_rechdev)[1]<- "France"
# colnames(df_depenses_rechdev)[2]<- "Allemagne"
# colnames(df_depenses_rechdev)[3]<- "UE"
# colnames(df_depenses_rechdev)[4]<- "Etats-Unis"
# colnames(df_depenses_rechdev)[5]<- "Chine"

#On abandonne ce tableau qui est trop compliqué à mettre en forme 



