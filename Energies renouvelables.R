#Energies renouvelables

install.packages('rdbnomics')
install.packages("dplyr")
install.packages("readr")

library(rdbnomics)
library(dplyr)
library(readr)

#I.Consommation mondiale d'énergies renouvelables en TWh

df6 = read.csv(file = 'Conso_energies_renouvelables_monde.csv')

colnames(df6)[1] <- 'consommation_energies_renouvelables_monde'

summarise(df6,mean(consommation_energies_renouvelables_monde))
#On obtient un nombre pas très parlant

#Il est peut-être plus intéressant de voir l'évolution 

install.packages('ggfortify')
library(ggfortify)

ggplot(df6,aes(x=date))+ geom_line(aes(y=consommation_energies_renouvelables_monde))

#Problème : la présentation des données dans le tableau ne permet pas de les représenter.
#Il faudra utiliser le package lubridate pour gérer l'affichage des dates.
#https://statistique-et-logiciel-r.com/gerer-les-dates-et-les-heures-avec-le-package-lubridate/





