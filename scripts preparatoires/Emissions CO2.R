
install.packages('rdbnomics')
install.packages("dplyr")
install.packages("readr")


library(rdbnomics)
library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)

#Données sur les émissions de CO2 / le contenu carbone de l'économie française


#I. Emissions de CO2 globales de la France, et comparaison avec l'Allemagne et avec la tendance européenne

df_CO2= read_tsv(file="~/données/Greenhouse Gas,  1850-2016 (in MtCO2eq).csv")

colnames(df_CO2)[1]<- 'Annee'
colnames(df_CO2)[4]<- 'Allemagne'

colonnes1 <- c("France","Europe","Allemagne")
df_CO2_long <- df_CO2 %>% pivot_longer(colonnes1, names_to = 'pays', values_to = "value")

#Pour la comparaison des distributions, on va supprimer l'Europe.
df_CO2_fr_all <- df_CO2[,-3]
colonnes2 <- c("France","Allemagne")
df_CO2_fr_all_long<- df_CO2_fr_all %>% pivot_longer(colonnes2, names_to = 'pays', values_to = "value")
                


graph1 <- ggplot(df_CO2_fr_all_long, aes(x=pays,y= value, color=pays)) +
  scale_color_manual(values = c("#E69F00","#56B4E9"),labels = c("Allemagne","France"))+
  labs(title="Distributions des émissions totales de CO2 \n  en France et en Allemagne ", x="Pays", y="Valeur(en millions de tonnes d'équivalent CO2)")+
  geom_boxplot()+
  theme(legend.position = "none",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))

#La distribution des valeurs est plus resserrée pour la France que pour l'Allemagne, qui dépasse globalement la France.


#Comparaison des tendances
graph2a <- ggplot(df_CO2_fr_all_long, aes(x=Annee,y= value, color=pays)) +
  scale_color_manual(values = c("#E69F00","#009E73","#56B4E9"),labels = c("Allemagne","Europe","France"))+
  labs(title="Evolution des émissions totales de CO2 \n  en France, en Allemagne et dans l'UE ", x="Année", y="Valeur(en millions de tonnes d'équivalent CO2)")+
  geom_line()+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))

#Tendance globale à la hausse depuis 1850, avec des décrochages ponctuels



#On va regarder l'évolution sur les dernières années (depuis 1980 par exemple)

df_CO2_fr_all_long_reduite <- df_CO2_fr_all_long %>% filter(Annee %in% c(1980:2016))

graph2b <- ggplot(df_CO2_fr_all_long_reduite, aes(x=Annee,y= value, color=pays)) +
  scale_color_manual(values = c("#E69F00","#009E73","#56B4E9"),labels = c("Allemagne","Europe","France"))+
  labs(title="Evolution des émissions totales de CO2 \n  en France, en Allemagne et dans l'UE ", x="Année", y="Valeur(en millions de tonnes d'équivalent CO2)")+
  geom_line()+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))

#Sur cette période, la tendance est plutôt à la stabilisation dans les trois zones.



#II. Emissions par secteurs en France


df_secteurs = read_tsv(file = "~/données/Emissions par secteurs Rapport Secten (en Mt).csv")

#On ne retient que les principaux secteurs émetteurs
df_secteurs <- df_secteurs[,-c(2:4)]
df_secteurs <- df_secteurs[,-5]
df_secteurs <- df_secteurs[,-6]

colnames(df_secteurs)[2] <- "Energie"
colnames(df_secteurs)[3] <- "Ind_manuf"
colnames(df_secteurs)[4] <- "Residentiel_Tertiaire"

colonnes3 = c("Energie","Ind_manuf", "Residentiel_Tertiaire", "Transports")
df_secteurs_long <- df_secteurs %>% pivot_longer(colonnes3, names_to = 'secteur', values_to = "value")


#Evolution
graph3 <- ggplot(df_secteurs_long, aes(x=Annee,y= value, color=secteur)) +
  scale_color_manual(values = c("blue","brown","orange","green"),labels = c("Industrie de l'energie","Industrie manufacturiere et construction", "Residentiel et Tertiaire", "Transports"))+
  labs(title="Evolution des émissions de CO2 \n  par secteurs en France ", x="Secteur", y="Valeur(en millions de tonnes de CO2)")+
  geom_line()+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))

#Hormis le secteur des transports qui reste stable et qui est le plus émetteur, les émissions sont en déclin (faible, sauf pour l'industrie de l'énergie où cette baisse est forte)


#Répartition
graph4 <- ggplot(df_secteurs_long, aes(x=Annee,y= value)) +
  geom_bar(aes (x=Annee, y =value, fill=secteur),stat = "identity", position = "stack")+
  scale_fill_manual(values = c("blue","brown","orange","green"),labels = c("Industrie de l'energie","Industrie manufacturiere et construction", "Residentiel et Tertiaire", "Transports"))+
  labs(title="Répartition sectorielle des émissions \n  de CO2 en France",x="Année",y="Mtoe")+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))


#Afin d'avoir une idée de l'intensité carbone des différents secteurs, on va essayer de pondérer leurs émissions par leur production.

#Difficultés pratiques (différence des découpages sectoriels) nous ont conduit à mettre l'idée de côté.



#III. Intensité carbone du PIB (en tonnes d'équivalent CO2 par dollar ) et de l'énergie

df_intensite_PIB = read_csv(file='~/données/Emissions intensity of GDP data.csv')

df_intensite_energie = read_csv(file='~/données/Emissions intensity of primary energy data.csv')

#1.PIB

df_intensite_PIB_past <- df_intensite_PIB[-c(83:90),] 
df_intensite_PIB_past <- df_intensite_PIB_past[-28,]
#On retire les colonnes correspondant à des prévisions et la donnée supplémentaire pour l'UE (on prend la dernière pour simplifier)

df_intensite_PIB_past$year<- c(rep(seq(1990,2016),3))
                  
#On supprime les colonnes inutiles et on renomme les colonnes restantes

df_intensite_PIB_past <- df_intensite_PIB_past[,-c(1:2)]
df_intensite_PIB_past <- df_intensite_PIB_past[,-2]
df_intensite_PIB_past <- df_intensite_PIB_past[,-3]

colnames(df_intensite_PIB_past)[1] <- 'pays'
colnames(df_intensite_PIB_past)[2] <- 'valeur'
colnames(df_intensite_PIB_past)[3] <- 'annee'

#Evolution comparée des intensités en CO2 du PIB

graph5 <- ggplot(df_intensite_PIB_past, aes(x=annee,y= valeur, color=pays)) +
  scale_color_manual(values = c("#E69F00","#009E73","#56B4E9"),labels = c("Allemagne","Europe","France"))+
  labs(title="Evolution des intensités en émissions de CO2 \n  du PIB en France, en Allemagne et en Europe ", x="Secteur", y="Valeur(en tonnes d'équivalent CO2 par dollar)")+
  geom_line()+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))

#Deux enseignements :
#Tendance globale à la baisse, qui peut s'expliquer soit par une réduction des émissions soit par une hausse du PIB plus rapide que celle des émissions, soit les deux à la fois (le plus plausible)
#La France est nettement sous l'Allemagne et sous la moyenne européenne.

graph6 <- ggplot(df_intensite_PIB_past, aes(x=annee,y= valeur, fill=pays)) +
  scale_fill_manual(values = c("#E69F00","#009E73","#56B4E9"),labels = c("Allemagne","Europe","France"))+
  labs(title="Evolution des intensités en émissions de CO2 \n  du PIB en France, en Allemagne et en Europe ", x="Secteur", y="Valeur(en tonnes d'équivalent CO2 par dollar)")+
  geom_area()+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))

#Evolution en proportions relatives : l'Allemagne et l'Europe ont diminué plus vite que la France.


#2.Intensité carbone de l'énergie primaire ( en tonnes de CO2 par TJ)

df_intensite_energie_past <- df_intensite_energie[-c(81:86),] 
df_intensite_energie_past <- df_intensite_energie_past[-80,] 
df_intensite_energie_past <- df_intensite_energie_past[-53,] 
#On retire les colonnes correspondant à des prévisions et les données supplémentaires pour l'UE et l'Allemagne (on prend la dernière pour simplifier)

df_intensite_energie_past$year<- c(rep(seq(1990,2015),3))

#On supprime les colonnes inutiles et on renomme les colonnes restantes

df_intensite_energie_past <- df_intensite_energie_past[,-c(1:2)]
df_intensite_energie_past <- df_intensite_energie_past[,-2]
df_intensite_energie_past <- df_intensite_energie_past[,-3]

colnames(df_intensite_energie_past)[1] <- 'pays'
colnames(df_intensite_energie_past)[2] <- 'valeur'
colnames(df_intensite_energie_past)[3] <- 'annee'

#Evolution comparée des intensités en CO2 de l'énergie primaire

graph7 <- ggplot(df_intensite_energie_past, aes(x=annee,y= valeur, color=pays)) +
  scale_color_manual(values = c("#E69F00","#009E73","#56B4E9"),labels = c("Allemagne","Europe","France"))+
  labs(title="Evolution des intensités en émissions de CO2 \n  de l'énergie primaire en France, en Allemagne et en Europe ", x="Année", y="Valeur(en tonnes d'équivalent CO2 par térajoule)")+
  geom_line()+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))

#Ce graphique confirme les deux tendances observées pour l'intensité carbone du PIB.


# graph8 <- ggplot(df_intensite_energie_past, aes(x=annee,y= valeur)) +
#   geom_bar(aes (x=annee, y =valeur, fill=secteur),stat = "identity", position = "stack")+
#   scale_fill_manual(values = c("#E69F00","#009E73","#56B4E9"),labels = c("Allemagne","Europe","France"))+
#   labs(title="Evolution des intensités en émissions de CO2 \n  de l'énergie primaire en France, en Allemagne et en Europe ", x="Année", y="Valeur(en tonnes d'équivalent CO2 par térajoule)")+
#   theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))+

  
#Problème pour représenter diagramme en barres.



#IV. Analyse de l'évolution des grandeurs de l'équation de Kaya en France après 1980 (en base 100)


#Comment expliquer la baisse des émissions de CO2 depuis 1980 ? Par quel(s) canal(canaux)?

df_Kaya = read_tsv(file='~/données/KAYA identity, France, 1980-2015 (in base 100).csv')

colnames(df_Kaya)[2]<- 'Contenu CO2 energie'
colnames(df_Kaya)[3]<- 'Intensite_energetique_PIB'
colnames(df_Kaya)[4]<- 'PIB par tete'

colonnes4 <- c('Contenu CO2 energie','Intensite_energetique_PIB', 'PIB par tete','Population')
df_Kaya_long <- df_Kaya %>% pivot_longer(colonnes4, names_to = 'composantes', values_to = "value")


graph9 <- ggplot(df_Kaya_long, aes(x=Annee,y= value, color=composantes)) +
  scale_color_manual(values = c("red","blue","orange","black"),labels = c("Contenu en CO2 de l'énergie", "Intensité énergétique du PIB", "PIB par tête", "Population"))+
  labs(title="Evolution des composantes des émissions de CO2 \n  en France après 1980 ", x="Année", y="base 100")+
  geom_line()+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))

#Deux composantes ont augmenté : le PIB/tête assez fortement, et la population dans une moindre mesure
#Deux autres ont baissé assez fortement : contenu carbone de l'énergie, et intensité énergétique du PIB.


