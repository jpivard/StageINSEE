
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

df1= read_tsv(file="données/Greenhouse Gas,  1850-2016 (in MtCO2eq).csv")

colnames(df1)[1]<- 'Annee'
colnames(df1)[4]<- 'Allemagne'

colonnes1 <- c("France","Europe","Allemagne")
df1_long <- df1 %>% pivot_longer(colonnes1, names_to = 'pays', values_to = "value")

#Pour la comparaison des distributions, on va supprimer l'Europe.
df1_fr_all <- df1 [,-3]
colonnes2 <- c("France","Allemagne")
df1_fr_all_long <- df1 %>% pivot_longer(colonnes2, names_to = 'pays', values_to = "value")

graph1 <- ggplot(df1_fr_all_long, aes(x=pays,y= value, color=pays)) +
  scale_color_manual(values = c("#E69F00","#56B4E9"),labels = c("Allemagne","France"))+
  labs(title="Distributions des émissions totales de CO2 \n  en France et en Allemagne ", x="Pays", y="Valeur(en millions de tonnes d'équivalent CO2)")+
  geom_boxplot()+
  theme(legend.position = "none",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))

#La distribution des valeurs est plus resserrée pour la France que pour l'Allemagne, qui dépasse globalement la France.


#Comparaison des tendances
graph2a <- ggplot(df1_long, aes(x=Annee,y= value, color=pays)) +
  scale_color_manual(values = c("#E69F00","#009E73","#56B4E9"),labels = c("Allemagne","Europe","France"))+
  labs(title="Evolution des émissions totales de CO2 \n  en France, en Allemagne et dans l'UE ", x="Année", y="Valeur(en millions de tonnes d'équivalent CO2)")+
  geom_line()+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))

#Tendance globale à la hausse depuis 1850, avec des décrochages ponctuels



#On va regarder l'évolution sur les dernières années (depuis 1980 par exemple)

df1_long_reduite <- df1_long %>% filter(Annee %in% c(1980:2016))

graph2b <- ggplot(df1_long_reduite, aes(x=Annee,y= value, color=pays)) +
  scale_color_manual(values = c("#E69F00","#009E73","#56B4E9"),labels = c("Allemagne","Europe","France"))+
  labs(title="Evolution des émissions totales de CO2 \n  en France, en Allemagne et dans l'UE ", x="Année", y="Valeur(en millions de tonnes d'équivalent CO2)")+
  geom_line()+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))

#Sur cette période, la tendance est plutôt à la stabilisation dans les trois zones.



#II. Emissions par secteurs en France


df2 = read_tsv(file = "données/Emissions par secteurs Rapport Secten (en Mt).csv")

#On ne retient que les principaux secteurs émetteurs
df2 <- df2[,-c(2:4)]
df2 <- df2[,-5]
df2 <- df2[,-6]

colnames(df2)[2] <- "Energie"
colnames(df2)[3] <- "Ind_manuf"
colnames(df2)[4] <- "Residentiel_Tertiaire"

colonnes3 = c("Energie","Ind_manuf", "Residentiel_Tertiaire", "Transports")
df2_long <- df2 %>% pivot_longer(colonnes3, names_to = 'secteur', values_to = "value")


#Evolution
graph3 <- ggplot(df2_long, aes(x=Annee,y= value, color=secteur)) +
  scale_color_manual(values = c("blue","brown","orange","green"),labels = c("Industrie de l'energie","Industrie manufacturiere et construction", "Residentiel et Tertiaire", "Transports"))+
  labs(title="Evolution des émissions de CO2 \n  par secteurs en France ", x="Secteur", y="Valeur(en millions de tonnes de CO2)")+
  geom_line()+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))

#Hormis le secteur des transports qui reste stable et qui est le plus émetteur, les émissions sont en déclin (faible, sauf pour l'industrie de l'énergie où cette baisse est forte)


#Répartition
graph4 <- ggplot(df2_long, aes(x=Annee,y= value)) +
  geom_bar(aes (x=Annee, y =value, fill=secteur),stat = "identity", position = "stack")+
  scale_fill_manual(values = c("blue","brown","orange","green"),labels = c("Industrie de l'energie","Industrie manufacturiere et construction", "Residentiel et Tertiaire", "Transports"))+
  labs(title="Répartition sectorielle des émissions \n  de CO2 en France",x="Année",y="Mtoe")+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))


#Afin d'avoir une idée de l'intensité carbone des différents secteurs, on va essayer de pondérer leurs émissions par leur production.






