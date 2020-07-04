#Energies renouvelables

install.packages('rdbnomics')
install.packages("dplyr")
install.packages("readr")


library(rdbnomics)
library(dplyr)
library(readr)


#I. Consommation finale d'énergies renouvelables en France (en TOE), comparaison avec le nucléaire et le fossile

df_1 =read.csv(file='Consommation_finale_energies_renouvelables_France.csv')

colnames(df_1 )[1] <- 'annee'
colnames(df_1 )[2] <- 'consommation_finale_energies_renouvelables_fr'

summarise(df_1 ,mean(consommation_finale_energies_renouvelables_fr))
#Conso finale moyenne : 10139 tonnes d'équivalent pétrole

ggplot(data = df_1  ) + aes (x=annee, y=consommation_finale_energies_renouvelables_fr ) + geom_boxplot()
#Le boxplot permet d'étudier la répartition des valeurs : la médiane est un peu en dessous de 10000, la majeure partie des valeurs est comprise entre 9000 et 11000.

ggplot(data = df_1 ) + aes (x=annee, y=consommation_finale_energies_renouvelables_fr) + geom_line()
#Ce graphique met en évidence une tendance à la baisse au cours des années 90, puis à la hausse depuis 2005.


df_2 = read.csv(file='Consommation_finale_energie_fossile_France.csv')

colnames(df_2 )[1] <- 'annee'
colnames(df_2 )[2] <- 'consommation_finale_energies_fossiles_fr'

summarise(df_2 ,mean(consommation_finale_energies_fossiles_fr))
#Conso finale moyenne : 100133 TEP

ggplot(data = df_2  ) + aes (x=annee, y=consommation_finale_energies_fossiles_fr ) + geom_boxplot()
#Le boxplot permet d'étudier la répartition des valeurs : la médiane est un peu au dessus de 100000, la majeure partie des valeurs est comprise entre 97000 et 106000.

ggplot(data = df_2 ) + aes (x=annee, y=consommation_finale_energies_fossiles_fr) + geom_line()
#Ce graphique met en évidence une tendance à la hausse au cours des années 90, puis à la baisse depuis 2000 : évolution symétriquement opposée à celle des ER.


#Visualisation sur un seul graphique

df_2 <- df_2 [-28,]  #On supprime les deux dernières valeurs du second tableau pour en avoir le même nombre

df_3 <- bind_cols(df_1, df_2)

df_3 <- df_3 [,-3]


col <- c("consommation_finale_energies_renouvelables_fr", "consommation_finale_energies_fossiles_fr") %>%
df_3 <- df_3 %>% pivot_longer(col, names_to = "consommation_finale", values_to = "value") %>%
  

ggplot(df_3, aes(x=annee, y=value, color=consommation_finale)) +
geom_boxplot() + 
theme(legend.position = "none")


ggplot(data = df_3 ) + aes (x=annee, y = value , col = consommation_finale) + geom_line() 


#La méthode qui a fonctionné dans le fichier précédent ne fonctionne plus...





  
  
#Malheureusement pas de données comparables sur le nucléaire
  


#Comparaison avec (d'autres pays de) l'UE

#Sur l'évolution de la consommation finale d'énergies renouvelables dans l'UE

df_4 = read.csv(file = 'conso_finale_energies_renouvelables_UE.csv')

colnames(df_4 )[1] <- 'annee'
colnames(df_4 )[2] <- 'consommation_finale_energies_renouvelables_UE'

ggplot(data = df_4  ) + aes (x=annee, y=consommation_finale_energies_renouvelables_UE ) + geom_boxplot()
#Pas très parlant

ggplot(data = df_4 ) + aes (x=annee, y=consommation_finale_energies_renouvelables_UE) + geom_line()
#L'évolution est encore un peu différente à l'échelle de l'Europe : progression quasi constante depuis les années 90 (même au moment où ça baissait en France)


#Comparaison avec l'Allemagne (pour comparer les valeurs cette fois et pas seulement la tendance)

df_5 = read.csv(file='conso_finale_energies_renouvelables_all.csv')

colnames(df_5)[1] <- 'annee'
colnames(df_5)[2] <- 'consommation_finale_energies_renouvelables_All'

ggplot(data = df_5) + aes (x=annee, y=consommation_finale_energies_renouvelables_All ) + geom_boxplot()
#Le boxplot nous montre déjà que les valeurs sont sensiblement inférieures à celle de la France 

ggplot(data = df_5) + aes (x=annee, y=consommation_finale_energies_renouvelables_All) + geom_line()
#Tendance à la hausse depuis les années 90, mais des niveaux de consommation inférieurs à ceux de la France.

#Essayer de visualiser sur un seul graphique : A FAIRE 




#II. Production primaire d'énergies renouvelables


df_6 =read.csv(file='Production_primaire_energies_renouvelables.csv')

colnames(df_6)[1] <- 'annee'
colnames(df_6)[2] <- 'production_primaire_energies_renouvelables_France'
colnames(df_6)[3] <- 'production_primaire_energies_renouvelables_UE'
colnames(df_6)[4] <- 'production_primaire_energies_renouvelables_Allemagne'

colonnes_2<- c("production_primaire_energies_renouvelables_France", "production_primaire_energies_renouvelables_Allemagne","production_primaire_energies_renouvelables_UE") %>%
df_6 <- df_6 %>% pivot_longer(colonnes_2, names_to = "production_primaire", values_to = "value") %>%
#Ne fonctionne toujours pas...
  
ggplot(data = df_6  ) + aes (x=annee, y=production_primaire_energies_renouvelables_France ) + geom_boxplot()
ggplot(data = df_6  ) + aes (x=annee, y=production_primaire_energies_renouvelables_Allemagne ) + geom_boxplot()
#A METTRE SUR UN SEUL GRAPHIQUE
# La distribution des valeurs pour la France est beaucoup plus étalée : de moins de 10000 TEP à près de 40000, alors que pour l'Allemagne elle est très resserrée (entre 15000 et 22500, à deux exceptions près)

ggplot(data = df_6  ) + aes (x=annee, y=production_primaire_energies_renouvelables_France ) + geom_line()
ggplot(data = df_6  ) + aes (x=annee, y=production_primaire_energies_renouvelables_Allemagne ) + geom_line()
ggplot(data = df_6  ) + aes (x=annee, y=production_primaire_energies_renouvelables_UE) + geom_line()

#Progession continue et rapide en France et dans l'UE, progression plus tardive et plus lente en Allemagne











