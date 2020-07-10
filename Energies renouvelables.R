#Energies renouvelables

install.packages('rdbnomics')
install.packages("dplyr")
install.packages("readr")


library(rdbnomics)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyverse)

#I. Consommation finale d'énergies renouvelables en France (en TOE), comparaison avec le nucléaire et le fossile

df_1 =read.csv(file='données/Energie/Consommation_finale_energies_renouvelables_France.csv')

colnames(df_1 )[1] <- 'annee'
colnames(df_1 )[2] <- 'consommation_finale_energies_renouvelables_fr'

summarise(df_1 ,mean(consommation_finale_energies_renouvelables_fr))
#Conso finale moyenne : 10139 tonnes d'équivalent pétrole

ggplot(data = df_1  ) + aes (x=annee, y=consommation_finale_energies_renouvelables_fr ) + geom_boxplot()
#Le boxplot permet d'étudier la répartition des valeurs : la médiane est un peu en dessous de 10000, la majeure partie des valeurs est comprise entre 9000 et 11000.

ggplot(data = df_1 ) + aes (x=annee, y=consommation_finale_energies_renouvelables_fr) + geom_line()
#Ce graphique met en évidence une tendance à la baisse au cours des années 90, puis à la hausse depuis 2005 (avec quelques décrochages ponctuels).


df_2 = read.csv(file='données/Energie/Consommation_finale_energie_fossile_France.csv')

colnames(df_2 )[1] <- 'annee'
colnames(df_2 )[2] <- 'consommation_finale_energies_fossiles_fr'

summarise(df_2 ,mean(consommation_finale_energies_fossiles_fr))
#Conso finale moyenne : 100133 TEP

ggplot(data = df_2  ) + aes (x=annee, y=consommation_finale_energies_fossiles_fr ) + geom_boxplot()
#Le boxplot permet d'étudier la répartition des valeurs : la médiane est un peu au dessus de 100000, la majeure partie des valeurs est comprise entre 97000 et 106000.

ggplot(data = df_2 ) + aes (x=annee, y=consommation_finale_energies_fossiles_fr) + geom_line()
#Ce graphique met en évidence une tendance à la hausse au cours des années 90, puis à la baisse depuis 2000 : évolution symétriquement opposée à celle des énergies renouvelables, qui traduit leur substituabilité avec les énergies fossiles.


#Visualisation sur un seul graphique

# df_2 <- df_2 [-28,]  #On supprime les deux dernières valeurs du second tableau pour en avoir le même nombre
# 
# df_3 <- bind_cols(df_1, df_2)
# 
# df_3 <- df_3 [,-3]

#df_2 <- df_2[-c(1:2),] #On supprime les deux dernières valeurs du second tableau pour en avoir le même nombre

df_3 <- df_1 %>% left_join(df_2, by ="annee", copy=FALSE)

col <- c("consommation_finale_energies_renouvelables_fr", "consommation_finale_energies_fossiles_fr")
df_3_long <- df_3 %>% pivot_longer(col, names_to = "consommation_finale", values_to = "value") 
  
graph3 <- ggplot(df_3_long, aes(x=consommation_finale,y=value, color=consommation_finale)) +
geom_boxplot() + 
scale_color_manual(values = c("brown", "green"),labels = c("Energies fossiles","Energies renouvelables"))
theme(legend.position = "none") 
#On voit ainsi que la distribution des valeurs est beaucoup plus étalée pour les énergies fossiles (à gauche).

graph4 <- ggplot(data = df_3_long ) + aes (x=annee, y = value , col =consommation_finale) + geom_line() +
scale_color_manual(values = c("brown", "green"),labels = c("Energies fossiles","Energies renouvelables"))+
labs(title="Evolution comparée de la consommation \n d'énergies fossiles et renouvelables en France", x="Année", y="Valeur")+
theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))

#On constate ainsi que les évolutions sont d'ampleur relativement modérées et que l'écart reste important, signe d'inertie des modes de consommation de l'énergie en France.



#Comparaison avec (d'autres pays de) l'UE

#Sur l'évolution de la consommation finale d'énergies renouvelables dans l'UE

df_4 = read.csv(file = 'données/Energie/conso_finale_energies_renouvelables_UE.csv')

colnames(df_4 )[1] <- 'annee'
colnames(df_4 )[2] <- 'consommation_finale_energies_renouvelables_UE'

ggplot(data = df_4  ) + aes (x=annee, y=consommation_finale_energies_renouvelables_UE ) + geom_boxplot()
#Pas très parlant

ggplot(data = df_4 ) + aes (x=annee, y=consommation_finale_energies_renouvelables_UE) + geom_line()
#L'évolution est encore un peu différente à l'échelle de l'Europe : progression quasi constante depuis les années 90 (même au moment où ça baissait en France)


#Comparaison avec l'Allemagne (pour comparer les valeurs cette fois et pas seulement la tendance)

df_5 = read.csv(file='données/Energie/conso_finale_energies_renouvelables_all.csv')

colnames(df_5)[1] <- 'annee'
colnames(df_5)[2] <- 'consommation_finale_energies_renouvelables_All'

ggplot(data = df_5) + aes (x=annee, y=consommation_finale_energies_renouvelables_All ) + geom_boxplot()
#Le boxplot nous montre déjà que les valeurs sont sensiblement inférieures à celle de la France 

ggplot(data = df_5) + aes (x=annee, y=consommation_finale_energies_renouvelables_All) + geom_line()
#Tendance à la hausse depuis les années 90, mais des niveaux de consommation inférieurs à ceux de la France.


#Essayer de tout visualiser sur un seul graphique

colnames(df_1)[2] <- 'France'
colnames(df_5)[2] <- 'Allemagne'
colnames(df_4)[2] <- 'UE'

df_6 <- df_1 %>% left_join(df_5, by ="annee", copy=FALSE) %>% left_join(df_4, by ="annee", copy=FALSE)

col_2 <- c("France", "Allemagne","UE")
df_6_long <- df_6 %>% pivot_longer(col_2, names_to = "consommation_finale_energies_renouvelables", values_to = "value") 

graph5 <- ggplot(df_6_long, aes(x=consommation_finale_energies_renouvelables,y=value, color=consommation_finale_energies_renouvelables)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9","#009E73"),labels = c("Allemagne","France","UE"))+
  geom_boxplot()+
  labs(title="Distributions de la consommation \n d'énergies renouvelables en France,\n en Allemagne et dans l'UE", x="Pays", y="Valeur(en TEP)")+
  theme(legend.position = "none",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))
#La distribution des valeurs est beaucoup plus étalée pour l'Allemagne  que pour la France 


#II. Production primaire d'énergies renouvelables (en TEP)


df_7 =read.csv(file='données/Energie/Production_primaire_energies_renouvelables.csv')

colnames(df_7)[1] <- 'annee'
colnames(df_7)[2] <- 'France'
colnames(df_7)[4] <- 'Allemagne'
colnames(df_7)[3] <- 'UE' 

colonnes_2<- c("France", "UE","Allemagne") 
df_7_long <- df_7 %>% pivot_longer(colonnes_2, names_to = "production_primaire_energies_renouvelables", values_to = "value") 

graph7 <- ggplot(df_7_long, aes(x=production_primaire_energies_renouvelables,y=value, color=production_primaire_energies_renouvelables)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9","#009E73"),labels = c("Allemagne","France","UE"))+
  labs(title="Distributions de la production primaire \n d'énergies renouvelables en France,\n en Allemagne et dans l'UE", x="Pays", y="Valeur(en TEP)")+
  geom_boxplot()+
  theme(legend.position = "none",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))
  

# La distribution des valeurs pour la France est beaucoup plus étalée : de moins de 10000 TEP à près de 40000, alors que pour l'Allemagne elle est très resserrée (entre 15000 et 22500, à deux exceptions près)

# ggplot(data = df_6  ) + aes (x=annee, y=production_primaire_energies_renouvelables_France ) + geom_line()
# ggplot(data = df_6  ) + aes (x=annee, y=production_primaire_energies_renouvelables_Allemagne ) + geom_line()
# ggplot(data = df_6  ) + aes (x=annee, y=production_primaire_energies_renouvelables_UE) + geom_line()

#Sur un même graphique

graph8 <- ggplot(data = df_7_long ) + aes (x=annee, y = value , col =production_primaire_energies_renouvelables) + geom_line() +
  scale_color_manual(values = c("#E69F00", "#56B4E9","#009E73"),labels = c("Allemagne","France","UE"))+
  labs(title="Evolution comparée de la production primaire \n d'énergies renouvelables en France,\n en Allemagne et dans l'UE", x="Année", y="Valeur(en tonnes d'équivalent pétrole)")+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))+
  labs(color = "Pays")

#Progression continue et rapide en France et dans l'UE, progression plus tardive et plus lente en Allemagne, dépassée par la France au milieu des années 2000.
#On a donc une situation différente (presque symétriquement opposée)à celle de la consommation.

#On regroupe les deux pour visualiser sur Shiny

df_groupe <-  df_6 %>% left_join(df_7, by ="annee", copy=FALSE)


#III. Parts des ER dans la conso et la prod d'énergie primaire


df_8 =  read_tsv(file='données/Renewable_Energy_Consumption_share_of_primary_energy.csv')
#Attention : pour les fichiers dont les colonnes sont séparées par tab, il faut utiliser read_tsv.

df_9 = read_tsv(file='données/Renewable_Energy_Production_share_of_primary_energy.csv')

#On enlève le Royaume-Uni (pour des raisons pratiques)
df_8 <- df_8[,-5]


#Consommation

colonnes_3<- c("France", "Allemagne","Italie","UE") 
df_8_long <- df_8 %>% pivot_longer(colonnes_3, names_to = "pays", values_to = "part_energies_renouvelables_conso_primaire") 


graph17 <- ggplot(df_8_long, aes(x=Annee,y=part_energies_renouvelables_conso_primaire, color=pays),lwd=2) +
  scale_color_manual(values = c("#E69F00", "#56B4E9","red","#009E73"),labels = c("Allemagne","France","Italie","UE"))+
  geom_line()+
  labs(title="Part des énergies renouvelables \n dans la consommation primaire \n en France, en Allemagne, en Italie et dans l'UE", x="Pays", y="en pourcentage")+
  theme(plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))

#La part des ER est la plus élevée en Italie (avec une forte progression depuis 2005), puis dans l'UE.
#C'est en France que l'on consomme le moins d'ER en proportion (à peine plus de 10% du mix énergétique), malgré une tendance récente à la hausse également.



#Voyons s'il en va de même pour la part dans la production.


colonnes_4<- c("France", "Allemagne","Italie","UE") 
df_9_long <- df_9 %>% pivot_longer(colonnes_4, names_to = "pays", values_to = "part_energies_renouvelables_prod_primaire") 


graph18 <- ggplot(df_9_long, aes(x=Annee,y=part_energies_renouvelables_prod_primaire, color=pays),lwd=2) +
  scale_color_manual(values = c("#E69F00", "#56B4E9","red","#009E73"),labels = c("Allemagne","France","Italie","UE"))+
  geom_line()+
  labs(title="Part des énergies renouvelables \n dans la production primaire \n en France, en Allemagne, en Italie et dans l'UE", x="Pays", y="en pourcentage")+
  theme(plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))

#Les énergies renouvelables représentant la majeure partie de la production en Italie (aussi pour des raisons géographiques), avec une proportion pouvant dépasser les 3/4 quoique très fluctuante.
#Dans les trois autres zones, elle est en progression globale mais demeure bien inférieure.
#En Allemagne, croissance exponentielle depuis les années 80, alors qu'en France pic à la fin des années 70 inégalé depuis.






















