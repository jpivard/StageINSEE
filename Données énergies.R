

install.packages('rdbnomics')
install.packages("dplyr")
install.packages("readr")


library(rdbnomics)
library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)


#I. Consommation d'électricité

#df3 <- rdb("WB", "WDI", dimensions = list(country = c("FR", "GA"), indicator = c("EG.USE.ELEC.KH.PC")))


# conso_electricite <- df3 %>% select(country, original_period, value) %>%
#                     rename(annee = original_period) %>%
#                     group_by(country) %>%
#                     filter(!is.na(value))%>%
#                     
#                     summarise(mean_value =mean(value))

#On a obtenu la moyenne de la conso d'électricité de la France : 5122 kWh par tête , et celle du Gabon : 821 kwh/tête


#Regardons maintenant la France, l'UE, La Chine et les Etats-Unis

df1 <- rdb("WB", "WDI", dimensions = list(country = c("FR", "EU","CN","US"), indicator = c("EG.USE.ELEC.KH.PC")))

conso_electricite1 <- df1 %>% select(country, original_period, value) %>%
                  rename(annee = original_period) %>%
                  group_by(country) %>%
                  filter(!is.na(value))%>%
#Moyennes
  
summarise(mean_value =mean(value))%>%

#1	CN	1081.572
#2	EU	4404.245
#3	FR	5122.390
#4	US	10318.214 
  
#Médianes
  
summarise(median_value= median(value))

#1	CN	633.6654
#2	EU	4966.7407
#3	FR	5651.9014
#4	US	10886.8583

#Quartiles

summarise(quantile(value))

# Chine : 	151.9893	295.8529	633.6654	1431.0732	3927.0445
# UE : 1253.4195	3241.6360	4966.7407	5848.5654 6409.2442
#France : 1462.7104 3208.6764 5651.9014  7186.9120	7734.7268

  
#Ecarts-type
  
summarise(sd_value=sd(value)) %>%

#1	CN	1084.663
#2	EU	1631.064
#3	FR	2123.510
#4	US	3027.915



#Histogrammes

hist(df1$value, col="gold", main="Consommation d'électricité")   #Histogramme sur toutes les observations de consommation d'électricité.

# df1_france <- df1 %>% filter(df1, country == "FR") %>%
#               filter (!is.na(value)) %>%
# hist(df1_france$value, col="blue", main="Consommation d'électricité en France") 
# #Je voudrais faire un histogramme sur les observations pour la France seulement mais ça ne fonctionne pas


#On récupère les données d'une autre table seulement sur la France

df2= read_csv(file ='données/Energie/Conso_electricite_fr.csv')

class(as.data.frame(df2))#On a transformé le tibble en data frame

colnames(df2)[2] <- 'consommation_electricite_fr'

df2 <- df2[-c(56:60),]
#On a supprimé les dernières lignes qui ne contiennent pas de valeurs.

summarise(df2,mean(consommation_electricite_fr))
#On obtient une moyenne de 5122.39 pour la France, c'est bien la même que tout à l'heure donc c'est cohérent


hist(df2$consommation_electricite_fr, col="blue", main="Consommation d'électricité en France") 
#La majorité des observations se situe au dessus de 6000 kWh par tête (on n'a plus du tout la même répartition que dans l'histogramme précédent)
#Conclusion : La France est plutôt un gros consommateur d'électricité si on compare avec l'UE et la Chine (pas les E-U qui restent devant)


#II.Production d'énergie en France ( en millions de tonnes métriques d'équivalent pétrole ) et comparaison avec l'Allemagne


# df4 <- rdb("EIA", "INTL", dimensions = list(country = c("FR"), indicator = c("44-1-FRA-MTOE.A")))
# 
# df_dimensions <- rdb_dimensions(provider_code = "EIA", dataset_code = "INTL")   #Methode pour voir les dimensions
# df4 <- rdb("EIA", "INTL", dimensions = list(frequency = c('A'),geography = c("fra"), units = c("tril")))
# 
# #Ca fonctionne sauf que les vaelurs ne correspondent pas du tout à celles que l'on voit sur le site !

#On reprend donc l'autre méthode d'importation
df4 = read_csv(file='données/Energie/Production_energie_fr.csv')

colnames(df4)[2] <- 'production_totale_energie_fr'

summarise(df4, mean(production_totale_energie_fr))
#Production moyenne en France : 113,3214 (millions de tonnes métriques d'équivalent pétrole)

summarise(df4,quantile(production_totale_energie_fr))
#Les quantiles : 60.1110      102.4020         123.3840         128.5793        130.4080


hist(df4$production_totale_energie_fr, col="red", main="Production d'énergie en France")
#L'histogramme nous permet d'observer la prépondérance des observations situées au dessus de 120 millions de tonnes métriques.




#Regardons maintenant l'Allemagne

df5 = read_csv(file='données/Energie/Production_energie_all.csv')

colnames(df5)[2] <- 'production_totale_energie_all'

df5 <- df5[-c(1:10),]

#Il faut enlever les 11 premières lignes car pas de données avant 1991.
#Le nombre inférieur de données va sans doute biaiser un peu la comparaison.

summarise(df5, mean(production_totale_energie_all))
#On trouve une moyenne supérieure à celle de la France :134,6

hist(df5$production_totale_energie_all, col="black", main="Production d'énergie en Allemagne")
#La majorité des valeurs se situe entre 120 et 150.

#A priori, l'Allemagne produit davantage d'énergie que la France.


#Comparaison de la distribution à l'aide de boxplots

ggplot(data = df4 ) + aes (x=period, y=production_totale_energie_fr ) + geom_boxplot()
ggplot(data = df4 , aes(x=period, y=production_totale_energie_fr ))  + geom_boxplot()

ggplot(data = df5 ) + aes (x=period, y=production_totale_energie_all ) + geom_boxplot()

#On observe que la distribution des données françaises est beaucoup plus étalée que celle des données allemandes, davantage concentrées autour de la médiane.


#Evolution comparée

ggplot(data = df4 ) + aes (x=period, y=production_totale_energie_fr ) + geom_point()
#Pour la France, la tendance est à la hausse jusqu'à la fin du XXe siècle puis stabilisation

ggplot(data = df5) + aes (x=period, y=production_totale_energie_all ) + geom_point()
# Pour l'Allemagne, c'est l'inverse : on observe une baisse assez régulière depuis 1990, pour atteindre actuellement une production d'un niveau comparable à celui de la France.


#Grouper les deux derniers tableaux pour pouvoir tout voir sur le même graphique

df4 <- df4[-c(1:10),]

df_fr_all <- df4 %>% left_join(df5, by ="period", copy=FALSE)
  
class(as.data.frame(df_fr_all))

# df_fr_all <- df_fr_all[,-3]

colnames(df_fr_all)[1] <- 'annee'
colnames(df_fr_all)[2] <- 'production_totale_energie_fr'
colnames(df_fr_all)[3] <- 'production_totale_energie_all'

# plot1 <- ggplot(data=df_fr_all) + theme_classic()+ annotate("France",x=annee, y=production_totale_energie_fr, label="classic()", col="blue")
# plot2 <- ggplot(data=df_fr_all) + theme_classic()+ annotate("Allemagne",x=annee, y=production_totale_energie_all, label="classic()", col="red")
# #Ne fonctionne pas
# 
# ggplot(data = df_fr_all) + aes (x=annee, y1=production_totale_energie_fr, y2= production_totale_energie_all ,col = annee) + geom_point()
# #Non plus
# 
# 
# #Autre piste : http://www.sthda.com/french/wiki/ggplot2-combiner-plusieurs-graphiques-sur-la-m-me-page-logiciel-r-et-visualisation-de-donn-es
# install.packages("gridExtra")
# library("gridExtra")
# 
# install.packages("cowplot")
# library("cowplot")

df_fr_all <- df_fr_all %>% filter(!is.na(production_totale_energie_all))
#On supprime toutes les lignes où on a des valeurs manquantes

 "x <-c(production_totale_energie_fr)
y <-production_totale_energie_all
boxplot(x,y,col=couleurs)"


#On essaie de regrouper les deux dernières colonnes en une seule et de dupliquer la première.

colonnes <- c("production_totale_energie_fr", "production_totale_energie_all")
df_fr_all %>% pivot_longer(colonnes, names_to = "production_energie", values_to = "value") %>%

ggplot( aes(y=value, color=production_energie)) +
  geom_boxplot() + 
  theme(legend.position = "none") -> graph1

#On a les deux boxplots sur le même graphique.

colonnes <- c("production_totale_energie_fr", "production_totale_energie_all")
df_fr_all_2 <- df_fr_all %>% pivot_longer(colonnes, names_to = "production_energie", values_to = "value") 
  
ggplot(data = df_fr_all_2 ) + geom_line(aes (x=annee, y =value, color=production_energie))+theme_bw()+
  scale_color_manual(values = c("#E69F00", "#56B4E9"),labels = c("Allemagne","France"))+
  labs(title="Evolution comparée de la production \n d'énergie en France et en Allemagne")+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))
  
#On a l'évolution comparée de la production d'énergie en Allemagne et en France : les deux tendances sont clairement inversées,
#la France a dépassé l'Allemagne aux alentours de 2008.






#III.Répartitions des différentes sources d'énergie dans la consommation et dans la production primaires en France


df7 <- read_tsv(file="données/Primary Energy Consumption by source, France, 1980-2016 (in Mtoe).csv")

df8 <- read_tsv(file = "données/Primary Energy Production by source, France, 1900-2016 (in Mtoe).csv")


#Utilisons le package Zoo pour traiter les données en séries temporelles

install.packages("zoo")
library(zoo)


z1.index <- df7$Annee 
z1.data <- df7[,-1]

z1 <- zoo(z1.data,order.by = z1.index)

#z1$Nuclear

z4.index <- df8$X1
z4.data <- df8 [,-1]

z4 <- zoo(z4.data, order.by=z4.index)


#On remarque que les énergies renouvelables représentent une proportion très faible du mix énergétique (à part l'hydroélectricité)


#1. Consommation

#Evolution comparée des consommations primaires en pétrole,et nucléaire
#Puis charbon et nucléaire
#Ainsi que gaz et nucléaire

graph13a <- ggplot(data=fortify(merge(z1$Oil,z1$Nuclear),melt=TRUE)) + 
  geom_line(aes (x=Index, y =as.numeric(Value), color=Series))+
  scale_color_manual(values = c("black", "orange"),labels = c("Pétrole","Nucléaire"))+
  labs(title="Evolution comparée des consommations primaires \n  en pétrole et nucléaire en France",x="Année",y="Mtoe")+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))
#Problème axe des ordonnées à régler !!!!!!


graph13b <- ggplot(data=fortify(merge(z1$Nuclear,z1$Coal),melt=TRUE)) + 
  geom_line(aes (x=Index, y =as.numeric(Value), color=Series))+
  scale_color_manual(values = c("orange", "brown"),labels = c("Nucléaire","Charbon"))+
  labs(title="Evolution comparée des consommations primaires \n  en charbon et nucléaire en France",x="Année",y="Mtoe")+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))


graph13c <- ggplot(data=fortify(merge(z1$Nuclear,z1$Gas),melt=TRUE)) + 
  geom_line(aes (x=Index, y =as.numeric(Value), color=Series))+
  scale_color_manual(values = c("orange", "blue"),labels = c("Nucléaire","Gaz"))+
  labs(title="Evolution comparée des consommations primaires \n  en gaz et nucléaire en France",x="Année",y="Mtoe")+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))

graph13d<- ggplot(data=fortify(merge(z1$Nuclear,z1$Hydroelectricity),melt=TRUE)) + 
  geom_line(aes (x=Index, y =as.numeric(Value), color=Series))+
  scale_color_manual(values = c("orange", "green"),labels = c("Nucléaire","Hydroélectricité"))+
  labs(title="Evolution comparée des consommations primaires \n  en hydroélectricité et nucléaire en France",x="Année",y="Mtoe")+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))



#Diagrammes en bâtons : options dodge et stack

#graph14 <- ggplot(data=fortify(merge(z1$Oil,z1$Nuclear),melt=TRUE)) + 
#geom_bar(aes (x=Index, y =as.numeric(Value), fill=Series),stat = "identity", position = "dodge")

graph15a <- ggplot(data=fortify(merge(z1$Oil,z1$Nuclear),melt=TRUE)) + 
  geom_bar(aes (x=Index, y =as.numeric(Value), fill=Series),stat = "identity", position = "stack")+
  scale_fill_manual(values = c("black", "orange"),labels = c("Pétrole","Nucléaire"))+
  labs(title="Evolution comparée des consommations primaires \n  en pétrole et nucléaire en France",x="Année",y="Mtoe")+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))


graph15b <- ggplot(data=fortify(merge(z1$Nuclear,z1$Coal),melt=TRUE)) + 
  geom_bar(aes (x=Index, y =as.numeric(Value), fill=Series), stat = "identity", position = "stack")+
  scale_fill_manual(values = c("orange", "brown"),labels = c("Nucléaire","Charbon"))+
  labs(title="Evolution comparée des consommations primaires \n  en charbon et nucléaire en France",x="Année",y="Mtoe")+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))


graph15c <- ggplot(data=fortify(merge(z1$Nuclear,z1$Gas),melt=TRUE)) + 
  geom_bar(aes (x=Index, y =as.numeric(Value), fill=Series),stat = "identity", position = "stack")+
  scale_fill_manual(values = c("orange", "blue"),labels = c("Nucléaire","Gaz"))+
  labs(title="Evolution comparée des consommations primaires \n  en gaz et nucléaire en France",x="Année",y="Mtoe")+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))


graph15d <- ggplot(data=fortify(merge(z1$Nuclear,z1$Hydroelectricity),melt=TRUE)) + 
  geom_bar(aes (x=Index, y =as.numeric(Value), fill=Series),stat = "identity", position = "stack")+
  scale_fill_manual(values = c("orange", "green"),labels = c("Nucléaire","Hydroélectricité"))+
  labs(title="Evolution comparée des consommations primaires \n  en gaz et nucléaire en France",x="Année",y="Mtoe")+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))


#Barres : Proportions relatives

graph16a <- ggplot(data=fortify(merge(z1$Oil,z1$Nuclear),melt=TRUE)) + 
  geom_bar(aes (x=Index, y =as.numeric(Value), fill=Series),stat = "identity",position = "fill")+
  scale_fill_manual(values = c("black", "orange"),labels = c("Pétrole","Nucléaire"))+
  labs(title="Evolution comparée des consommations primaires \n  en pétrole et nucléaire en France",x="Année",y="Proportion relative")+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))


graph16b <- ggplot(data=fortify(merge(z1$Nuclear,z1$Coal),melt=TRUE)) + 
  geom_bar(aes (x=Index, y =as.numeric(Value), fill=Series), stat = "identity", position = "fill")+
  scale_fill_manual(values = c("orange", "brown"),labels = c("Nucléaire","Charbon"))+
  labs(title="Evolution comparée des consommations primaires \n  en charbon et nucléaire en France",x="Année",y="Proportion relative")+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))


graph16c <- ggplot(data=fortify(merge(z1$Nuclear,z1$Gas),melt=TRUE)) + 
  geom_bar(aes (x=Index, y =as.numeric(Value), fill=Series),stat = "identity", position = "fill")+
  scale_fill_manual(values = c("orange", "blue"),labels = c("Nucléaire","Gaz"))+
  labs(title="Evolution comparée des consommations primaires \n  en gaz et nucléaire en France",x="Année",y="Proportion relative")+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))



#2.¨Production

graph17a <- ggplot(data=fortify(merge(z4$Oil,z4$Nuclear),melt=TRUE)) + 
  geom_line(aes (x=Index, y =as.numeric(Value), color=Series))+
  scale_color_manual(values = c("black", "orange"),labels = c("Pétrole","Nucléaire"))+
  labs(title="Evolution comparée des productions primaires \n  en pétrole et nucléaire en France",x="Année",y="Mtoe")+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))
#Problème axe des ordonnées à régler !!!!!!


graph17b <- ggplot(data=fortify(merge(z4$Nuclear,z4$Coal),melt=TRUE)) + 
  geom_line(aes (x=Index, y =as.numeric(Value), color=Series))+
  scale_color_manual(values = c("orange", "brown"),labels = c("Nucléaire","Charbon"))+
  labs(title="Evolution comparée des productions primaires \n  en charbon et nucléaire en France",x="Année",y="Mtoe")+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))


graph17c <- ggplot(data=fortify(merge(z4$Nuclear,z4$Gas),melt=TRUE)) + 
  geom_line(aes (x=Index, y =as.numeric(Value), color=Series))+
  scale_color_manual(values = c("orange", "blue"),labels = c("Nucléaire","Gaz"))+
  labs(title="Evolution comparée des productions primaires \n  en gaz et nucléaire en France",x="Année",y="Mtoe")+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))

graph17d <- ggplot(data=fortify(merge(z4$Nuclear,z4$Hydroelectricity),melt=TRUE)) + 
  geom_line(aes (x=Index, y =as.numeric(Value), color=Series))+
  scale_color_manual(values = c("orange", "green"),labels = c("Nucléaire","Hydroélectricité"))+
  labs(title="Evolution comparée des productions primaires \n  en nucléaire et hydroélectricité en France",x="Année",y="Mtoe")+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))


#Diagrammes en bâtons : options dodge et stack

#graph14 <- ggplot(data=fortify(merge(z1$Oil,z1$Nuclear),melt=TRUE)) + 
#geom_bar(aes (x=Index, y =as.numeric(Value), fill=Series),stat = "identity", position = "dodge")

graph18a <- ggplot(data=fortify(merge(z4$Oil,z4$Nuclear),melt=TRUE)) + 
  geom_bar(aes (x=Index, y =as.numeric(Value), fill=Series),stat = "identity", position = "stack")+
  scale_fill_manual(values = c("black", "orange"),labels = c("Pétrole","Nucléaire"))+
  labs(title="Evolution comparée des productions primaires \n  en pétrole et nucléaire en France",x="Année",y="Mtoe")+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))

graph18b <- ggplot(data=fortify(merge(z4$Nuclear,z4$Coal),melt=TRUE)) + 
  geom_bar(aes (x=Index, y =as.numeric(Value), fill=Series), stat = "identity", position = "stack")+
  scale_fill_manual(values = c("orange", "brown"),labels = c("Nucléaire","Charbon"))+
  labs(title="Evolution comparée des productions primaires \n  en charbon et nucléaire en France",x="Année",y="Mtoe")+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))


graph18c <- ggplot(data=fortify(merge(z4$Nuclear,z4$Gas),melt=TRUE)) + 
  geom_bar(aes (x=Index, y =as.numeric(Value), fill=Series),stat = "identity", position = "stack")+
  scale_fill_manual(values = c("orange", "blue"),labels = c("Nucléaire","Gaz"))+
  labs(title="Evolution comparée des productions primaires \n  en gaz et nucléaire en France",x="Année",y="Mtoe")+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))


graph18d <- ggplot(data=fortify(merge(z4$Nuclear,z4$Hydroelectricity),melt=TRUE)) + 
  geom_bar(aes (x=Index, y =as.numeric(Value), fill=Series),stat = "identity", position = "stack")+
  scale_fill_manual(values = c("orange", "green"),labels = c("Nucléaire","Hydroélectricité"))+
  labs(title="Evolution comparée des productions primaires \n  en hydroélectricité et nucléaire en France",x="Année",y="Mtoe")+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))

#Barres : Proportions relatives

graph19a <- ggplot(data=fortify(merge(z4$Oil,z4$Nuclear),melt=TRUE)) + 
  geom_bar(aes (x=Index, y =as.numeric(Value), fill=Series),stat = "identity", position = "fill")+
  scale_fill_manual(values = c("black", "orange"),labels = c("Pétrole","Nucléaire"))+
  labs(title="Evolution comparée des productions primaires \n  en pétrole et nucléaire en France",x="Année",y="proportion relative")+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))


graph19b <- ggplot(data=fortify(merge(z4$Nuclear,z4$Coal),melt=TRUE)) + 
  geom_bar(aes (x=Index, y =as.numeric(Value), fill=Series), stat = "identity", position = "fill")+
  scale_fill_manual(values = c("orange", "brown"),labels = c("Nucléaire","Charbon"))+
  labs(title="Evolution comparée des productions primaires \n  en charbon et nucléaire en France",x="Année",y="proportion relative")+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))


graph19c <- ggplot(data=fortify(merge(z4$Nuclear,z4$Gas),melt=TRUE)) + 
  geom_bar(aes (x=Index, y =as.numeric(Value), fill=Series),stat = "identity", position = "fill")+
  scale_fill_manual(values = c("orange", "blue"),labels = c("Nucléaire","Gaz"))+
  labs(title="Evolution comparée des productions primaires \n  en gaz et nucléaire en France",x="Année",y="proportion relative")+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))


#Conclusion générale : les mix énergétique de la consommation et de la production sont très différents.
#Par exemple, le nucléaire représente une plus grande part de la prod que de la conso, pour les énergies fossiles c'est plutôt l'inverse.



#Astuces supplémentaires

# #On regroupe les données sur la conso et sur la prod, en se limitant à la période pour laquelle on n'a pas de valeurs manquantes
# z5 <- merge(z1,z4,all = FALSE)
# 
# 
# 
# #Pour passer d'une série temporelle fréq courte à fréq longue : aggregate
# 
# #Dans l'autre sens : il faut changer la fréquence de la zoo
# 
# #Passer d'un zoo à un data frame
# 
# df_test <- as.data.frame(z1)




#On peut aussi le faire sur des bases de données avec colonne où il y a une chaîne de caractères en utilisant as.date

#IV.Intensité du PIB en consommation primaire de pétrole ( en TEP)

df9 <- read_tsv(file='données/Total Primary Oil Consumption intensity of GDP,  1980-2016 (in toe).csv')

z2.index <- as.Date(df9$Annee,tryFormats = "%d/%m/%Y")
z2.data <- df9 [,-1]

z2 <- zoo(z2.data, order.by = as.yearmon(z2.index), frequency = 12)
#Série temporelle mensuelle

z3 <- zoo(z2.data, order.by = as.yearqtr(z2.index), frequency = 4)
#Série temporelle trimestrielle

graph20 <- ggplot(data=fortify(merge(z2$France,z2$Allemagne,z2$Europe),melt=TRUE)) + 
  geom_line(aes (x=Index, y =as.numeric(Value), color=Series))+
  scale_color_manual(values = c("#E69F00", "#56B4E9","#009E73"),labels = c("Allemagne","France","UE"))+
  labs(title="Evolution comparée des intensités du PIB\n  en consommation primaire de pétrole",x="Année",y="tonnes d'équivalent pétrole")+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))






