
install.packages('rdbnomics')
install.packages("dplyr")
install.packages("readr")

library(rdbnomics)
library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)

#I. La place de l'énergie nucléaire dans l'énergie produite (en millions de KwH): comparaison France-Allemagne

df6 =  read_csv(file='données/Conso_energie_nucleaire_France_Allemagne.csv')

df6 <- df6 [-c(56:57),] #On supprime les deux dernières lignes qui n'apportent pas d'infos supplémentaires.

#Pour supprimer les colonnes, on peut passer par la syntaxe tidyverse 
df6 <- df6 %>% select(-'Unit')%>% select(-'Quantity Footnotes')

colnames(df6)[1]<-"pays"
colnames(df6)[2]<-"bien"
colnames(df6)[3]<-"annee"
colnames(df6)[4]<-"quantite_produite"

df6 <- df6 %>% select(-'bien') 

df6_moy <- summarise(df6,moyenne=mean(quantite_produite))
#Quantité de production moyenne (deux pays confondus) : 276820.6 millions de KwH

df6_group_moy <-  df6 %>% group_by(pays) %>% summarise(moyenne=mean(quantite_produite))
#Quantités de production moyennes par pays :
#	France	407597.4
# Germany	141200.3
#Il y a donc une nette avance de la France en termes de production d'énergie nucléaire.


hist(df6$quantite_produite, col="yellow", main="Production d'énergie nucléaire en France et en Allemagne")
#L'histogramme montre clairement deux groupes distincts de valeur : à gauche, les valeurs basses correspondent à l'Allemagne,
#à droite les valeurs plus élevées correspondent à la France.


#Représentation graphique des évolutions de la production dans les deux pays

graph9 <- ggplot(data = df6 ) + geom_line(aes (x=annee, y =quantite_produite, color=pays))+
  scale_color_manual(values = c("#56B4E9","#E69F00"),labels = c("France","Allemagne"))+
  labs(title="Evolution comparée de la production \n d'énergie nucléaire en France et en Allemagne",x="Année", y="Quantité produite(en millions de KwH)")+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))

#Alors que la production française a fortement augmenté dans les années 90 avant de se stabiliser à un haut niveau, elle a baissé assez regulièrement en Allemagne.
#Les mix énergétiques des deux pays sont donc forcément assez différents 


#On peut regarder l'évolution dans les deux pays sur une période plus réduite (après 2000 par exemple)
df6_reduite <- df6 %>% filter(annee %in% c(2000:2017))


graph10 <- ggplot(data = df6_reduite ) + geom_line(aes (x=annee, y =quantite_produite, color=pays))+
  scale_color_manual(values = c("#56B4E9","#E69F00"),labels = c("France","Allemagne"))+
  labs(title="Evolution comparée de la production \n d'énergie nucléaire après 2000 en France et en Allemagne",x="Année", y="Quantité produite(en millions de KwH)")+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))

#Sur les vingt dernières années, tendance à la stabilité pour la France alors qu'il y a un déclin assez marqué en Allemagne.


#On peut aussi visualiser les données d'autres manières


graph11<- ggplot(df6, aes(x=annee, y=quantite_produite, fill=pays))+
geom_bar(width = 1, stat = "identity")+
scale_fill_manual(values = c("#56B4E9","#E69F00"),labels = c("France","Allemagne"))+
labs(title="Evolution comparée de la production \n d'énergie nucléaire en France et en Allemagne",x="Année", y="Quantité produite(en millions de KwH)")+
theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))
#Cette visualisation permet de voir à la fois le déséquilibre dans la production des deux pays et les tendances respectives.

#Diagramme en camembert

install.packages(scales)
library(scales)

graph12 <- graph11 + coord_polar("y", start=0) 
# geom_text(aes(y = quantite_produite/55 + c(0, cumsum(quantite_produite)[-length(quantite_produite)]), 
# label = percent(quantite_produite/100)), size=5)
#Essai non concluant pour ajouter des pourcentages
  




