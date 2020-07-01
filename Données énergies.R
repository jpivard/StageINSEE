

install.packages('rdbnomics')

library(rdbnomics)

#I. Consommation d'électricité

df3 <- rdb("WB", "WDI", dimensions = list(country = c("FR", "GA"), indicator = c("EG.USE.ELEC.KH.PC")))

library(dplyr)

conso_electricite <- df3 %>% select(country, original_period, value) %>%
                    rename(annee = original_period) %>%
                    group_by(country) %>%
                    filter(!is.na(value))%>%
                    
                    summarise(mean_value =mean(value))

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
  
#Ecarts-type
  
summarise(sd_value=sd(value))

#1	CN	1084.663
#2	EU	1631.064
#3	FR	2123.510
#4	US	3027.915


#II.Production d'énergie

df2 <- rdb("EIA", "INTL", dimensions = list(country = c("FR"), indicator = c("44-1-FRA-MTOE.A")))





