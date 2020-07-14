

plot_Kaya <- function (var, color, legend.title, min = 0, max = 100) {

# A adapter 
  
  
  # generate vector of fill colors for map
  shades <- colorRampPalette(c("white", color))(100)
  
  # constrain gradient to percents that occur between min and max
  var <- pmax(var, min)
  var <- pmin(var, max)
  percents <- as.integer(cut(var, 100, 
                             include.lowest = TRUE, ordered = TRUE))
  fills <- shades[percents]
  
  # plot choropleth map
  map("county", fill = TRUE, col = fills, 
      resolution = 0, lty = 0, projection = "polyconic", 
      myborder = 0, mar = c(0,0,0,0))
  
  # overlay state borders
  map("state", col = "white", fill = FALSE, add = TRUE,
      lty = 1, lwd = 1, projection = "polyconic", 
      myborder = 0, mar = c(0,0,0,0))
  
  # add a legend
  inc <- (max - min) / 4
  legend.text <- c(paste0(min, " % or less"),
                   paste0(min + inc, " %"),
                   paste0(min + 2 * inc, " %"),
                   paste0(min + 3 * inc, " %"),
                   paste0(max, " % or more"))
  
  legend("bottomleft", 
         legend = legend.text, 
         fill = shades[c(1, 25, 50, 75, 100)], 
         title = legend.title)
}




#IV. Analyse de l'évolution des grandeurs de l'équation de Kaya en France après 1980 (en base 100)


#Comment expliquer la baisse des émissions de CO2 depuis 1980 ? Par quel(s) canal(canaux)?

df5 = read_tsv(file='données/KAYA identity, France, 1980-2015 (in base 100).csv')

colnames(df5)[2]<- 'Contenu CO2 energie'
colnames(df5)[3]<- 'Intensite_energetique_PIB'
colnames(df5)[4]<- 'PIB par tete'

colonnes4 <- c('Contenu CO2 energie','Intensite_energetique_PIB', 'PIB par tete','Population')
df5_long <- df5 %>% pivot_longer(colonnes4, names_to = 'composantes', values_to = "value")


graph9 <- ggplot(df5_long, aes(x=Annee,y= value, color=composantes)) +
  scale_color_manual(values = c("red","blue","orange","black"),labels = c("Contenu en CO2 de l'énergie", "Intensité énergétique du PIB", "PIB par tête", "Population"))+
  labs(title="Evolution des composantes des émissions de CO2 \n  en France après 1980 ", x="Année", y="base 100")+
  geom_line()+
  theme(legend.position = "bottom",plot.title = element_text(family="TT Times New Roman", face= "bold", colour="black", size=16))

#Deux composantes ont augmenté : le PIB/tête assez fortement, et la population dans une moindre mesure
#Deux autres ont baissé assez fortement : contenu carbone de l'énergie, et intensité énergétique du PIB.


