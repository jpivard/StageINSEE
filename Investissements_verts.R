
#Données sur les investissements verts en France


library(rdbnomics)
library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)


#I.Investissements et besoins d'investissements en France : chiffres des trois dernières périodes

dfinv_1 = read_tsv (file='~/données/Niveau investissement climat I4CE.csv')

#Plusieurs lignes sont inutiles car en fait on n'a que trois données différentes portant à chaque fois sur plusieurs années,
#on va donc simplifier le tableau.

colnames(dfinv_1)[1] <- 'annee_debut_periode'

# dfinv_1 <- dfinv_1 %>% filter (annee_debut_periode == xor(2016,2019,2024) )
#Attention : ça ne fonctionne qu'avec deux paramètres à l'intérieur...

dfinv_1 <- dfinv_1 [-c(2:3),] 
dfinv_1 <- dfinv_1 [-c(3:6),] 
dfinv_1 <- dfinv_1 [-c(4:7),] 



#II. Investissements par secteurs

dfinv_2 = read_tsv(file='~/données/Investissements par secteur I4CE.csv')

colnames(dfinv_2)[2] <- 'Montant_financement_public_annuel_actuel'
colnames(dfinv_2)[3] <- 'Nouvel_objectif_annuel'
colnames(dfinv_2)[4] <- 'Investissement_supplementaire_annuel_genere_espere'


#III.Principaux investisseurs par types de financements

dfinv_3 = read_tsv(file = '~/données/Investissements Plan I4CE.csv')

colnames(dfinv_3)[2] <- 'Investissements_historiques_2016_2018'
colnames(dfinv_3)[3] <- 'Investissements_court_terme'
colnames(dfinv_3)[4] <- 'Investissements_moyen_terme'
