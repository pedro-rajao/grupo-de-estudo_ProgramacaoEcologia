###############################################
# Data: 14-out-2022
# Nome: matriz_pokemon_14_10_2022_14_10_2022.R
# Autor: Thiago Amorim
# E-mail: azevedoamorim@hotmail.com
# Descricao: 
# Obs.: 
################################################


##### pacotes ####
require(tidyverse)
require(picante)
##################

setwd('/home/curadoria/Downloads/17_10_2022')
##############################################################


pokemon <- read.csv("pocket_monster.csv")
#View(pokemon)

poke_name <- pokemon$name

hab <- pokemon$abilities |>
    strsplit(", ")

tam <- max(sapply(hab, length))

for (i in 1:length(hab)) {
    tm <- length(hab[[i]])
    hab[[i]] <- c(hab[[i]], rep("", tam - tm))
}

poke <- rep(poke_name, tam)

abilities <- data.frame(do.call('rbind', hab))
str(abilities)
#rownames(abilities) <- pokemon$name
#View(abilities)

hh <- stack(abilities[, 1:6])

frequencia <- data.frame("name" = poke, "abilities" = hh$values)
#View(frequencia)

freq_hab <- frequencia[nchar(frequencia$abilities) > 0, ]

freq_poke <- table(freq_hab)

matriz_habilidades <- sample2matrix(matrix2sample(freq_poke))
min(rowSums(matriz_habilidades))
max(rowSums(matriz_habilidades))

#View(matriz_habilidades[poke_name,])

matriz_pokemon <- matriz_habilidades[poke_name,]

write.csv(matriz_pokemon, 'abilites_pokemon.csv')

bray_poke <- vegdist(matriz_pokemon, "gow")
pcoa_poke <- cmdscale(bray_poke)

plot(pcoa_poke)
