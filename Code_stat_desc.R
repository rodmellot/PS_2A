library(readr)
library(esquisse)
library(dplyr)

#Importation des données
generalise <- read_delim("generalise.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
demo <- read_delim("demo.csv", delim = ";",escape_double = FALSE, trim_ws = TRUE)

#Même si on a toute confiance, on vérfie la présence de valeur manquantes 
colSums(is.na(demo))
colSums(is.na(generalise))

#Création d'une table via un merge 
data_all <- merge(generalise, demo, by = "Code")

#Création de la nouvelle variable 
data_all <- data_all %>%
  mutate(tx_visite_par_population = `Nb_visite` / `Population municipale 2021`)


#Stats Desciptives 
summary(data_all)
# Créer un boxplot pour visualiser les valeurs extrêmes
boxplot(data_all$tx_visite_par_population, main = "Boxplot du taux visite/population",
        ylab = "Taux visite/population")

# Obtenir les valeurs extrêmes

# Calcul des quantiles
Q1 <- quantile(data_all$tx_visite_par_population, 0.25, na.rm = TRUE)
Q3 <- quantile(data_all$tx_visite_par_population, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Bornes pour détecter les valeurs extrêmes
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Filtrer les lignes avec les valeurs extrêmes et afficher les communes associées
communes_extremes <- data_all %>%
  filter(tx_visite_par_population < lower_bound | tx_visite_par_population > upper_bound) %>%
  select(Code, Libellé.x, tx_visite_par_population)  # Inclure les colonnes utiles


# Transformation logarithmique
boxplot(
  log10(data_all$`Population municipale 2021`),  # On a pas de données nulles donc pas de log(0)
  main = "Boxplot de la population municipale (échelle log)",
  ylab = "Log10(Population)"
)


