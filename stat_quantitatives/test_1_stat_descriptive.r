library(FactoMineR)
library(tidyverse)
library(ggplot2)
library(dplyr)

# Chargement des données = le csv complet (ESLO + MPF)
data <- read.csv(file.choose(), header=TRUE, sep = ",")

# Pré-visualisation rapide de nos données

head(data)
sum <- summary(data)
write.csv(sum, "./COURS/Master_NLP/COURS/M2/S2/projet_tuteure/summary_dataset.csv", row.names = TRUE)


# PARTIE 1 : STATISTIQUES QUANTITATIVES SIMPLES



#######
# AGE #
#######


boxplot(data$age_loc)
hist(data$age_loc, main = "Distribution de l'âge des locuteurs", xlab = "âge réel", ylab = "Fréquence", col = "lightblue", border = "black")

# On isole la tranche d'âge pour visualiser les données sous forme de camembert

data_tranche_age <- table(data$classe_nouv)
data_tranche_age_df <- as.data.frame(data_tranche_age)
colnames(data_tranche_age_df)[colnames(data_tranche_age_df) == "Var1"] <- "tranche_age"
data_tranche_age_df$Percentage <- round((data_tranche_age_df$Freq / sum(data_tranche_age_df$Freq)) * 100)
data_tranche_age_df
write.csv(sum, "./COURS/Master_NLP/COURS/M2/S2/projet_tuteure/graphiques_stat_quanti/data_%_tranche_age.csv", row.names = TRUE)

pie_chart <- ggplot(data_tranche_age_df, aes(x = "", y = Percentage, fill = tranche_age)) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.1)) +
  coord_polar(theta = "y") +
  ggtitle("Répartition des locuteurs par tranche d'âge (en pourcentage)") +
  theme(legend.position = "bottom")

print(pie_chart)


###################
# NIVEAU D'ETUDES #
###################



# La répartition des locuteurs en fonction du niveau d'étude

data_niveau_etude <- table(data$niveau_etude_normalise)
data_niveau_etude
data_niveau_etude_df <- as.data.frame(data_niveau_etude)
colnames(data_niveau_etude_df)[colnames(data_niveau_etude_df) == "Var1"] <- "Niveau d'études"

# Petit nettoyage et regroupement des données
data_niveau_etude_df$`Niveau d'études` <- tolower(data_niveau_etude_df$`Niveau d'études`)
data_niveau_etude_df$`Niveau d'études` <- trimws(data_niveau_etude_df$`Niveau d'études`)
# data_niveau_etude_df$`Niveau d'études` <- gsub("(\\b4ème\\b|\\b5ème\\b|\\b3ème\\b|\\btroisième\\b|\\btroisieme\\b|\\b4eme\\b|\\b3eme\\b)", "collège", data_niveau_etude_df$`Niveau d'études`)
# data_niveau_etude_df$`Niveau d'études` <- gsub("lycéen|terminale|terminal", "lycée", data_niveau_etude_df$`Niveau d'études`)
# data_niveau_etude_df$`Niveau d'études` <- gsub(".*\\lycée\\b.*", "lycée", data_niveau_etude_df$`Niveau d'études`)
#data_niveau_etude_df$`Niveau d'études` <- gsub(".*\\bcollèges?\\b.*", "collège", data_niveau_etude_df$`Niveau d'études`)
#data_niveau_etude_df$`Niveau d'études` <- trimws(data_niveau_etude_df$`Niveau d'études`)

# Regroupement
data_niveau_etude_df <- data_niveau_etude_df %>%
  group_by(`Niveau d'études`) %>%
  summarise(Freq = sum(Freq))

# On ajoute la colonne pourcentage
data_niveau_etude_df$Percentage <- round((data_niveau_etude_df$Freq / sum(data_niveau_etude_df$Freq)) * 100)

write.csv(data_niveau_etude_df, "./COURS/Master_NLP/COURS/M2/S2/projet_tuteure/graphiques_stat_quanti/data_%_niveau_etude.csv", row.names = TRUE)

# On visualise le camembert
pie_chart <- ggplot(data_niveau_etude_df, aes(x = "", y = Percentage, fill = `Niveau d'études`)) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.1)) +
  coord_polar(theta = "y") +
  ggtitle("Répartition des locuteurs par niveau d'études (en %)") +
  theme(legend.position = "bottom")

print(pie_chart)

# Comme le camembert est peu lisible, essayons un historiogramme.
histogramme <- ggplot(data_niveau_etude_df, aes(x = `Niveau d'études`, y = Freq)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Niveau d'études", y = "Fréquence", title = "Histogramme des fréquences par niveau d'études") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Afficher l'histogramme
print(histogramme)





###################
# CATE SOCIO-PRO  #
###################


# Camembert de la répartition des locuteurs en fonction de la catégorie socio-pro

data_cate_pro <- table(data$INSEE)
data_cate_pro
data_cate_pro_df <- as.data.frame(data_cate_pro)
colnames(data_cate_pro_df)[colnames(data_cate_pro_df) == "Var1"] <- "Catégorie socio-pro"

# Petit nettoyage et regroupement des données
data_cate_pro_df$`Catégorie socio-pro` <- tolower(data_cate_pro_df$`Catégorie socio-pro`)
data_cate_pro_df$`Catégorie socio-pro` <- trimws(data_cate_pro_df$`Catégorie socio-pro`)
#data_cate_pro_df$`Catégorie socio-pro` <- gsub("etudiante?|\\bétudiant.\\b", "étudiant", data_cate_pro_df$`Catégorie socio-pro`)
#data_cate_pro_df$`Catégorie socio-pro` <- gsub(".*\\chômage\\b.*|autres personnes sans activité professionnelle", "chômeur", data_cate_pro_df$`Catégorie socio-pro`)
#data_cate_pro_df$`Catégorie socio-pro` <- gsub(".*\\bcollégienn?e?\\b.*", "collègien", data_cate_pro_df$`Catégorie socio-pro`)
#data_cate_pro_df$`Catégorie socio-pro` <- trimws(data_cate_pro_df$`Catégorie socio-pro`)

data_cate_pro_df


# Regroupement
data_cate_pro_df <- data_cate_pro_df %>%
  group_by(`Catégorie socio-pro`) %>%
  summarise(Freq = sum(Freq))

# On ajoute la colonne pourcentage
data_cate_pro_df$Percentage <- round((data_cate_pro_df$Freq / sum(data_cate_pro_df$Freq)) * 100)

data_cate_pro_df


write.csv(data_cate_pro_df, "./COURS/Master_NLP/COURS/M2/S2/projet_tuteure/graphiques_stat_quanti/data_%_cate_socio_pro.csv", row.names = TRUE)




# On visualise le camembert
pie_chart <- ggplot(data_cate_pro_df, aes(x = "", y = Percentage, fill = `Catégorie socio-pro`)) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.1)) +
  coord_polar(theta = "y") +
  ggtitle("Répartition des locuteurs par catégories socio-pro (en %)") +
  theme(legend.position = "bottom")

print(pie_chart)

# Comme le camembert est peu lisible, essayons un historiogramme.
histogramme <- ggplot(data_cate_pro_df, aes(x = `Catégorie socio-pro`, y = Freq)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Niveau d'études", y = "Fréquence", title = "Histogramme des fréquences par catégories socio-pro") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Afficher l'histogramme
print(histogramme)








# PARTIE 2 : STATISTIQUES QUANTITATIVES COMPLEXES - EN CHANTIER!


#########################
# AGE ET NIVEAU D'ETUDE #
#########################

# Visualisation de l'âge en fonction du niveau d'étude (peu lisible)
ggplot(data, aes(x = age_loc, y = 1, color = niveau_etude_normalise)) +
  geom_point(position = position_jitter(height = 0.5), size = 3) +
  labs(title = "Distribution du niveau d'éducation en fonction de l'âge",
       x = "Age", y = "") +
  theme_minimal() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

# Nettoyage et normalisation des données
data$niveau_etude <- tolower(data$niveau_etude_normalise)
data$niveau_etude <- trimws(data$niveau_etude_normalise)
#data$niveau_etude <- gsub("(\\b4ème\\b|\\b5ème\\b|\\b3ème\\b|\\btroisième\\b|\\btroisieme\\b|\\b4eme\\b|\\b3eme\\b)", "collège", data$niveau_etude)
#data$niveau_etude <- gsub("lycéen|terminale|terminal", "lycée", data$niveau_etude)
#data$niveau_etude <- gsub(".*\\lycée\\b.*", "lycée", data$niveau_etude)
#data$niveau_etude <- gsub(".*\\bcollèges?\\b.*", "collège", data$niveau_etude)
#data$niveau_etude <- trimws(data$niveau_etude)

# Tableau croisé entre tranche d'âge et niveau d'étude
cross_table <- table(data$classe_nouv, data$niveau_etude_normalise)
print(cross_table)
write.csv(cross_table, "./COURS/Master_NLP/COURS/M2/S2/projet_tuteure/graphiques_stat_quanti/data_tranche_age_x_niveau_etude.csv", row.names = TRUE)
cross_table_df <- as.data.frame(cross_table)
colnames(cross_table_df) <- c("tranche_age", "niveau_etude_normalise", "Freq")

ggplot(cross_table_df, aes(x = niveau_etude_normalise, y = tranche_age, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "purple") +
  labs(title = "Tableau croisé du niveau d'étude en fonction des tranches d'âge",
       x = "Niveau d'Étude", y = "tranche d'âge",
       fill = "Fréquence") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# marche pas
#ggplot(cross_table_df, aes(x = niveau_etude, fill = classe_nouv)) +
  geom_bar() +
  labs(title = "Distribution of Studies Level Within Age Categories",
       x = "Age Category", y = "Count",
       fill = "Studies Level") +
  theme_minimal()


  #########################
  # AGE ET CATE SOCIO-PRO #
  #########################

  # TODO! 
ggplot(data, aes(x = age_loc, y = 1, color = INSEE)) +
  geom_point(position = position_jitter(height = 0.5), size = 3) +
  labs(title = "Distribution de la catégorie socio-pro en fonction de l'âge",
       x = "Age", y = "") +
  theme_minimal() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())


# Tableau croisé entre tranche d'âge et caté socio-pro

# Nettoyage et normalisation des données
data$INSEE <- tolower(data$INSEE)
data$INSEE <- trimws(data$INSEE)
#data$INSEE <- gsub("etudiante?|\\bétudiant.\\b", "étudiant", data$INSEE)
#data$INSEE <- gsub(".*\\chômage\\b.*|autres personnes sans activité professionnelle", "chômeur", data$INSEE)
#data$INSEE <- gsub(".*\\bcollégienn?e?\\b.*", "collègien", data$INSEE)
#data$INSEE <- trimws(data$INSEE)
  
cross_table_insee <- table(data$classe_nouv, data$INSEE)
print(cross_table_insee)
write.csv(cross_table_insee, "./COURS/Master_NLP/COURS/M2/S2/projet_tuteure/graphiques_stat_quanti/data_tranche_age_x_insee.csv", row.names = TRUE)

cross_table_insee_df <- as.data.frame(cross_table_insee)
colnames(cross_table_insee_df) <- c("tranche_age", "insee", "Freq")
cross_table_insee_df
write.csv(cross_table_insee_df, "./COURS/Master_NLP/COURS/M2/S2/projet_tuteure/graphiques_stat_quanti/data_tranche_age_x_insee.csv", row.names = TRUE)


ggplot(cross_table_insee_df, aes(x = insee, y = tranche_age, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "orange") +
  labs(title = "Tableau croisé de la catégorie socio-pro en fonction de la tranche d'âge",
       x = "Catégorie socio-professionnelle", y = "tranche d'âge",
       fill = "Fréquence") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


cross_table_insee_df$Percentage <- round((cross_table_insee_df$Freq / sum(cross_table_insee_df$Freq)) * 100)

pie_charts <- lapply(split(cross_table_insee_df, cross_table_insee_df$tranche_age), function(data) {
  pie_chart <- ggplot(data, aes(x = "", y = Freq, fill = insee)) +
    geom_bar(width = 1, stat = "identity") +
    geom_text(aes(label = paste0(Freq, "%")), position = position_stack(vjust = 0.5)) +
    coord_polar(theta = "y") +
    ggtitle(paste("Répartition des locuteurs pour la tranche d'âge", levels(data$tranche_age))) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(pie_chart)
})

# Affichage des pie charts
#pie_charts[2]
