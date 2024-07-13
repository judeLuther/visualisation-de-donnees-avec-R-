#Vider la mémoire
rm(list = ls())

# Charger le package ggplot2
library(ggplot2)

library(dplyr)


################################################
#### Visualisation de la structure de cours ####
################################################

#Lire le fichier courses.csv contenant les cours
courses <- read.csv(file.choose(), header = TRUE, stringsAsFactors = TRUE, sep = ',')
View(courses)

#Les présentations des cours commencent en février et octobre. 
#Elles sont marquées respectivement par «B» et «J».

# Ordonner le data frame par code_presentation
courses <- arrange(courses, code_presentation)

# Charger le package RColorBrewer
library(RColorBrewer)

# Sélectionner une couleur proche du rouge à partir de la palette "Reds"
red_palette <- brewer.pal(9, "YlOrRd")[9]

# Créer l'histogramme
histogram <- ggplot(courses, aes(x = paste(code_presentation, code_module), y = module_presentation_length)) +
  geom_bar(stat = "identity", fill = red_palette, color = "black") + # Définir les barres et les couleurs
  labs(x = "Presentation and Code Module", y = "Number of days of courses", title = "Structure de cours") + # Nommer les axes et le titre
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Ajuster l'angle du texte sur l'axe x pour une meilleure lisibilité

# Afficher l'histogramme
histogram


################################################################
#### Visualisation des inscriptions studentRegistration.csv ####
################################################################

#Lire le fichier contenu dans ``studentRegistration.csv''
studentRegistration <- read.csv(file.choose(), header = TRUE, stringsAsFactors = TRUE, sep = ',')
View(studentRegistration)

# Agréger les données pour obtenir le nombre d'étudiants inscrits par présentation et par module de code
data_aggregated <- aggregate(id_student ~ code_module + code_presentation, data = studentRegistration, FUN = length)

# Renommer la colonne agrégée
names(data_aggregated)[3] <- "count"

# Créer le graphique avec ggplot2
ggplot(data_aggregated, aes(x = code_presentation, y = count, fill = code_module)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5) +  # Ajouter des étiquettes de texte
  labs(title = "Nombre d'étudiants inscrits par présentation et par module de code",
       x = "Présentation",
       y = "Nombre d'étudiants inscrits",
       fill = "Module de Code")

####  Afficher pourcentage d'étudiants ayant abandonné le cours avant la fin

# Calculer le nombre d'abandons et le nombre total d'inscriptions par module et par présentation
df <- studentRegistration %>%
  mutate(abandon = !is.na(date_unregistration)) %>%
  group_by(code_module, code_presentation) %>%
  summarise(nb_abandons = sum(abandon), total_inscriptions = n(), .groups = 'drop') %>%
  mutate(perc_abandon = nb_abandons / total_inscriptions)  # Ratio de 0 à 1

#Graphique
ggplot(df, aes(x = code_module, y = perc_abandon, fill = code_presentation)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = sprintf("%.1f%%", 100 * perc_abandon),  # Convertir le ratio en pourcentage pour l'affichage
                y = perc_abandon + 0.05),  # Ajuster la position du texte au-dessus de la barre
            position = position_dodge(width = 0.9), size = 3, vjust = 0) +
  scale_y_continuous(labels = scales::percent) + # Convertir les valeurs de l'axe Y en pourcentage pour l'affichage
  theme_minimal() +
  labs(title = "Pourcentage d'abandon par module et présentation",
       x = "Module",
       y = "Pourcentage d'abandon",
       fill = "Présentation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#######################################################
#### Visualisation de la structure des evaluations ####
#######################################################

#Lire le fichier contenu dans ``assessments.csv``
assessments <- read.csv(file.choose(), header = TRUE, stringsAsFactors = TRUE, sep = ',')
View(assessments)

# Pour représenter une variable quantitative (weight) en fonction d'une variable qualitative (assessment_type),
plot(weight~assessment_type, data=assessments, main="Boxplots",
     xlab="Type d'evaluation", ylab="Poids de l'evaluation") # la fonction plot() dans ce cas retourne une boîte
# à moustaches par modalité de la variable (assessment_type)
abline(h = mean(assessments$weight), lty=3) # on représente la moyenne de la variable (weight) par une ligne horizontale

# Création du graphique
#Le graphique que j'ai proposé est un graphique en nuage de points (scatter plot) 
#avec la fonction geom_point() de ggplot2. Ce type de graphique est utilisé pour visualiser 
#la relation entre deux variables continues (dans ce cas, la date et le poids de l'évaluation), 
#avec chaque point représentant une observation dans les données. 
#En utilisant la couleur pour représenter une troisième variable (dans ce cas, le type d'évaluation), 
#nous pouvons également ajouter une dimension supplémentaire à la visualisation des données.

#Poids des évaluations par module et présentation
ggplot(assessments, aes(x = date, y = weight, color = assessment_type)) +
  geom_point() +
  facet_wrap(~ code_module + code_presentation, nrow = 2) +
  labs(title = "Poids des évaluations par module et présentation",
       x = "Date",
       y = "Poids",
       color = "Type d'évaluation") +
  theme_minimal()

################################################################
#### Visualisation des informations des etudiants studentInfo.csv#############
################################################################
#Lire le fichier contenu dans ``studentInfo.csv``
studentInfo <- read.csv(file.choose(), header = TRUE, stringsAsFactors = TRUE, sep = ',')
View(studentInfo)

#### Répartition finale des résultats par tranches d'âge
ggplot(studentInfo, aes(x = final_result, fill = final_result)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), position=position_stack(vjust=0.5)) +
  facet_wrap(~age_band, scales = "free") +
  labs(title = "Répartition finale des résultats par tranches d'âge",
       x = "Résultat final",
       y = "Nombre d'étudiants",
       fill = "Résultat final") +
  scale_fill_manual(values = c("Fail" = "red", "Pass" = "green", "Distinction" = "blue", "Withdrawn" = "gray"))

### Visualisation des resultat par genre
ggplot(studentInfo, aes(x = final_result, fill = gender)) +
  geom_bar(position = "dodge", alpha = 0.8) +
  labs(x = "Résultat final", y = "Nombre d'étudiants", title = "Résultats par genre") +
  theme_minimal()

### Visualisation des résultats par niveau d'éducation le plus élevé
ggplot(studentInfo, aes(x = final_result, fill = highest_education)) +
  geom_bar(position = "dodge", alpha = 0.8) +
  labs(x = "Résultat final", y = "Nombre d'étudiants", title = "Résultats par niveau d'éducation le plus élevé") +
  theme_minimal()

##############################################################################################
#### Visualisation de la structure des interractions etudiants avec la plateforme en ligne####
##############################################################################################

##########################################################################################################
####### Analyse et visualisation multivariée des interactions et des performances des apprenants #########
##########################################################################################################

#Lire le fichier ``studentVle.csv`` contenant les interractions avec la plateforme en ligne
studentVle <- read.csv(file.choose(), header = TRUE, stringsAsFactors = TRUE, sep = ',')
View(studentVle)

####################################################################
## Visualisation interactions des etudiants pour le semestre 2013B ##
####################################################################
####################################################################
# Filtrer les étudiants ayant réussi ou obtenu une distinction dans tous les modules du semestre 2013B
passing_students <- studentInfo %>%
  filter(code_presentation == '2013B', final_result %in% c('Pass', 'Distinction')) %>%
  group_by(id_student) %>%
  filter(all(final_result %in% c('Pass', 'Distinction'))) %>%
  pull(id_student)

# Filtrer les données d'interaction pour inclure uniquement ces étudiants
filtered_data <- studentVle %>%
  filter(code_presentation == '2013B', id_student %in% passing_students)

# Calculer le nombre total d'étudiants inscrits à chaque module
total_students_per_module <- filtered_data %>%
  group_by(code_module, code_presentation) %>%
  summarise(total_students = n_distinct(id_student))

# Calculer le nombre d'étudiants ayant interagi avec le module du cours pour chaque module et chaque date
students_interacted_per_module <- filtered_data %>%
  filter(sum_click > 0) %>%
  group_by(code_module, code_presentation, date) %>%
  summarise(num_interacted = n_distinct(id_student))

# Fusionner les deux dataframes pour obtenir le pourcentage d'interactions pour chaque date et chaque module
interaction_percentage <- left_join(students_interacted_per_module, total_students_per_module, 
                                    by = c("code_module", "code_presentation")) %>%
  mutate(percentage_interacted = num_interacted / total_students)

# Calculer le nombre d'étudiants filtrés
num_filtered_students <- length(unique(filtered_data$id_student))

# Tracer le nuage de points avec les modules de cette présentation comme couleurs différentes
ggplot(interaction_percentage, aes(x = date, y = percentage_interacted, color = code_module)) +
  geom_point() +
  labs(x = "Date",
       y = "Pourcentage d'étudiants interagissant avec le module",
       title = paste("Réussite (Pass ou Distinction) Semestre 2013B (", num_filtered_students, "étudiants)")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Centrer le titre


####################################################################
# Filtrer les étudiants ayant échoués dans tous les modules de 2013B
failing_students <- studentInfo %>%
  filter(code_presentation == '2013B', final_result %in% c('Fail')) %>%
  group_by(id_student) %>%
  filter(all(final_result %in% c('Fail'))) %>%
  pull(id_student)

# Filtrer les données d'interaction pour inclure uniquement ces étudiants
filtered_data <- studentVle %>%
  filter(code_presentation == '2013B', id_student %in% failing_students)

# Calculer le nombre total d'étudiants inscrits à chaque module
total_students_per_module <- filtered_data %>%
  group_by(code_module, code_presentation) %>%
  summarise(total_students = n_distinct(id_student))

# Calculer le nombre d'étudiants ayant interagi avec le module du cours pour chaque module et chaque date
students_interacted_per_module <- filtered_data %>%
  filter(sum_click > 0) %>%
  group_by(code_module, code_presentation, date) %>%
  summarise(num_interacted = n_distinct(id_student))

# Fusionner les deux dataframes pour obtenir le pourcentage d'interactions pour chaque date et chaque module
interaction_percentage <- left_join(students_interacted_per_module, total_students_per_module, 
                                    by = c("code_module", "code_presentation")) %>%
  mutate(percentage_interacted = num_interacted / total_students)

# Calculer le nombre d'étudiants filtrés
num_filtered_students <- length(unique(filtered_data$id_student))

# Tracer le nuage de points avec les modules de cette présentation comme couleurs différentes
ggplot(interaction_percentage, aes(x = date, y = percentage_interacted, color = code_module)) +
  geom_point() +
  labs(x = "Date",
       y = "Pourcentage d'étudiants interagissant avec le module",
       title = paste(" Echec(Fail) Semestre 2013B (", num_filtered_students, "étudiants)")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Centrer le titre


####################################################################
# Filtrer les étudiants ayant abondonés dans tous les modules de semestre 2013B
withdrawing_students <- studentInfo %>%
  filter(code_presentation == '2013B', final_result %in% c('Fail')) %>%
  group_by(id_student) %>%
  filter(all(final_result %in% c('Withdrawn'))) %>%
  pull(id_student)

# Filtrer les données d'interaction pour inclure uniquement ces étudiants
filtered_data <- studentVle %>%
  filter(code_presentation == '2013B', id_student %in% failing_students)

# Calculer le nombre total d'étudiants inscrits à chaque module
total_students_per_module <- filtered_data %>%
  group_by(code_module, code_presentation) %>%
  summarise(total_students = n_distinct(id_student))

# Calculer le nombre d'étudiants ayant interagi avec le module du cours pour chaque module et chaque date
students_interacted_per_module <- filtered_data %>%
  filter(sum_click > 0) %>%
  group_by(code_module, code_presentation, date) %>%
  summarise(num_interacted = n_distinct(id_student))

# Fusionner les deux dataframes pour obtenir le pourcentage d'interactions pour chaque date et chaque module
interaction_percentage <- left_join(students_interacted_per_module, total_students_per_module, 
                                    by = c("code_module", "code_presentation")) %>%
  mutate(percentage_interacted = num_interacted / total_students)

# Calculer le nombre d'étudiants filtrés
num_filtered_students <- length(unique(filtered_data$id_student))

# Tracer le nuage de points avec les modules de cette présentation comme couleurs différentes
ggplot(interaction_percentage, aes(x = date, y = percentage_interacted, color = code_module)) +
  geom_point() +
  labs(x = "Date",
       y = "Pourcentage d'étudiants interagissant avec le module",
       title = paste(" Abondan(Withdrawn) Semestre 2013B (", num_filtered_students, "étudiants)")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Centrer le titre

####################################################################
## Visualisation interactions des etudiants pour le semestre 2014J ##
####################################################################
# Filtrer les étudiants ayant réussi ou obtenu une distinction dans tous les modules de 2014J
passing_students <- studentInfo %>%
  filter(code_presentation == '2014J', final_result %in% c('Pass', 'Distinction')) %>%
  group_by(id_student) %>%
  filter(all(final_result %in% c('Pass', 'Distinction'))) %>%
  pull(id_student)

# Filtrer les données d'interaction pour inclure uniquement ces étudiants
filtered_data <- studentVle %>%
  filter(code_presentation == '2014J', id_student %in% passing_students)

# Calculer le nombre total d'étudiants inscrits à chaque module
total_students_per_module <- filtered_data %>%
  group_by(code_module, code_presentation) %>%
  summarise(total_students = n_distinct(id_student))

# Calculer le nombre d'étudiants ayant interagi avec le module du cours pour chaque module et chaque date
students_interacted_per_module <- filtered_data %>%
  filter(sum_click > 0) %>%
  group_by(code_module, code_presentation, date) %>%
  summarise(num_interacted = n_distinct(id_student))

# Fusionner les deux dataframes pour obtenir le pourcentage d'interactions pour chaque date et chaque module
interaction_percentage <- left_join(students_interacted_per_module, total_students_per_module, 
                                    by = c("code_module", "code_presentation")) %>%
  mutate(percentage_interacted = num_interacted / total_students)

# Calculer le nombre d'étudiants filtrés
num_filtered_students <- length(unique(filtered_data$id_student))

# Tracer le nuage de points avec les modules de cette présentation comme couleurs différentes
ggplot(interaction_percentage, aes(x = date, y = percentage_interacted, color = code_module)) +
  geom_point() +
  labs(x = "Date",
       y = "Pourcentage d'étudiants interagissant avec le module",
       title = paste("Réussite (Pass ou Distinction) Semestre 2014J (", num_filtered_students, "étudiants)")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Centrer le titre


####################################################################
# Filtrer les étudiants ayant échoués dans tous les modules de semestre 2014J
failing_students <- studentInfo %>%
  filter(code_presentation == '2014J', final_result %in% c('Fail')) %>%
  group_by(id_student) %>%
  filter(all(final_result %in% c('Fail'))) %>%
  pull(id_student)

# Filtrer les données d'interaction pour inclure uniquement ces étudiants
filtered_data <- studentVle %>%
  filter(code_presentation == '2014J', id_student %in% failing_students)

# Calculer le nombre total d'étudiants inscrits à chaque module
total_students_per_module <- filtered_data %>%
  group_by(code_module, code_presentation) %>%
  summarise(total_students = n_distinct(id_student))

# Calculer le nombre d'étudiants ayant interagi avec le module du cours pour chaque module et chaque date
students_interacted_per_module <- filtered_data %>%
  filter(sum_click > 0) %>%
  group_by(code_module, code_presentation, date) %>%
  summarise(num_interacted = n_distinct(id_student))

# Fusionner les deux dataframes pour obtenir le pourcentage d'interactions pour chaque date et chaque module
interaction_percentage <- left_join(students_interacted_per_module, total_students_per_module, 
                                    by = c("code_module", "code_presentation")) %>%
  mutate(percentage_interacted = num_interacted / total_students)

# Calculer le nombre d'étudiants filtrés
num_filtered_students <- length(unique(filtered_data$id_student))

# Tracer le nuage de points avec les modules de cette présentation comme couleurs différentes
ggplot(interaction_percentage, aes(x = date, y = percentage_interacted, color = code_module)) +
  geom_point() +
  labs(x = "Date",
       y = "Pourcentage d'étudiants interagissant avec le module",
       title = paste(" Echec(Fail) Semestre 2014J (", num_filtered_students, "étudiants)")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Centrer le titre


####################################################################
# Filtrer les étudiants ayant abondonés dans tous les modules de semestre 2014J
withdrawing_students <- studentInfo %>%
  filter(code_presentation == '2014J', final_result %in% c('Fail')) %>%
  group_by(id_student) %>%
  filter(all(final_result %in% c('Withdrawn'))) %>%
  pull(id_student)

# Filtrer les données d'interaction pour inclure uniquement ces étudiants
filtered_data <- studentVle %>%
  filter(code_presentation == '2014J', id_student %in% failing_students)

# Calculer le nombre total d'étudiants inscrits à chaque module
total_students_per_module <- filtered_data %>%
  group_by(code_module, code_presentation) %>%
  summarise(total_students = n_distinct(id_student))

# Calculer le nombre d'étudiants ayant interagi avec le module du cours pour chaque module et chaque date
students_interacted_per_module <- filtered_data %>%
  filter(sum_click > 0) %>%
  group_by(code_module, code_presentation, date) %>%
  summarise(num_interacted = n_distinct(id_student))

# Fusionner les deux dataframes pour obtenir le pourcentage d'interactions pour chaque date et chaque module
interaction_percentage <- left_join(students_interacted_per_module, total_students_per_module, 
                                    by = c("code_module", "code_presentation")) %>%
  mutate(percentage_interacted = num_interacted / total_students)

# Calculer le nombre d'étudiants filtrés
num_filtered_students <- length(unique(filtered_data$id_student))

# Tracer le nuage de points avec les modules de cette présentation comme couleurs différentes
ggplot(interaction_percentage, aes(x = date, y = percentage_interacted, color = code_module)) +
  geom_point() +
  labs(x = "Date",
       y = "Pourcentage d'étudiants interagissant avec le module",
       title = paste(" Abondan(Withdrawn) Semestre 2014J (", num_filtered_students, "étudiants)")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Centrer le titre