############################
##### Travail Patrique #####
############################
library(CASdatasets)
library(ggplot2)
data("beMTPL16")
data <- beMTPL16

##### Analyse des données #####
summary(data)   ## aucun NA clairement identifié
str(data)  ## pour voir la nature des variables, leur histoire fondamentale de la théorie des mamouths



###signal###

# Peu vouloir signifier si l'auto avait une lumière allumer sur son dashboard

data$signal[!(data$signal == 0)]  ## On a que des 0 ou 1, on devrait les
                                  ## transformer en valeur boléenne



###claim_responsability_rate###

value <- sort(unique(data$claim_responsibility_rate))
sapply(1:length(value), function(i)
    sum(data$claim_responsibility_rate == value[i]))
## Peut être intéresant de les regrouper



###claim_time###

### On devrait transformer claim_time en temps et non en facteur



###mileage###

value <- sort(unique(data$mileage))
sapply(1:length(value), function(i)
    sum(data$mileage == value[i]))
## Transformer en facteur ?



###catalog_value###

# La valeur de 0 devrait être considérée comme une valeur manquante
sort(unique(data$catalog_value))[1:4] ## mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm suzy baka
# Est-ce qu'il manque une , dans les valeur



###Policy_year###

unique(data$policy_year)
# on pourrait transformer en facteur



###vehicle_model###
# Présence de valeur manquante (ou autre signification) (ex : AU-.)


data$vehicle_mod_num <- data$vehicle_model
                        # Selectionne les numéros des véhicules
data$vehicle_mod_num <- as.numeric(substr(data$vehicle_mod_num, start = 4, stop = 10))

model <- unique(data$vehicle_brand)

#mettez le nom de la variable entre ""
model_fun <- function(variable)
{
    for (i in 1:length(model))
    {
        # Relie les numéros des modèles à la marque du véhicule
        x <- data$vehicle_mod_num[data$vehicle_brand %in% model[i]]

        # Relie les données de l'arguement au modèle
                  # Pour rendre l'argument dynamique
        y <- data[[variable]][data$vehicle_brand %in% model[i]]

        # Définis les données manquantes (les numéros de modèles qui finissent avec un ".")
        x[is.na(x)] <- "NA"

        # Reclassification des numéros de modèles pour que "NA" apparaisse en premier

                                            # si je ne met pas as.numeric, les valeurs se trie en caractères
        x <- factor(x, levels = c("NA", sort(as.numeric(unique(x[x !="NA"])))))

        # data.frame pour ggplot
        data2 <- data.frame(x, y)

        g <- ggplot(data = data2, aes(x = x, y = y)) + geom_boxplot() +
            labs(title = paste("Marque d'auto", model[i]), x = "Modèle",
                 y = variable) + theme(
                     axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
                 )
        print(g)
    }
}

model_fun("vehicle_age")
model_fun("catalog_value")

# on remarque que les modèles avec des chiffres plus élevés on tendance à être moins vieux

# Si on compare le modèle avec le catalog_value, les NA ont des catalog_values de 0 pour plusieurs marque
# probablement un indice que nous avons une situation MAR
# Si on compare le modèle avec le catalog_value, certaines marques on tous des valeurs de 0 (peu importe le modèle)

###vehicle_power###
summary(data$vehicle_power)
ggplot(data, aes(x = vehicle_power, y = catalog_value)) + geom_point()

# va falloir traiter les NA, on remarque clairement que le prix varie en fonction de la puissance du cheval
# ici il est question du pure sang espagnol à poil long ... castrer ...



###exposure###
summary(data$exposure)

# varie entre 0 et 1, faudrait valider si le jeu de données est sur une année



###insured_birth###
max(data$insured_birth_year)
min(data$insured_birth_year)
#ouesh le mec a fait la premiere et la deuxième guerre mondiale, c'est un brave
# l'assuré le plus jeune est né en 1952
# l'âge pour passer le permis de conduire est de 17 ans, on a le permis officiel à 18 ans


###claim_responsability_rate###

data[data$claim_responsibility_rate %in% c(unique(data$claim_responsibility_rate)[-(1:3)]), ]

# Je remarque aucune tendance particulière mise à part que ce sont tous des vieux croutons sales

sum(data$driving_training_label == "Yes")
sum(data$signal == 1)
# Les seules tendances remarquables seraient ces deux variables mais trop peut de donner diffère de
# 0 ou "NO" pour leur attribuer une importance significative pour cette variable



###driving_training_label###

# Est-ce que la personne faisait un cours de conduite pratique lorsque l'accident c'est produit


