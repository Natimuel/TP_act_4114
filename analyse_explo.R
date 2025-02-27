############################
##### Travail Patrique #####
############################

library(CASdatasets)
library(ggplot2)
library(dplyr)
library(nnet)
library(FactoMineR)
library(factoextra)
library(chron)
library(mice)
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
sort(unique(data$catalog_value))[1:4]
sum(data$catalog_value == 0)
sum(data$catalog_value == 4)
sum(data$catalog_value == 40)
# On remarque qu'il y a beaucoup de 0 peu de 4 et 40, peut-être enlever les 4 et 40
# 0 est une valeur manquante


###Policy_year###

unique(data$policy_year)
# on pourrait transformer en facteur



###vehicle_model###
# Présence de valeur manquante (ou autre signification) (ex : AU-.)


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


sapply(2:19, function(i) unique(data[, i]))

# il ne semble pas y avoir d'autres données manquantes mise à part le modèle et catalog_value



##### Transformation des variables #####

# Claim_time

data$claim_time <- times(paste0(as.character(data$claim_time), ":00"))

# signal
data$signal <- as.factor(data$signal)

# policy_year
data$policy_year <- as.factor(data$policy_year)

# number_of_liability_claim
data$number_of_liability_claims <- as.factor(data$number_of_liability_claims)

# number_of_bodily_injury_liability_claims
data$number_of_bodily_injury_liability_claims <- as.factor(data$number_of_bodily_injury_liability_claims)

# insured_birth
data$insured_birth_year <- 1970 - data$insured_birth_year

# vehicle_model

# pour la fonction model_fun
vehicle_model_num <- data$vehicle_model

data$vehicle_model <- as.factor(substr(data$vehicle_model, start = 4, stop = 10))
data$vehicle_model[data$vehicle_model == "."] <- NA

# mileage
data$mileage <- as.factor(data$mileage)

##### Traitement des valeurs manquantes #####
### vehicle_model


# Selectionne les numéros des véhicules
vehicle_model_num <- as.numeric(substr(vehicle_model_num, start = 4, stop = 10))

model <- unique(data$vehicle_brand)

#mettez le nom de la variable entre ""
model_fun <- function(variable, type = geom_boxplot)
{
    for (i in 1:length(model))
    {
        # Relie les numéros des modèles à la marque du véhicule
        x <- vehicle_model_num[data$vehicle_brand %in% model[i]]

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

        g <- ggplot(data = data2, aes(x = x, y = y)) + type() +
            labs(title = paste("Marque d'auto", model[i]), x = "Modèle",
                 y = variable) + theme(
                     axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
                 )
        print(g)
    }
}

# Exemple
model_fun("vehicle_age", geom_boxplot)
model_fun("catalog_value")

# on remarque que les modèles avec des chiffres plus élevés on tendance à être moins vieux

# Si on compare le modèle avec le catalog_value, les NA ont des catalog_values de 0 pour plusieurs marque
# probablement un indice que nous avons une situation MAR
# Si on compare le modèle avec le catalog_value, certaines marques on tous des valeurs de 0 (peu importe le modèle)


### Catalog_value
data$catalog_value[data$catalog_value == 0] <- NA

### Type de données manquantes

# Cette fonction vise à effectuer des tests d'hypothèses variant en fonction de la nature de
# la variable qui sera tester (chisq ou student)

typeNA <- function(variableNA)
{

    variable_test <- data %>% select(c(-variableNA, -insurance_contract))

    variable_test_num <- variable_test[sapply(variable_test, class) %in% c("numeric", "integer", "times")]

    variable_test_categ <- variable_test[sapply(variable_test, class) %in% "factor"]


    if (class(data[[variableNA]]) %in% c("numeric", "integer", "times"))
    {
        x <- numeric(length(variable_test_num[, ]))

        for(i in 1:length(variable_test_num[, ]))
        {
            x[i] <- t.test(x = variable_test_num[, i][is.na(data[[variableNA]])],
                           y = variable_test_num[, i][!is.na(data[[variableNA]])])$p.value
        }

    }


    else
    {
        x <- numeric(length(variable_test_categ[, ]))

        for(i in variable_test_num[, ]:(length(variable_test_categ[, ]) +  variable_test_num[, ]))
        {
            x[i] <- chisq.test(x = is.na(data[[variableNA]]),
                                   y = variable_test_categ[, i], correct = F)$p.value
        }

    }
    x
}

typeNA("vehicle_model") <= 0.05
typeNA("catalog_value") <= 0.05

?aggregate
md.pattern(data, plot = T)

#### Imputation stochastique par une régression


# catalog_value
data$catalog_value[data$catalog_value == 0] <- NA
data$vehicle_brand <- as.factor(data$vehicle_brand)
data


data_for_catalog_pred <- data.frame(log(data$catalog_value), data[, 8])
str(data_for_catalog_pred)
summary(data_for_catalog_pred)
                                                      # soit on utilise norm ou pmm  et polyreg ou polr
imput <- mice(data_for_catalog_pred, m=1,  method = c("norm", "polyreg"))
don.compl <- complete(imput)
data$catalog_value <- exp(don.compl[, 1])
mean(exp(don.compl[, 1]))
sd(exp(don.compl[, 1]))

# my_glm <- glm(catalog_value~vehicle_power, data, family = Gamma(link = "log"))
# n <- sum(is.na(data$catalog_value))
#
# previsions <- predict(my_glm, newdata=data[is.na(data$catalog_value),],type="response")
#
# shape <- 1 / summary(my_glm)$dispersion
#
# data$catalog_value[is.na(data$catalog_value)] <- rgamma(length(previsions), shape = shape, scale = mean(previsions) / shape)
#
# mean(data$catalog_value)

mean(data$catalog_value[!is.na(data$catalog_value)])
# [1] 842291.5
sd(data$catalog_value[!is.na(data$catalog_value)])

# vehicle_model
data_for_model_pred <- data.frame(data$vehicle_model, data[, c(8, 11)])
str(data_for_model_pred)
summary(data_for_model_pred)

# peut-être le faire pour chaque marque de véhicule, un à la fois
imput <- mice(data_for_model_pred, m=1,  method = c("polyreg", "polyreg", "norm"))
don.compl <- complete(imput)
mean(exp(don.compl[, 1]))
sd(exp(don.compl[, 1]))


vehicle_model_num_pred <- as.factor(vehicle_model_num)

model <- unique(data$vehicle_brand)

for (i in 1:length(model))
{
    # Relie les numéros des modèles à la marque du véhicule
    x <- vehicle_model_num_pred[data$vehicle_brand %in% model[i]]

    # Relie les données de l'arguement au modèle
    # Pour rendre l'argument dynamique
    y <- data$vehicle_brand[data$vehicle_brand %in% model[i]]

    z <- data$vehicle_power[data$vehicle_brand %in% model[i]]

    data_for_model_pred <- data.frame(x, y, z)

    imput <- mice(data_for_model_pred, m=1,  method = c("cart", "polyreg", "norm"))
    don.compl <- complete(imput)
}
str(data)
summary(data[, c(-1, -2, -8, -9, -10, -14, -15, -18, -19)])

## Analyse en composante principale

















# On doit retirer toutes les composantes non numérique
acp <- PCA(data[, c(-1, -2, -8, -9, -10, -14, -15, -18, -19)], scale = T)
fviz_screeplot(acp)
acp$eig
acp <- PCA(data[, -c(1, 2, 8, 9, 10, 14, 15, 18, 19)])
plot(acp$eig[, 3], type = "b")
abline(h = 80, col = "red")
acp$eig[, 3]





data1 <- data[, c(-1, -2, -8, -9, -10, -12, -14, -15, -18, -19)]
library(cluster)


kmoy <- kmeans(data1, centers = 7, nstart= 10)
table(kmoy$cluster)
data1_sim <- data1[sample(1:nrow(data), 20000), ]
fviz_nbclust(scale(data1_sim), FUNcluster = kmeans, nstart = 3, method = "wss", k.max = 25)
data2 <- data.frame(x= acp$ind$coord[, 1], y = acp$ind$coord[, 2], groupe = )
