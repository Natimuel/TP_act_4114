############################
##### Travail Patrique #####
############################
library(CASdatasets)
library(ggplot2)


data("beMTPL16")
data <- beMTPL16

##### Analyse des données #####
summary(data)   ## aucun NA
str(data)

###signal###

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



###Policy_year###

unique(data$policy_year)
# on pourrait transformer en facteur



###vehicle_model###
# Présence de valeur manquante (ou autre signification) (ex : AU-.)

data$vehicle_mod_cha <- data$vehicle_model
data$vehicle_mod_cha <- substr(as.character(data$vehicle_mod_cha), start = 0, stop = 2)
unique(data$vehicle_mod_cha)

data$vehicle_mod_num <- data$vehicle_model
data$vehicle_mod_num <- as.numeric(substr(data$vehicle_mod_num, start = 4, stop = 10))

model <- unique(data$vehicle_brand)

for (i in 1:length(model))
{
    y <- data$vehicle_mod_num[data$vehicle_brand %in% model[i]]
    x <- data$vehicle_age[data$vehicle_brand %in% model[i]]
    y[is.na(y)] <- "NA"
    y <- factor(y, levels = c("NA", sort(unique(y[y !="NA"]))))

    data2 <- data.frame(x, y)
    g <- ggplot(data = data2, aes(x = y, y = x)) + geom_boxplot() +
        labs(title = paste("Marque d'auto", model[i]), x = "Modèle",
             y = "Âge du véhicule") + theme(
                 axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
             )
    print(g)
}


# on remarque que les modèles avec des chiffres plus élevés on tendance à être moins vieux



