source("code/utils/import.R")
source("code/utils/data_treatment.R")
source("code/utils/graphs.R")
source("code/utils/test_correlation.R")
source("code/utils/stationarity.R")
source("code/models/chi.R")
source("code/models/PMC.R")

###### Importation des données ######
data_fr <- excel_import("code/data_modif.xlsx", "France")
data_us <- excel_import("code/data_modif.xlsx", "US")

###### Saisonnalité ######

# Consommation française
ts_conso_fr <- ts(data_fr["conso"], start = c(1995, 12), frequency = 4)
data_fr["conso"] <- X13(ts_conso_fr)

# Revenus français
ts_income_fr <- ts(data_fr["revenu"], start = c(1995, 12), frequency = 4)
data_fr["revenu"] <- X13(ts_income_fr)

###### Données - Richesse ######

# date_debut <- "1997-12-01"
# date_fin <- "2023-12-01"

# richesse_fr <- df_richesse(data_fr, date_debut, date_fin, 1)
# plot_richesse(richesse_fr, "France")

# richesse_us <- df_richesse(data_us, date_debut, date_fin, 1)
# plot_richesse(richesse_us, "US")

###### Données - Variable explicative ######

# consommation_fr <- df_consommation(data_fr, date_debut, date_fin)
# plot_consommation(consommation_fr, "France")
# cor_conso(consommation_fr, "France")

# consommation_us <- df_consommation(data_us, date_debut, date_fin)
# plot_consommation(consommation_us, "US")
# cor_conso(consommation_us, "US")

###### Données - Variables de contrôles ######

# var_control_fr <- df_control(data_fr, date_debut, date_fin)
# visualisation_controles(var_control_fr, "France")

# var_control_us <- df_control(data_us, date_debut, date_fin)
# visualisation_controles(var_control_us, "US")

###### Tests de stationnarité ######

# stationarite(richesse_fr)
# stationarite(consommation_fr)
# stationarite(var_control_fr)

# stationarite(richesse_us)
# stationarite(consommation_us)


#Essai d'une régression sur la conso des années 70 à 04 aux US sans variables instrumentales

data_us_old <- excel_import("code/data_us_70_04.xlsx", "Quarterly")

library(dplyr)
data_us_old$delta_log_c <- c(NA, diff(log(data_us_old$conso)))
model <- lm(delta_log_c ~ lag(delta_log_c), data = data_us_old, na.action = na.omit)
#print(summary(model))

###### Modélisation ######

chi <- chi(data_us, 1)["chi"]
print(chi$chi)

# Pour les US
date_debut <- "1997-12-01"
date_fin <- "2023-03-01"

# Pour la France, immo 1
#date_debut <- "1998-12-01"
#date_fin <- "2023-03-01"

# Pour la France, immo 2
# date_debut <- "2009-12-01"
# date_fin <- "2024-06-01"

PMC <- PMC(data_us, 1, date_debut, date_fin, TRUE)
print(PMC)