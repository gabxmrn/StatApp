source("code/utils/import.R")
source("code/utils/data_treatment.R")
source("code/utils/graphs.R")
source("code/utils/test_correlation.R")
source("code/utils/stationarity.R")

###### Importation des données ######
data_fr <- excel_import("code/data.xlsx", "France")
data_us <- excel_import("code/data.xlsx", "US")

###### Données - Richesse ######

date_debut <- "1998-12-01"
date_fin <- "2023-03-01"

# richesse_fr <- df_richesse(data_fr, date_debut, date_fin, 1)
# png("richesse_fr.png", width = 500, height = 600)
# plot_richesse(richesse_fr, "France")
# dev.off()

richesse_us <- df_richesse(data_us, date_debut, date_fin, 1)
plot_richesse(richesse_us, "US")
head(richesse_us)

###### Données - Variable explicative ######

# consommation_fr <- df_consommation(data_fr, date_debut, date_fin)
# png("conso_fr.png", width = 800, height = 400)
# plot_consommation(consommation_fr, "France")
# dev.off()
# cor_conso(consommation_fr, "France")

consommation_us <- df_consommation(data_us, date_debut, date_fin, TRUE)
plot_consommation(consommation_us, "US")
cor_conso(consommation_us, "US")

###### Données - Variables de contrôles ######

# var_control_fr <- df_control(data_fr, date_debut, date_fin)
# png("varcontrol_fr.png", width = 800, height = 600)
# visualisation_controles(var_control_fr, "France")
# dev.off()

# var_control_us <- df_control(data_us, date_debut, date_fin)
# png("varcontrol_us.png", width = 800, height = 600)
# visualisation_controles(var_control_us, "US")
# dev.off()

###### Tests de stationnarité ######

# stationarite(richesse_fr)
# stationarite(consommation_fr)
# stationarite(var_control_fr)

<<<<<<< HEAD
#stationarite(richesse_us)
#stationarite(consommation_us)
=======
# stationarite(richesse_us)
# stationarite(consommation_us)
# stationarite(var_control_us)
>>>>>>> 2433b0628a762e8468e25bdc3d0143d43703b88c
