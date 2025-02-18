source("code/utils/import.R")
source("code/utils/data_treatment.R")
source("code/utils/graphs.R")
source("code/utils/test_correlation.R")
source("code/utils/stationarity.R")

###### Importation des données ######
data_fr <- excel_import("code/data.xlsx", "France")
data_us <- excel_import("code/data.xlsx", "US")

###### Données - Richesse ######

date_debut <- "1997-12-01"
date_fin <- "2023-12-01"

# richesse_fr <- df_richesse(data_fr, date_debut, date_fin, 1)
# plot_richesse(richesse_fr, "France")

# richesse_us <- df_richesse(data_us, date_debut, date_fin, 1)
# plot_richesse(richesse_us, "US")

###### Données - Variable explicative ######

# consommation_fr <- df_consommation(data_fr, date_debut, date_fin, TRUE)
# plot_consommation(consommation_fr, "France")
#cor_conso(consommation_fr, "France")

consommation_us <- df_consommation(data_us, date_debut, date_fin, FALSE)
plot_consommation(consommation_us, "US")
cor_conso(consommation_us, "US")

###### Données - Variables de contrôles ######

#! Construction de jeux de variables de contrôle:
#! Lag t − 2 of consumption growth, income growth, unemployment rate, differenced short-term interest rate, interest rate spread and consumer sentiment
#! Rq: consommation, revenu, richesse: were deflated with consumption deflators and expressed in per capita terms
#! Series were de-seasonalized using the X-12 method where necessary.

#! Représentation graphique des variables de contrôles

###### Tests de stationnarité ######

# stationarite(richesse_fr)
# stationarite(consommation_fr)

# stationarite(richesse_us)
# stationarite(consommation_us)
