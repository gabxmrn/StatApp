source("code/utils/import.R")
source("code/utils/data_treatment.R")
source("code/utils/graphs.R")

###### Importation des données ######
data_fr <- excel_import("code/data.xlsx", "France")
#! Ajouter une base pour les US
#! Bien récupérer la population => toutes les séries sont par habitant
###### Données - Richesse ######

date_debut <- "2009-12-01"
date_fin <- "2024-06-01"

richesse_fr <- df_richesse(data_fr, date_debut, date_fin, 2) #! les séries sont en niveau, les exprimer par habitant + consumption deflators
plot_richesse(richesse_fr, "France")



#! All series were deflated with consumption deflators and expressed in per capita terms.
#! de-seasonalized using the X-12 method where necessary.
#! Variables de contrôle