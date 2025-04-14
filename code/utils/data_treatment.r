richesse_immo_slacalek <- function(df) {
  # Fonction pour répliquer la richesse immobilière
  # Méthode: Slacalek
  # HW = sf * (DS * N) * HP avec DS: dwelling stocks per capita

  # Facteur d'échelle
  # sf = HW/FW * FW avec HW/FW: ratio fixe
  # FW de 2 sources différentes => 2eme inaccessible
  # Remplacement par HW 01/01/2010 (RHP en base 100 = 2010)
  sf <- df["2010-01-01", "immo_1"]

  # Séries de données
  dwelling_stock <- df[, "dwelling stocks"]
  housing_price <- df[, "RHP"] / 100

  # Calcul du patrimoine immobilier
  # Dwelling exprimé en million donc pas besoin de multiplier par la pop
  hw <- sf * dwelling_stock * housing_price

  # Ajout au dataset
  df["immo_slac"] <- hw

  return(df)
}

conversion_monetaire <- function(df, pays) {
  # Fonction qui convertit les séries en euros
  # Séries: 1 USD -> x euros et 1£ -> x euros

  if (pays == "US") {
    tx_change <- df[, "usd_to_euro"]
  } else if (pays == "UK") {
    tx_change <- df[, "pound_to_euro"]
  }

  # Conversion des séries
  df["conso"] <- df["conso"] * tx_change
  df["revenu"] <- df["revenu"] * tx_change
  df["actif_fin"] <- df["actif_fin"] * tx_change
  df["immo_1"] <- df["immo_1"]* tx_change

  return(df)
}

year_dummy <- function(df, threshold) {
  # Fonction qui créée une indicatrice en fonction d'une date limite

  df["year_dummy"] <- 0

  for (row in 1:nrow(df)) {
    if (rownames(df)[row] >= threshold) {
      df[row, "year_dummy"] <- 1
    }
  }

  return(df)
}


X13 <- function(df) {
  # Fonction pour désaisonnaliser des séries de données
  # Méthode: X-13 ARIMA du Census Bureau (US Gov)

  library(seasonal)

  adj_data <- seas(df)
  deseasonalized_series <- final(adj_data)

  return(deseasonalized_series)
}

#Utilisée dans PMC.r pour traiter les variables de contrôle

compute_weighted_sum_lag <- function(x, chi) {
  n <- length(x)
  S <- numeric(n)  # Initialiser le vecteur résultant

  for (t in 2:n) {
    if (t == 1) {
      S[t] <- NA
    } else {
      S[t] <- sum(x[2:(t - 1)] * chi^((t - 2):1))
    }
  }

  return(S)
}

synthese <- function(freq,date_debut,date_fin){

source("code/utils/data_treatment.R")
source("code/utils/data_visualisation.R")
source("code/models/chi.R")
source("code/models/PMC.R")

fichier <- "code/data.xlsx"
feuilles <- excel_sheets(fichier)

resultats <- data.frame(Pays = character(), Chi = numeric(), std_chi = numeric(), PMC_ev_res = numeric(), PMC_ev = numeric(), clr = data.frame(), p_value = numeric(), chi = numeric())

date_temp <- as.Date(date_debut)

for (nom_pays in feuilles) {

  if(nom_pays != "France_old" & nom_pays != "US_old"){
  donnees <- excel_import(fichier, nom_pays)
  donnees <- donnees[rownames(donnees) >= date_debut & rownames(donnees) <= date_fin, ]
  print(nom_pays)

  if (nom_pays == "France" || nom_pays == "Italie" || nom_pays == "Espagne") {
  temp <- ts(donnees["conso"],
             start = c(as.numeric(format(date_temp, "%Y")), as.numeric(format(date_temp, "%m"))),
             frequency = 4)
  donnees["conso"] <- X13(temp)
}

# Revenu
if (nom_pays == "UK" || nom_pays == "Italie" || nom_pays == "Espagne") {
  temp <- ts(donnees["revenu"],
             start = c(as.numeric(format(date_temp, "%Y")), as.numeric(format(date_temp, "%m"))),
             frequency = 4)
  donnees["revenu"] <- X13(temp)}

  chi <- chi(donnees,date_debut,date_fin, 1)
  PMC_res <- PMC(donnees, 1, date_debut, date_fin, 1,TRUE)
  PMC <- PMC(donnees, 1, date_debut, date_fin, 1,FALSE)
  
  resultats <- rbind(resultats, data.frame(Pays = nom_pays, Chi = chi['chi'], std_chi = chi['std_chi'],PMC_ev_res = unlist(PMC_res['PMC_ev']), PMC_ev = unlist(PMC['PMC_ev']), clr = as.data.frame(chi['clr']), p_value = chi['robpval'], R1 = chi['R1']))
  }}
 return(resultats)
 }