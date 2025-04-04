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