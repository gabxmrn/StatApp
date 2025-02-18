df_richesse <- function(donnees, debut, fin, serie_immo) {

  # Sélection de la série à utiliser pour le patrimoine
  if (serie_immo == 1) {
    nom_immo <- "immo_1"
  } else if (serie_immo == 2) {
    nom_immo <- "immo_2"
  } else {
    stop("Erreur : serie_immo doit être 1 ou 2")
  }

  # Nouveau dataframe avec uniquement le patrimoine financier et immobilier
  df <- donnees[rownames(donnees) >= debut & rownames(donnees) <= fin,
                c("actif_fin", nom_immo)]
  colnames(df) <- c("financier", "immobilier")

  # Ajout de la richesse totale du ménage
  df["totale"] <- df["financier"] + df["immobilier"]

  # Expression de la richesse par habitant
  pop <- donnees[rownames(donnees) >= debut & rownames(donnees) <= fin,
                 "population"] / 10^6

  df <- df / pop

  # Correction de la consommation avec l'indice des prix
  cpi <- donnees[rownames(donnees) >= debut & rownames(donnees) <= fin,
                 "cpi"]

  df <- df / cpi

  return(df)
}


df_consommation <- function(donnees, debut, fin) {

  # Nouveau dataframe avec uniquement la consommation et le CPI
  df <- donnees[rownames(donnees) >= debut & rownames(donnees) <= fin,
                "conso", drop = FALSE]

  cpi <- donnees[rownames(donnees) >= debut & rownames(donnees) <= fin,
                 "cpi"]

  pop <- donnees[rownames(donnees) >= debut & rownames(donnees) <= fin,
                 "population"] / 10^6

  # Expression de la consommation par habitant
  df <- df / pop

  # Correction de la consommation avec l'indice des prix
  df <- df / cpi

  # Calcul du log de la consommation
  log_diff <- diff(log(df[[1]]))

  # Ajout au df
  df$var_log_conso <- c(NA, log_diff)
  df <- df[-1, , drop = FALSE]

  return(df)
}
