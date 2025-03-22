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
                 "population"]

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
                 "population"]

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

df_control <- function(donnees, debut, fin) {

  data_sub <- donnees[rownames(donnees) >= debut & rownames(donnees) <= fin, ]

  # Séries de la consommation et du revenu: expression par tête
  pop <- data_sub[rownames(data_sub) >= debut & rownames(data_sub) <= fin,
                  "population"]
  data_sub["conso"] <- data_sub["conso"] / pop
  data_sub["revenu"] <- data_sub["revenu"] / pop

  # Séries de la consommation et du revenu: correction de l'inflation
  cpi <- data_sub[rownames(data_sub) >= debut & rownames(data_sub) <= fin,
                  "cpi"]
  data_sub["conso"] <- data_sub["conso"] / cpi
  data_sub["revenu"] <- data_sub["revenu"] / cpi

  # Construction des variables de contrôle à partir de data_fr_sub
  # Après diff(), la longueur devient nrow(data_fr_sub) - 1 et, après lag(2),
  # les 2 premiers indices = NA. On garde les indices valides de 3 à (nrow - 1).
  valid <- 3:(nrow(data_sub) - 1)
  rownames_valid <- rownames(data_sub)[valid]

  variables_controle <- data.frame(
    # Calcul de la croissance de la consommation (diff du log, 2 lag)
    croissance_conso = dplyr::lag(diff(log(data_sub$conso)), 2)[valid],

    # Calcul de la croissance du revenu (différence du logarithme, lag de 2)
    croissance_revenu = dplyr::lag(diff(log(data_sub$revenu)), 2)[valid],

    # Taux de chômage décalé de 2 périodes
    taux_chomage_lag2 = dplyr::lag(data_sub$chomage, 2)[valid],

    # Différenciation du taux d'intérêt à court terme, puis lag de 2
    interet_diff = dplyr::lag(diff(data_sub$taux_ct), 2)[valid],

    # Spread des taux décalé de 2 périodes
    spread_interet_lag2 = dplyr::lag(data_sub$spread, 2)[valid],

    # Sentiment des consommateurs décalé de 2 périodes
    sentiment_lag2 = dplyr::lag(data_sub$confiance, 2)[valid]
  )

  rownames(variables_controle) <- rownames_valid

  return(variables_controle)
}

#Utilisée dans PMC.r pour traiter les variables de contrôle

compute_weighted_sum_lag <- function(x, chi) {
  n <- length(x)
  S <- numeric(n)  # Initialiser le vecteur résultant

  for (t in 2:n) {
    if(t==1){
    S[t] <- NA
  }else{
    S[t] <- sum(x[2:(t-1)] * chi^((t-2):1))}
  }
  
  return(S)
}