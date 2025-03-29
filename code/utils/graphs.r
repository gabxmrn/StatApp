plot_richesse <- function(df, pays) {

  if (pays == "France" || pays == "Italie" || pays == "Espagne") {
    yleg <- "Valeur en euros (base 2015)"
  } else if (pays == "US") {
    yleg <- "Valeur en dollars (base 2015)"
  } else if (pays == "UK") {
    yleg <- "Valeur en pound sterling (base 2015)"
  }

  dates <- as.Date(rownames(df))

  y_min <- min(c(df$pat_immo, df$pat_fin))
  y_max <- max(c(df$pat_immo, df$pat_fin))

  plot(dates, df$pat_immo, type = "l",
       main = paste("Evolution de la richesse - ", pays),
       xlab = "Années",
       ylab = yleg,
       ylim = c(y_min, y_max))
  lines(dates, df$pat_fin, type = "l", lty = "dashed")

  legend("bottomright", legend = c("Patrimoine immobilier", "Patrimoine financier"),
         lty = c("solid", "dashed"))

}

plot_consommation <- function(df, pays) {

  if (pays == "France" || pays == "Italie" || pays == "Espagne") {
    yleg <- "Valeur en euros (base 2015)"
  } else if (pays == "US") {
    yleg <- "Valeur en dollars (base 2015)"
  } else if (pays == "UK") {
    yleg <- "Valeur en pound sterling (base 2015)"
  }

  dates <- as.Date(rownames(df))

  par(mfrow = c(1, 2))
  plot(dates, df$conso, type = "l",
       main = paste("Evolution de la consommation - ", pays),
       xlab = "Années",
       ylab = yleg,
       ylim = c(min(df$conso), max(df$conso)))

  plot(dates, df$var_log_conso, type = "l",
       main = paste("Variation du log de la consommation - ", pays),
       xlab = "Années",
       ylab = yleg,
       ylim = c(min(df$var_log_conso), max(df$var_log_conso)))

  par(mfrow = c(1, 1))

}

visualisation_controles <- function(variables_controle, pays) {

  dates <- as.Date(rownames(variables_controle))

  op <- par(no.readonly = TRUE)

  # Affichage des graphiques
  par(mfrow = c(2, 3), oma = c(0, 0, 3, 0))  

  # Pour chaque variable
  for (i in 1:ncol(variables_controle)) {

    y_min <- min(variables_controle[[i]], na.rm = TRUE)
    y_max <- max(variables_controle[[i]], na.rm = TRUE)

    if (names(variables_controle)[i] == "sentiment_lag2") {
      yaxis <- "Valeur"
    } else {
      yaxis <- "Pourcents"
    }

    if (names(variables_controle)[i] == "croissance_conso") {
      title <- "Croissance de la consommation"
    } else if (names(variables_controle)[i] == "croissance_revenu") {
      title <- "Croissance du revenu"
    } else if (names(variables_controle)[i] == "taux_chomage_lag2") {
      title <- "Taux de chômage"
    } else if (names(variables_controle)[i] == "interet_diff") {
      title <- "Différenciation du taux CT"
    } else if (names(variables_controle)[i] == "spread_interet_lag2") {
      title <- "Spread des taux"
    } else {
      title <- "Confiance des consommateurs"
    }

    # Création du graphique
    plot(dates,
         variables_controle[[i]],
         type = "l",
         main = title,
         xlab = "Années",
         ylab = yaxis,
         ylim = c(y_min, y_max))
  }

  mtext(paste("Évolution des variables de contrôle -", pays),
        outer = TRUE, cex = 1.5, font = 2)
  par(op)
}
