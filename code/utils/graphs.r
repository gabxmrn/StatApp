plot_richesse <- function(df, pays) {

  if (pays == "France") {
    yleg <- "Valeur en Millions d'euros (base 2015)"
  } else if (pays == "US") {
    yleg <- "Valeur en Millions de dollars"
  }

  dates <- as.Date(rownames(df))

  y_min <- min(c(df$immobilier, df$financier))
  y_max <- max(c(df$immobilier, df$financier))

  plot(dates, df$immobilier, type = "l",
       main = paste("Evolution de la richesse - ", pays),
       xlab = "Années",
       ylab = yleg,
       ylim = c(y_min, y_max))
  lines(dates, df$financier, type = "l", lty = "dashed")

  legend("bottomright", legend = c("Patrimoine immobilier", "Patrimoine financier"),
         lty = c("solid", "dashed"))

}

plot_consommation <- function(df, pays) {

  if (pays == "France") {
    yleg <- "Valeur en Millions d'euros (base 2015)"
  } else if (pays == "US") {
    yleg <- "Valeur en Millions de dollars"
  }

  dates <- as.Date(rownames(df))

  par(mfrow = c(2, 1))
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
  par(mfrow = c(2, 3))

  # Pour chaque variable
  for (i in 1:ncol(variables_controle)) {
    message("Graphique: ", names(variables_controle)[i])

    y_min <- min(variables_controle[[i]], na.rm = TRUE)
    y_max <- max(variables_controle[[i]], na.rm = TRUE)

    # Création du graphique
    plot(dates,
         variables_controle[[i]],
         type = "l",
         main = names(variables_controle)[i],
         xlab = "Date",
         ylab = names(variables_controle)[i],
         ylim = c(y_min, y_max))
  }

  mtext(paste("Évolution des variables de contrôle -", pays),
        outer = TRUE, cex = 1.5, font = 2)
  par(op)
}
