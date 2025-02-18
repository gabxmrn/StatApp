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