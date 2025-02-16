plot_richesse <- function(df, pays) {

  dates <- as.Date(rownames(df))

  y_min <- min(c(df$immobilier, df$financier))
  y_max <- max(c(df$immobilier, df$financier))

  plot(dates, df$immobilier, type = "l",
       main = paste("Evolution de la richesse - ", pays),
       xlab = "Années",
       ylab = "Valeur en Millions d'euros (base 2015)",
       ylim = c(y_min, y_max))
  lines(dates, df$financier, type = "l", lty = "dashed")

  legend("bottomright", legend = c("Patrimoine immobilier", "Patrimoine financier"),
         lty = c("solid", "dashed"))

}

plot_consommation <- function(df, pays) {

  dates <- as.Date(rownames(df))

  y_min <- min(df$conso)
  y_max <- max(df$conso)

  plot(dates, df$conso, type = "l",
       main = paste("Evolution de la consommation - ", pays),
       xlab = "Années",
       ylab = "Valeur en Millions d'euros (base 2015)")
       ylim = c(y_min,y_max)

}

