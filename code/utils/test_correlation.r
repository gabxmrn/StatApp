cor_conso <- function(conso, pays) {

  df <- data.frame(
    "lag0" = conso[6:nrow(conso), 1],
    "lag5" = conso[1:(nrow(conso) - 5), 1]
  )

  corr <- cor(df$lag0, df$lag5, method = "pearson")
  print(paste("La corrélation de la consommation pour la", pays, "est de", corr))
}
