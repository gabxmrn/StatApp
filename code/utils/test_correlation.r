cor_conso <- function(conso) {

  var_log_conso <- as.data.frame(conso$var_log_conso)

  df <- c(rep(NA, 5),
          (conso$conso[6:length(conso$conso)] - conso$conso[5:(length(conso$conso) - 1)]) / conso$conso[1:(length(conso$conso) - 5)])
  df <- as.data.frame(na.omit(df))

  corr <- cor(df, conso[6:nrow(conso), "var_log_conso"], method = "pearson")
  print(paste("La corrélation est de", corr))
}
