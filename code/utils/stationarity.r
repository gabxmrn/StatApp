stationarite <- function(df)  {

  library(urca)

  for (col in colnames(df)) {

    cat("\nAnalyse de la colonne :", col, "\n")

    # Affichage des graphiques ACF et PACF
    par(mfrow = c(2, 1))
    acf(df[[col]], main = paste("ACF de", col))
    pacf(df[[col]], main = paste("PACF de", col))
    par(mfrow = c(1, 1))

    # Test ADF
    adf_test <- ur.df(df[[col]], type = "trend", selectlags = "AIC")
    print(summary(adf_test))

    # Test de KPSS
    kpps_test <- ur.kpss(df[[col]], type = "mu")
    print(summary(kpps_test))

  }
}