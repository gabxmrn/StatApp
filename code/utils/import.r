excel_import <- function(chemin, feuille) {

  library(readxl)

  df <- read_excel(chemin, sheet = feuille,
                   col_names = TRUE)
  df <- as.data.frame(df)
  rownames(df) <- as.Date(df[[1]])
  df <- df[-1]

  return(df)
}

X13 <- function(df){

#deseasonalize X-13 ARIMA (pas encore testée)

  install.packages("seasonal")  # Install the package if not already installed
  library(seasonal)

  adj_data <- seas(df)
  deseasonalized_series <- final(adj_data)

  print(summary(adj_data))

return(deasonalized_series)
}