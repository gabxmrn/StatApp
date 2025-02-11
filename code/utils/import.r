excel_import <- function(chemin, feuille) {

  library(readxl)

  df <- read_excel(chemin, sheet = feuille,
                   col_names = TRUE)
  df <- as.data.frame(df)
  rownames(df) <- as.Date(df[[1]])
  df <- df[-1]

  return(df)
}