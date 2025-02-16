cor_conso_intertemp <- function(donnees, debut, fin, methode) {
    df <- donnees[rownames(donnees) >= debut & rownames(donnees) <= fin,
                "conso"]

    df["lag_conso"] <- dplyr::lag(df["conso"], n = 5)

    df <- na.omit(df)    
   
    correlation <- cor(df["conso"], df["lag_conso"], method = methode)
    
    print(correlation)
}
