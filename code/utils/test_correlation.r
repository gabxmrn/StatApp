cor_conso_intertemp <- function(donnees, debut, fin) {
    df <- donnees[rownames(donnees) >= debut & rownames(donnees) <= fin,
                "conso"]
    
    acf(df)
}
