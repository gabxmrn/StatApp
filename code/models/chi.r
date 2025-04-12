chi <- function(df, debut, fin, freq) {
  # Fonction qui permet de calculer la persistance de la consommation
  # Méthode: Slacalek, régression avec variable instrumentale
  # Variable expliquée (Y): variation du log de la conso (t)
  # Variable explicative (X): variation du log de la conso (t-1)
  # Instruments (Z): croissance conso, croissance revenu, spread, confiance consommateurs, taux CT et chômage


  # Importations
  library(dplyr)
  library(ivmodel)

  # Dataframe (annuel ou trimestriel)
  data_new <- df[rownames(df) >= debut & rownames(df) <= fin, ][seq(1, nrow(df), by = freq), ]

  # Z: calcul croissance de la conso et du revenu par habitant (base 2015)
  data_new <- data_new %>%
    mutate(conso = conso / (population * cpi),
           revenu = revenu / (population * cpi),
           income_growth = (revenu - lag(revenu)) / lag(revenu) * 100,
           conso_growth = (conso - lag(conso)) / lag(conso) * 100)

  # Z: calcul différence du taux CT
  data_new <- data_new %>%
    mutate(diff_taux_ct = (taux_ct - lag(taux_ct)))

  # Y: Calcul de Delta log(C_t), représente la variation de C_t
  data_new$delta_log_c <- c(NA, diff(log(data_new$conso)))

  # Obtenir les lag t-1 et t-2
  # On les nomme lag1 et lag2 suivi du nom de la variable originale
  data_new <- data_new %>%
    mutate(across(c(chomage, diff_taux_ct, conso_growth, spread,confiance,income_growth),
                  ~ lag(.x, n = 2),
                  .names = "lag2_{.col}"))

  data_new <- data_new %>%
    mutate(across(c(chomage, diff_taux_ct, conso_growth,spread , confiance,income_growth,delta_log_c),
                  ~ lag(.x, n = 1),
                  .names = "lag1_{.col}"))

  # Rég 1: Z sur X
  model_IV <- lm(lag1_delta_log_c ~ lag2_chomage + lag2_diff_taux_ct + lag2_spread + lag2_conso_growth + lag2_confiance + lag2_income_growth,
                 data = data_new, na.action = na.omit)

  # Construction de l'argument pour faire la prédiction du régressseur dans la 2eme partie de l'IV reg
  new_data <- data_new %>%
    select(lag2_chomage,lag2_diff_taux_ct,lag2_spread,lag2_conso_growth, lag2_confiance, lag2_income_growth)

  data_new$predicted_lag1_delta_log_c <- predict(model_IV, newdata = new_data)

  # Rég 2: X sur Y
  model <- lm(delta_log_c ~ predicted_lag1_delta_log_c, data = data_new, na.action = na.omit)

  # Utilisation d'un package, pour tous les outputs annexes
  y <- data_new$delta_log_c
  x <- data_new$lag1_delta_log_c
  z <- cbind(data_new$lag2_chomage,
             data_new$lag2_diff_taux_ct,
             data_new$lag2_spread,
             data_new$lag2_conso_growth,
             data_new$lag2_confiance,
             data_new$lag2_income_growthy)

  # Régression instrumentale
  iv_model <- ivmodel(Y = y, D = x, Z = z)

  # Coefficients
  chi <- iv_model$kClass$point.est[2]
  std <- iv_model$kClass$std.err[2]
  pval <- iv_model$kClass$p.value[2]

  # Moreira CLR (2003)
  clr <- iv_model$CLR$ci
  rob_pval <- iv_model$CLR$p.value

  #Pour le calcul ultérieur de la PMC (résidu de l'ivreg)
  residuals <- c(NA,NA,NA,as.vector(residuals(model)))

  #pour calculer le R1^2 barre
  adjust_R1 <- summary(model_IV)$adj.r.squared

  result <- list(model = iv_model,
                 chi = chi, std_chi = std, pvalue = pval, residuals = residuals,
                 clr = clr, robpval = rob_pval, R1 = adjust_R1)

  return(result)
}

chi_with_year_dummy <- function(df, freq) {

  # Importations
  library(dplyr)
  library(ivmodel)
  library(here)
  source(here("code/utils/data_treatment.R"))

  # Data frame de travail
  data_new <- df[seq(1, nrow(df), by = freq), ]
  data_new <- year_dummy(data_new, "2020-01-01")

    #ajout spread et croissance conso et revenu, (correction par cpi nécessaire ? faite)
  data_new <- data_new %>%
    mutate(conso = conso/(population*cpi),
    revenu = revenu/(population*cpi),
      income_growth = (revenu - lag(revenu)) / lag(revenu) * 100,
    conso_growth = (conso - lag(conso)) / lag(conso) * 100)

  #Donne le "differenced" taux_ct
  data_new <- data_new %>%
    mutate(diff_taux_ct = (taux_ct - lag(taux_ct)))

  #Obtenir \Delta log(C_t), représente la variation de C_t
  data_new$delta_log_c <- c(NA, diff(log(data_new$conso)))

  #Obtenir les lag t-1 et t-2, on les nomme lag1 et lag2 suivi du nom de la variable originale
  data_new <- data_new %>%
    mutate(across(c(chomage, diff_taux_ct, conso_growth, spread,confiance,income_growth, year_dummy), ~ lag(.x, n = 2), .names = "lag2_{.col}"))

  data_new <- data_new %>%
    mutate(across(c(chomage, diff_taux_ct, conso_growth,spread , confiance,income_growth,delta_log_c, year_dummy), ~ lag(.x, n = 1), .names = "lag1_{.col}"))

  #première régression sur les variables instrumentales, pour faire la reg IV de Delta log ct sur son lag t-1
  model_IV <- lm(lag1_delta_log_c ~ year_dummy + lag2_chomage + lag2_diff_taux_ct + lag2_spread + lag2_conso_growth + lag2_confiance + lag2_income_growth, data = data_new, na.action = na.omit)

  #construction de l'argument pour faire la prédiction du régressseur dans la 2eme partie de l'IV reg
  new_data <- data_new %>%
    select(year_dummy, lag2_chomage ,lag2_diff_taux_ct,lag2_spread,lag2_conso_growth, lag2_confiance, lag2_income_growth) # Sélectionner les colonnes

  data_new$predicted_lag1_delta_log_c <- predict(model_IV, newdata = new_data)

  #régression finale
  model <- lm(delta_log_c ~ predicted_lag1_delta_log_c, data = data_new, na.action = na.omit)

  # Utilisation d'un package, pour tous les outputs annexes
  y <- data_new$delta_log_c
  x <- data_new$lag1_delta_log_c
  z <- cbind(data_new$lag2_chomage,
             data_new$lag2_diff_taux_ct,
             data_new$lag2_spread,
             data_new$lag2_conso_growth,
             data_new$lag2_confiance,
             data_new$lag2_income_growth,
             data_new$year_dummy)

  # Régression instrumentale
  iv_model <- ivmodel(Y = y, D = x, Z = z)

  # Coefficients
  chi <- iv_model$kClass$point.est[2]
  std <- iv_model$kClass$std.err[2]
  pval <- iv_model$kClass$p.value[2]

  # Moreira CLR (2003)
  clr <- iv_model$CLR$ci

  # Test de Moreira
  rob_pval <- CLR.test(iv_model)$p.value

  #Pour le calcul ultérieur de la PMC (résidu de l'ivreg)
  residuals <- c(NA,NA,NA,as.vector(residuals(model)))

  result <- list(model = iv_model,
                 chi = chi, std_chi = std, p_value = pval, residuals = residuals,
                 clr = clr, robpval = rob_pval)

  return(result)

}


#Fonction pour le tracé de chi en fonction du temps
#memes entrées que chi avec maille longueur de la plage temporelle de calcul
plot_chi <- function(df, maille, date_debut, date_fin, freq) {

  library(lubridate)
  library(ggplot2)
  library(here)

  source(here("code/models/chi.R"))


step <- "1 year" #pas du graphique
window <- maille #longueur de la plage temporelle
resultats <- data.frame(Annee = numeric(), Valeur = numeric(), Ecart_Type = numeric())

  #Construction des plages d'années pour les calculs
  for (annee in seq.Date(as.Date(date_debut), as.Date(date_fin) - years(window), by = step)) {
    debut <- as.Date(annee)
    fin <- as.Date(annee) + years(window)

  # Construction du tableau de valeurs pour le plot (chi, écart-type et p value de la fonction chi)
    valeur <- chi(df,debut, fin,freq)["chi"]
    ecart_type <- chi(df,debut, fin,freq)["std_chi"]
    p_value <- chi(df,debut, fin,freq)["p_value"]

    # Stocker les résultats
    resultats <- rbind(resultats, data.frame(Annee = as.Date(annee) + years(window%/%2), Valeur = valeur, Ecart_Type = ecart_type, p_values = p_value))
  }

  #coefficient pour l'intervalle de confiance à 95%
  coef_IC <- 1.96 / sqrt((4*window)/freq)
  moyenne_chi <- mean(resultats$chi, na.rm = TRUE)

  # Tracer le graphique avec ggplot2
  plot <- ggplot(resultats, aes(x = Annee, y = chi)) +
    geom_segment(aes(x = Annee - years(window%/%2), xend = Annee + years(window%/%2), y = chi, yend = chi), color = "gray40", size = 1.2) +  # Barre temporelle
    geom_point(color = "blue", size = 3) +  # Points des valeurs
    #geom_errorbar(aes(ymin = chi - coef_IC*std_chi, ymax = chi + coef_IC*std_chi), width = 0.5, color = "black") +
    geom_text(aes(label = paste("p-value:", round(as.numeric(p_value),4))), 
              hjust = -0.1, vjust = -0.5, size = 4, color = "black") +
    geom_hline(yintercept = moyenne_chi, linetype = "dashed", color = "red", size = 1) +
    annotate("text", x = min(resultats$Annee) - 2000, y = moyenne_chi + 0.005, 
           label = paste(round(moyenne_chi, 2)), 
           vjust = 0, hjust = 1.2, size = 5, color = "red", fontface = "bold") +
    geom_smooth(method = "loess", se = TRUE, color = "purple", fill = "lightgray", size = 1.2) +  # Courbe approximative, utilise une régression LOESS (Local Polynomial Regression)
    labs(title = "Évolution de la valeur avec barres d'erreur",
        x = "Année de milieu de la plage",
        y = "Chi estimé") +
    theme_minimal()

  print(plot)

}