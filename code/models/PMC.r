
#data_us_new_var n'est pas que pour les US, c'est une ancienne notation
#probablement encore un problème dans les données de richesse us
#et aussi un peut etre un problème de date dans la fusion de data_us_new_var et richesse_us

PMC <- function(df, freq, date_debut, date_fin, methode_immo, chi_restricted = FALSE) {

  library(dplyr)
  library(here)

  source(here("code/models/chi.R"))
  source(here("code/utils/data_treatment.R"))

  #df des nouvelles variables
  data_us_new_var <- df[seq(1, nrow(df), by = freq), ]

  #ajout spread et croissance conso et revenu
  data_us_new_var$spread <- data_us_new_var$taux_lt - data_us_new_var$taux_ct
  data_us_new_var <- data_us_new_var %>%
    mutate(income_growth = (revenu - lag(revenu)) / lag(revenu) * 100)
  data_us_new_var <- data_us_new_var %>%
    mutate(conso_growth = (conso - lag(conso)) / lag(conso) * 100)

  #Donne le "differenced" taux_ct : change pas franchement le résultat et conflit de notation à revoir
  data_us_new_var <- data_us_new_var %>%
    mutate(diff_taux_ct = (taux_ct - lag(taux_ct)))

  #obtenir le terme d'erreur pour régression
  chi_fun <- chi(df, freq)
  #epsilon <- chi_fun$residuals


  #obtenir delta W_t
  richesse_us <- df_richesse(df, date_debut, date_fin, methode_immo)
  data_us_new_var$wealth <- richesse_us[seq(1, nrow(df), by = freq), "pat_total"]
  #data_us_new_var$epsilon <- epsilon
  # data_us_new_var <- data_us_new_var %>%
  #   mutate(delta_W = (wealth - lag(wealth))/ lag(conso))

  # #première régression dite pas efficace par Slacalek
  # model <- lm(epsilon ~ delta_W + diff_taux_ct + spread + income_growth, data = data_us_new_var, na.action = na.omit)

  # alpha_w <- coef(model)["delta_W"]
  # #calcul avec chi = 0.6
  # PMC_ev <- alpha_w / 0.24

#obtention des nouvelles variables pour la 2eme méthode
# Calcul de ΔW = Wt - Wt-1
  data_us_new_var <- data_us_new_var %>%
  mutate(Delta_W = wealth - lag(wealth, 1),
  Delta_log_conso = log(conso) - lag(log(conso),1))


  # Définir chi
  if (chi_restricted == TRUE) {
    chi <- 0.6
  } else {
    chi <- chi_fun$chi
  }

  #calcul de delta_barre_W (voir article p.20)
  data_us_new_var <- data_us_new_var %>%
    mutate(
      Delta_W_t1 = lag(Delta_W, 1),  # Décalage de ΔW par 1
      Delta_W_t2 = lag(Delta_W, 2),  # Décalage de ΔW par 2
      Delta_W_t3 = lag(Delta_W, 3),  # Décalage de ΔW par 3
      delta_barre_W = chi * (Delta_W + chi * Delta_W_t1 + chi^2 * Delta_W_t2 + chi^3 * Delta_W_t3)/lag(conso,4)
    )

  #Calcul de delta_C (p.20) et lag de toutes les variables
  data_us_new_var <- data_us_new_var %>%
    mutate(delta_C = (conso - lag(conso))/lag(conso,5),
    across(c(chomage, diff_taux_ct,spread ,income_growth), ~compute_weighted_sum_lag(.x,chi), .names = "acc_{.col}"))

  #régression de la p.20
  model_2 <- lm(delta_C ~ lag(delta_barre_W) + acc_income_growth + acc_chomage + acc_diff_taux_ct + acc_spread, data = data_us_new_var, na.action = na.omit)
  alpha_w <- as.numeric(coef(model_2)["lag(delta_barre_W)"])[[1]]
  std <- summary(model_2)$coefficients["lag(delta_barre_W)", 2]


  # PMC immédiate
  PMC_imm <- alpha_w / chi

  # PMV éventuelle
  PMC_ev <- alpha_w / (chi * (1 - chi))
  result <- list(PMC_imm <- PMC_imm, PMC_ev <- PMC_ev, std <- std)
 
  return(result)

}

#Fonction pour le tracé de la PMC en fonction du temps
#memes entrées que PMC avec maille longueur de la plage temporelle de calcul
plot_PMC <- function(df,maille, date_debut, date_fin,freq,chi_restricted = FALSE){

source("code/models/PMC.r")
source("code/models/chi.R")

library(lubridate)
library(ggplot2)

step <- "1 year" #pas du graphique
window <- maille #longueur de la plage temporelle
resultats <- data.frame(Annee = numeric(), Valeur = numeric(), std = numeric())

#Construction des plages d'années pour les calculs
for (annee in seq.Date(as.Date(date_debut), as.Date(date_fin) - years(window), by = step)) {
  debut <- as.Date(annee)
  fin <- as.Date(annee) + years(window)

#la PMC considérée est la PMC_ev dans ce code(modifiable facilement)
#Construction du tableau de valeurs pour le plot (PMC_ev, écart-type)
  valeur <- PMC(df, freq, debut, fin, chi_restricted)[[2]] #transformer 2 en 1 pour passer à la PMC_im
  std <- PMC(df, freq, debut, fin, chi_restricted)[[3]] #la std prise est celle de alpha_w, valable uniquement pour chi_restricted = TRUE
  #p_value <- chi(df,debut, fin,freq)["p_value"]
  
  # Stocker les résultats
  resultats <- rbind(resultats, data.frame(Annee = as.Date(annee) + years(window%/%2), Valeur = valeur, std=std))
}

print(resultats)

#coefficient pour l'intervalle de confiance à 95%
coef_IC <- 1.96 / sqrt((4*window)/freq)
moyenne_PMC <- mean(resultats$Valeur, na.rm = TRUE)

# Tracer le graphique avec ggplot2
plot <- ggplot(resultats, aes(x = Annee, y = Valeur)) +
  geom_point(color = "blue", size = 3) +  # Points des valeurs
  geom_errorbar(aes(ymin = Valeur - coef_IC*std, ymax = Valeur + coef_IC*std), width = 0.5, color = "black") +
  #geom_text(aes(label = paste("p-value:", round(as.numeric(p_value),4))), 
  #          hjust = -0.1, vjust = -0.5, size = 4, color = "black") +
  geom_hline(yintercept = moyenne_PMC, linetype = "dashed", color = "red", size = 1) +
  geom_smooth(method = "loess", se = TRUE, color = "purple", fill = "lightgray", size = 1.2) +  # Courbe approximative, utilise une régression LOESS (Local Polynomial Regression)
  labs(title = "Évolution de la valeur avec barres d'erreur",
       x = "Année de milieu de la plage",
       y = "PMC estimée") +
  theme_minimal()

print(plot)

}