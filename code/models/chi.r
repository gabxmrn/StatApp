
#data_us_new_var n'est pas utilisé que pour les US
#c'est une ancienne notation

chi <- function(df, date_debut,date_fin,freq) {

#to get lag function
library(dplyr)


#df des nouvelles variables
df <- df[rownames(df) >= date_debut & rownames(df) <= date_fin, ]
data_us_new_var <- df[seq(1, nrow(df), by = freq), ]

#ajout spread et croissance conso et revenu, (correction par cpi nécessaire ? faite)
data_us_new_var <- data_us_new_var %>%
  mutate(conso = conso/(population),
  revenu = revenu/(population),
    income_growth = (revenu - lag(revenu)) / lag(revenu) * 100,
  conso_growth = (conso - lag(conso)) / lag(conso) * 100)

#Donne le "differenced" taux_ct
data_us_new_var <- data_us_new_var %>%
  mutate(diff_taux_ct = (taux_ct - lag(taux_ct)))


#Obtenir \Delta log(C_t), représente la variation de C_t
data_us_new_var$delta_log_c <- c(NA, diff(log(data_us_new_var$conso)))


#Obtenir les lag t-1 et t-2, on les nomme lag1 et lag2 suivi du nom de la variable originale
data_us_new_var <- data_us_new_var %>%
  mutate(across(c(chomage, diff_taux_ct, conso_growth, spread,confiance,income_growth), ~ lag(.x, n = 2), .names = "lag2_{.col}"))

data_us_new_var <- data_us_new_var %>%
  mutate(across(c(chomage, diff_taux_ct, conso_growth,spread , confiance,income_growth,delta_log_c), ~ lag(.x, n = 1), .names = "lag1_{.col}"))

#première régression sur les variables instrumentales, pour faire la reg IV de Delta log ct sur son lag t-1
model_IV <- lm(lag1_delta_log_c ~ lag2_chomage + lag2_diff_taux_ct + lag2_spread + lag2_conso_growth + lag2_confiance + lag2_income_growth, data = data_us_new_var, na.action = na.omit)

#reconstruction de la prédiction sur les IV
#new_data <- data_us_new_var %>%
#  select(lag1_chomage ,lag1_diff_taux_ct,lag1_spread,lag1_conso_growth, lag1_confiance, lag1_income_growth) %>%  # Sélectionner les colonnes
#  rename(lag2_chomage = lag1_chomage, lag2_diff_taux_ct = lag1_diff_taux_ct, lag2_spread = lag1_spread, lag2_conso_growth = lag1_conso_growth, lag2_confiance = lag1_confiance, lag2_income_growth = lag1_income_growth)

#construction de l'argument pour faire la prédiction du régressseur dans la 2eme partie de l'IV reg
new_data <- data_us_new_var %>%
  select(lag2_chomage ,lag2_diff_taux_ct,lag2_spread,lag2_conso_growth, lag2_confiance, lag2_income_growth) # Sélectionner les colonnes

data_us_new_var$predicted_lag1_delta_log_c <- predict(model_IV, newdata = new_data)

#régression finale
model <- lm(delta_log_c ~ predicted_lag1_delta_log_c, data = data_us_new_var, na.action = na.omit)

#Essai avec une IV reg intégrée (les deux donnent les memes chi sur fr et us)
library(AER)  # Load the package for ivreg

iv_model <- ivreg(delta_log_c ~ lag1_delta_log_c | lag2_chomage + lag2_diff_taux_ct + lag2_spread + lag2_conso_growth + lag2_confiance + lag2_income_growth, data = data_us_new_var, na.action = na.omit)

chi <- coef(iv_model)["lag1_delta_log_c"]

coef_lag1 <- summary(iv_model)$coefficients["lag1_delta_log_c", 1]  # Coefficient
std_lag1 <- summary(iv_model)$coefficients["lag1_delta_log_c", 2]   # Écart-type
p_value_lag1 <- summary(iv_model)$coefficients["lag1_delta_log_c", 4]   # p-value
#print(summary(iv_model))

#Pour le calcul ultérieur de la PMC (résidu de l'ivreg)
residuals <- c(NA,NA,NA,as.vector(residuals(model)))

result <- list(chi = coef_lag1, std_chi = std_lag1, p_value = p_value_lag1, residuals = residuals)

return(result)

}

#Fonction pour le tracé de chi en fonction du temps
#memes entrées que chi avec maille longueur de la plage temporelle de calcul
plot_chi <- function(df,maille, date_debut, date_fin,freq){

source("code/models/chi.R")

library(lubridate)
library(ggplot2)

step <- "1 month" #pas du graphique
window <- maille #longueur de la plage temporelle
resultats <- data.frame(Annee = numeric(), Valeur = numeric(), Ecart_Type = numeric())

#Construction des plages d'années pour les calculs
for (annee in seq.Date(as.Date(date_debut), as.Date(date_fin) - years(window), by = step)) {
  debut <- as.Date(annee)
  fin <- as.Date(annee) + years(window)
  
#Construction du tableau de valeurs pour le plot (chi, écart-type et p value de la fonction chi)
  valeur <- chi(df,debut, fin,freq)["chi"]
  ecart_type <- chi(df,debut, fin,freq)["std_chi"]
  p_value <- chi(df,debut, fin,freq)["p_value"]
  
  # Stocker les résultats
  resultats <- rbind(resultats, data.frame(Annee = as.Date(annee) + years(window%/%2), Valeur = valeur, Ecart_Type = ecart_type, p_values = p_value))
}

print(resultats)

#coefficient pour l'intervalle de confiance à 95%
coef_IC <- 1.96 / sqrt((4*window)/freq)
moyenne_chi <- mean(resultats$chi, na.rm = TRUE)

# Tracer le graphique avec ggplot2
plot <- ggplot(resultats, aes(x = Annee, y = chi)) +
  geom_point(color = "blue", size = 3) +  # Points des valeurs
  geom_errorbar(aes(ymin = chi - coef_IC*std_chi, ymax = chi + coef_IC*std_chi), width = 0.5, color = "black") +
  geom_text(aes(label = paste("p-value:", round(as.numeric(p_value),4))), 
            hjust = -0.1, vjust = -0.5, size = 4, color = "black") +
  geom_hline(yintercept = moyenne_chi, linetype = "dashed", color = "red", size = 1) +
  geom_smooth(method = "loess", se = TRUE, color = "purple", fill = "lightgray", size = 1.2) +  # Courbe approximative, utilise une régression LOESS (Local Polynomial Regression)
  labs(title = "Évolution de la valeur avec barres d'erreur",
       x = "Année de milieu de la plage",
       y = "Chi estimé") +
  theme_minimal()

print(plot)

}