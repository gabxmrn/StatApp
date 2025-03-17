
#data_us_new_var n'est pas utilisé que pour les US
#c'est une ancienne notation

chi <- function(df, freq) {

#to get lag function
library(dplyr)


#df des nouvelles variables
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
print(summary(iv_model))

#Pour le calcul ultérieur de la PMC (résidu de l'ivreg)
residuals <- c(NA,NA,NA,as.vector(residuals(model)))

result <- list(chi = chi, residuals = residuals)

return(result)

}