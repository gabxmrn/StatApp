install.packages("dplyr")

source("code/utils/import.R")
source("code/utils/data_treatment.R")
source("code/utils/graphs.R")
source("code/utils/test_correlation.R")
source("code/utils/stationarity.R")

###### Importation des données ######
data_fr <- excel_import("code/data.xlsx", "France")
data_us <- excel_import("code/data.xlsx", "US")

#print(head(data_us))

#to get lead function
library(dplyr)


#df des nouvelles variables
data_us_new_var <- data_us[seq(1, nrow(data_us), by = 4),]

#ajout spread et croissance conso et revenu
data_us_new_var$spread <- data_us_new_var$taux_lt - data_us_new_var$taux_st
data_us_new_var <- data_us_new_var %>%
  mutate(income_growth = (revenu - lag(revenu)) / lag(revenu) * 100)
data_us_new_var <- data_us_new_var %>%
  mutate(conso_growth = (conso - lag(conso)) / lag(conso) * 100)

#Obtenir \Delta log(C_t)
data_us_new_var$delta_log_c <- c(diff(log(data_us$conso[seq(1, nrow(data_us), by = 4)])),NA)

#Obtenir les lag t-1 et t-2, on les nomme lag1 et lag2 suivi du nom de la variable originale
data_us_new_var <- data_us_new_var %>%
  mutate(across(c(chomage, taux_st, conso_growth, spread,confiance,income_growth,delta_log_c), ~ lead(.x, n = 2), .names = "lag2_{.col}"))

data_us_new_var <- data_us_new_var %>%
  mutate(across(c(chomage, taux_st, conso_growth,spread, confiance,income_growth), ~ lead(.x, n = 1), .names = "lag1_{.col}"))

#print(tail(data_us_new_var))

#première régression sur les variables instrumentales, pour faire la reg IV de Delta log ct sur son lag t-1
model_IV <- lm(lag_delta_log_c ~ lag2_chomage + lag2_taux_st + lag2_spread + lag2_conso_growth + lag2_confiance + lag2_income_growth, data = data_us_new_var, na.action = na.omit)

#reconstruction de la prédiction sur les IV
data_us_new_var$predicted_lag_delta_log_c <- predict(model_IV, newdata = data_us_new_var[,c("lag1_chomage","lag1_conso_growth","lag1_income_growth","lag1_taux_st","lag1_spread","lag1_confiance")])

#régression finale
model <- lm(delta_log_c ~ predicted_lag_delta_log_c + cpi, data = data_us_new_var, na.action = na.omit)
chi <- coef(model)["predicted_lag_delta_log_c"]

print(chi)