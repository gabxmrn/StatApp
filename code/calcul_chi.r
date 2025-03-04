install.packages("dplyr")
source("code/utils/import.R")
source("code/utils/data_treatment.R")
source("code/utils/graphs.R")
source("code/utils/test_correlation.R")
source("code/utils/stationarity.R")

###### Importation des données ######
data_fr <- excel_import("code/data.xlsx", "France")
data_us <- excel_import("code/data.xlsx", "US")

###### Données - Richesse ######

date_debut <- "1997-12-01"
date_fin <- "2023-12-01"

# richesse_fr <- df_richesse(data_fr, date_debut, date_fin, 1)
# plot_richesse(richesse_fr, "France")

richesse_us <- df_richesse(data_us, date_debut, date_fin, 1)
print(head(data_us))

#to get lead function
library(dplyr)


#df des nouvelles variables
data_us_new_var <- data_us[seq(1, nrow(data_us), by = 4),]
data_us_new_var$spread <- data_us_new_var$taux_lt - data_us_new_var$taux_st
data_us_new_var <- data_us_new_var %>%
  mutate(income_growth = (revenu - lag(revenu)) / lag(revenu) * 100)
data_us_new_var <- data_us_new_var %>%
  mutate(conso_growth = (conso - lag(conso)) / lag(conso) * 100)

#Obtenir \Delta log(C_t)
data_us_new_var$delta_log_c <- c(diff(log(data_us$conso[seq(1, nrow(data_us), by = 4)])),NA)
#Obtenir le lag t-1 : \Delta log(C_(t-1))
data_us_new_var$lag_delta_log_c <- lead(data_us_new_var$delta_log_c, n = 1)

data_us_new_var <- data_us_new_var %>%
  mutate(across(c(chomage, taux_st, conso_growth, spread,confiance,income_growth), ~ lead(.x, n = 2), .names = "lag2_{.col}"))

data_us_new_var <- data_us_new_var %>%
  mutate(across(c(chomage, taux_st, conso_growth,spread, confiance,income_growth), ~ lead(.x, n = 1), .names = "lag1_{.col}"))

data_us_new_var$lag_delta <- lead(data_us_new_var$conso, n=2)
print(tail(data_us_new_var))

model_IV <- lm(lag_delta_log_c ~ lag2_chomage + lag2_taux_st + lag2_spread + lag2_conso_growth + lag2_confiance + lag2_income_growth, data = data_us_new_var, na.action = na.omit)
data_us_new_var$predicted_lag_delta_log_c <- predict(model_IV, newdata = data_us_new_var[,c("lag1_chomage","lag1_conso_growth","lag1_income_growth","lag1_taux_st","lag1_spread","lag1_confiance")])
model <- lm(delta_log_c ~ predicted_lag_delta_log_c + cpi, data = data_us_new_var, na.action = na.omit)
chi <- coef(model)["predicted_lag_delta_log_c"]

print(chi)