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
data_us_new_var <- data_us$date

#Obtenir \Delta log(C_t)
data_us_new_var$delta_log_c <- c(diff(log(data_us$conso[seq(1, nrow(data_us), by = 4)])),NA)
#Obtenir le lag t-1 : \Delta log(C_(t-1))
data_us_new_var$lag_delta_log_c <- lead(data_us_new_var$delta_log_c, n = 1)

print(tail(data_us_new_var))

model <- lm(delta_log_c ~ lag_delta_log_c, data = data_us_new_var, na.action = na.omit)
chi <- coef(model)["lag_delta_log_c"]

print(summary(model))
print(chi)