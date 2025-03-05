PMC <- function(df){

library(dplyr)


#df des nouvelles variables
data_us_new_var <- df[seq(1, nrow(df), by = 4),]

#ajout spread et croissance conso et revenu
data_us_new_var$spread <- data_us_new_var$taux_lt - data_us_new_var$taux_ct
data_us_new_var <- data_us_new_var %>%
  mutate(income_growth = (revenu - lag(revenu)) / lag(revenu) * 100)
data_us_new_var <- data_us_new_var %>%
  mutate(conso_growth = (conso - lag(conso)) / lag(conso) * 100)

#Donne le "differenced" taux_ct : change pas franchement le résultat et conflit de notation à revoir
data_us_new_var <- data_us_new_var %>%
  mutate(taux_ct = (taux_ct - lag(taux_ct)))


#obtenir epsilon
epsilon <- chi(df)["residuals"]

#obtenir delta W_t
date_debut <- "1998-12-01"
date_fin <- "2023-03-01"
richesse_us <- df_richesse(df, date_debut, date_fin, 1)
data_us_new_var$wealth <- richesse_us["totale"]

data_us_new_var <- data_us_new_var %>%
  mutate(delta_W = (wealth - lag(wealth))/lag(conso))

model <- lm(epsilon ~ delta_W + taux_ct + spread + income_growth, data = data_us_new_var, na.action = na.omit)

alpha_w <- coef(model)["delta_W"]

return(alpha_w)

}