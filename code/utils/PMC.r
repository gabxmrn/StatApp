
#data_us_new_var n'est pas que pour les US, c'est une ancienne notation
#probablement encore un problème dans les données de richesse us
#et aussi un peut etre un problème de date dans la fusion de data_us_new_var et richesse_us

PMC <- function(df,by){

source("code/utils/chi.R")
library(dplyr)


#df des nouvelles variables
data_us_new_var <- df[seq(1, nrow(df), by = by),]

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
epsilon <- chi(df,by)["residuals"]

#obtenir delta W_t
date_debut <- "1998-12-01"
date_fin <- "2023-03-01"
richesse_us <- df_richesse(df, date_debut, date_fin, 1)
data_us_new_var$wealth <- richesse_us[seq(1, nrow(df), by = by),"totale"]
data_us_new_var$epsilon <- epsilon$residuals
data_us_new_var <- data_us_new_var %>%
  mutate(delta_W = (wealth - lag(wealth))/lag(conso))

print(head(data_us_new_var))

#première régression dite pas efficace par Slacalek
model <- lm(epsilon ~ delta_W + diff_taux_ct + spread + income_growth, data = data_us_new_var, na.action = na.omit)

alpha_w <- coef(model)["delta_W"]
#calcul avec chi = 0.6
PMC_ev <- alpha_w / 0.24

#obtention des nouvelles variables pour la 2eme méthode
# Calcul de ΔW = Wt - Wt-1
data_us_new_var <- data_us_new_var %>%
  mutate(Delta_W = wealth - lag(wealth, 1),
  Delta_log_conso = log(conso) - lag(log(conso),1))

# Définir chi
chi <- 0.6  # Remplacer par la valeur réelle de χ

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
  mutate(delta_C = (conso - lag(conso))/lag(conso,5)) %>%
  mutate(across(c(delta_barre_W,chomage, diff_taux_ct,spread ,income_growth), ~ lag(.x, n = 1), .names = "lag1_{.col}"))

#régression de la p.20
model_2 <- lm(Delta_log_conso ~ lag1_delta_barre_W + lag1_diff_taux_ct + lag1_spread + lag1_income_growth, data = data_us_new_var, na.action = na.omit)
alpha_w <- coef(model_2)["lag1_delta_barre_W"]

print(summary(model_2))

#calcul de la PMC eventual avec chi = 0.6 chi(1-chi) = 0.24
PMC_ev <- alpha_w / 0.24

#Comme la conso est en k$ et les assets en M$,
#On divise par 10 pour avoir l'unité de Slacalek (en %)
PMC_ev <- PMC_ev / 10

return(PMC_ev)

}