plot_richesse <- function(df, pays) {

  if (pays == "France") {
    yleg <- "Valeur en Millions d'euros (base 2015)"
  } else if (pays == "US") {
    yleg <- "Valeur en Millions de dollars"
  }

  dates <- as.Date(rownames(df))

  y_min <- min(c(df$immobilier, df$financier))
  y_max <- max(c(df$immobilier, df$financier))

  plot(dates, df$immobilier, type = "l",
       main = paste("Evolution de la richesse - ", pays),
       xlab = "Années",
       ylab = yleg,
       ylim = c(y_min, y_max))
  lines(dates, df$financier, type = "l", lty = "dashed")

  legend("bottomright", legend = c("Patrimoine immobilier", "Patrimoine financier"),
         lty = c("solid", "dashed"))

}

plot_consommation <- function(df, pays) {

  if (pays == "France") {
    yleg <- "Valeur en Millions d'euros (base 2015)"
  } else if (pays == "US") {
    yleg <- "Valeur en Millions de dollars"
  }

  dates <- as.Date(rownames(df))

  par(mfrow = c(2, 1))
  plot(dates, df$conso, type = "l",
       main = paste("Evolution de la consommation - ", pays),
       xlab = "Années",
       ylab = yleg,
       ylim = c(min(df$conso), max(df$conso)))

  plot(dates, df$var_log_conso, type = "l",
       main = paste("Variation du log de la consommation - ", pays),
       xlab = "Années",
       ylab = yleg,
       ylim = c(min(df$var_log_conso), max(df$var_log_conso)))

  par(mfrow = c(1, 1))

}

visualisation_controles <- function(variables_controle) {
  # Ensure that the 'date' column is of Date type
  if (!inherits(variables_controle$date, "Date")) {
    variables_controle$date <- as.Date(as.character(variables_controle$date))
  }
  
  # Save current graphical parameters
  op <- par(no.readonly = TRUE)
  
  # Set up a 2-row x 3-column plotting layout
  par(mfrow = c(2, 3))
  
  # Loop through each control variable (skip the first column which is "date")
  for (i in 2:ncol(variables_controle)) {
    message("Plotting: ", names(variables_controle)[i])
    # Compute y-axis limits for the current variable, ignoring NA values
    y_min <- min(variables_controle[[i]], na.rm = TRUE)
    y_max <  # Create the plot
    plot(variables_controle$date,
         variables_controle[[i]],
         type = "l",
         main = names(variables_controle)[i],
         xlab = "Date",
         ylab = names(variables_controle)[i],
         ylim = c(y_min, y_max))
  }
  
  # Restore original graphical parameters
  par(op)
}
- max(variables_controle[[i]], na.rm = TRUE)
