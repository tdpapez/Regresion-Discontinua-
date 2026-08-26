# Librerias necesarias
install.packages("haven")
install.packages("rdrobust")
install.packages("rddensity")
install.packages("gt")
install.packages("webshot2")

library(haven)
library(rdrobust)
library(rddensity)
library(gt)
library(webshot2)

datos <- read_dta("headstart.dta")

# Histograma

cutoff <- 59.1984

hist(datos$povrate60,
     breaks = 40,
     main = "Distribución de la tasa de pobreza en 1960",
     xlab = "Tasa de pobreza (povrate60)",
     ylab = "Frecuencia",
     col = "gray80",
     border = "white")

abline(v = cutoff, col = "red", lwd = 2, lty = 2)

legend("topright",
       legend = paste("Cutoff OEO =", cutoff),
       col = "red", lty = 2, lwd = 2, bty = "n")

# Estimacion del efecto de HS sobre mortalidad infantil

cutoff <- 59.1984

y <- datos$mort_age59_related_postHS
x <- datos$povrate60

covs <- cbind(
  datos$census1960_pop,
  datos$census1960_pcturban,
  datos$census1960_pctblack
)

rd_resultado <- rdrobust(
  y      = y,
  x      = x,
  c      = cutoff,
  p      = 1,              
  kernel = "triangular",
  covs   = covs,
  vce    = "hc1"            
)

summary(rd_resultado)


tabla_rd <- data.frame(
  Estimador = c("Convencional", "Bias-Corrected", "Robust"),
  Coeficiente = round(rd_resultado$coef[, 1], 3),
  Error_Estandar = round(rd_resultado$se[, 1], 3),
  Z = round(rd_resultado$z[, 1], 3),
  P_Valor = round(rd_resultado$pv[, 1], 3)
)

bw_izq <- round(rd_resultado$bws[1, 1], 2)
bw_der <- round(rd_resultado$bws[1, 2], 2)
n_izq <- rd_resultado$N_h[1]
n_der <- rd_resultado$N_h[2]

tabla_gt <- gt(tabla_rd)

tabla_gt <- tab_header(
  tabla_gt,
  title = "Efecto de Head Start sobre la mortalidad infantil",
  subtitle = paste0(
    "Variable dependiente: mort_age59_related_postHS | Cutoff: ", cutoff,
    " | BW izq/der: ", bw_izq, "/", bw_der,
    " | N efectivo izq/der: ", n_izq, "/", n_der
  )
)

tabla_gt <- cols_label(
  tabla_gt,
  Estimador = "Estimador",
  Coeficiente = "Coeficiente",
  Error_Estandar = "Error Estándar",
  Z = "Estadístico Z",
  P_Valor = "p-valor"
)

tabla_gt <- tab_source_note(
  tabla_gt,
  source_note = "Polinomio local grado 1, kernel triangular, errores robustos (HC1). Controles: población, % urbano y % población negra (Censo 1960)."
)

tabla_gt <- tab_style(
  tabla_gt,
  style = cell_text(weight = "bold"),
  locations = cells_body(rows = Estimador == "Robust")
)
gtsave(tabla_gt, "tabla_rd_mortalidad.png")

# Rdplot

png("rdplot_mortalidad.png", width = 1600, height = 1200, res = 200)

rdplot(
  y = y,
  x = x,
  c = cutoff,
  p = 1,                        
  kernel = "triangular",
  x.label = "Tasa de pobreza en 1960 (povrate60)",
  y.label = "Mortalidad relacionada con HS, edades 5-9 (1973-1983)",
  title = "Discontinuidad en mortalidad infantil en el cutoff de OEO"
)

dev.off()

# Test de densidad

test_densidad <- rddensity(X = x, c = cutoff)

summary(test_densidad)

png("rddensity_plot.png", width = 1600, height = 1200, res = 200)

rdplotdensity(
  rdd = test_densidad,
  X = x,
  type = "both",              
  xlabel = "Tasa de pobreza en 1960 (povrate60)",
  ylabel = "Densidad estimada"
)

dev.off()

# Continuidad en covariables

covariables <- c(
  "census1960_pop",
  "census1960_pcturban",
  "census1960_pctblack",
  "census1960_pctsch1417",
  "census1960_pctsch534",
  "census1960_pctsch25plus"
)

resultados_balance <- data.frame(
  Covariable = character(),
  Coeficiente = numeric(),
  Error_Estandar = numeric(),
  Z = numeric(),
  P_Valor = numeric(),
  BW_Izq = numeric(),
  BW_Der = numeric(),
  stringsAsFactors = FALSE
)

for (var in covariables) {
  
  y_temp <- datos[[var]]
  
  rd_temp <- rdrobust(
    y      = y_temp,
    x      = x,
    c      = cutoff,
    p      = 1,
    kernel = "triangular",
    vce    = "hc1"
  )
  
  fila <- data.frame(
    Covariable = var,
    Coeficiente = round(rd_temp$coef[3, 1], 3),      
    Error_Estandar = round(rd_temp$se[3, 1], 3),
    Z = round(rd_temp$z[3, 1], 3),
    P_Valor = round(rd_temp$pv[3, 1], 3),
    BW_Izq = round(rd_temp$bws[1, 1], 2),
    BW_Der = round(rd_temp$bws[1, 2], 2)
  )
  
  resultados_balance <- rbind(resultados_balance, fila)
}

print(resultados_balance)

tabla_balance <- gt(resultados_balance)

tabla_balance <- tab_header(
  tabla_balance,
  title = "Test de balance en covariables predeterminadas (Censo 1960)",
  subtitle = paste0("Cutoff: ", cutoff, " | Polinomio local grado 1, kernel triangular, errores HC1")
)

tabla_balance <- cols_label(
  tabla_balance,
  Covariable = "Covariable",
  Coeficiente = "Coeficiente",
  Error_Estandar = "Error Estándar",
  Z = "Z",
  P_Valor = "p-valor",
  BW_Izq = "BW Izq.",
  BW_Der = "BW Der."
)

tabla_balance <- tab_style(
  tabla_balance,
  style = cell_fill(color = "#ffcccc"),
  locations = cells_body(rows = P_Valor < 0.10)   # resalta si hay señal de desbalance
)

tabla_balance <- tab_source_note(
  tabla_balance,
  source_note = "Estimador Robust (bias-corrected). Cada fila es una regresión RD separada usando la covariable como variable dependiente."
)

gtsave(tabla_balance, "tabla_balance_covariables.png")

# Gráficos de continuidad por covariable

for (var in covariables) {
  
  png(paste0("rdplot_balance_", var, ".png"), width = 1400, height = 1000, res = 200)
  
  rdplot(
    y = datos[[var]],
    x = x,
    c = cutoff,
    p = 1,
    kernel = "triangular",
    x.label = "Tasa de pobreza en 1960 (povrate60)",
    y.label = var,
    title = paste("Continuidad en", var)
  )
  
  dev.off()
}


# Placebo con cutoff falso mas bajo que el verdader

datos_control <- datos[datos$povrate60 < cutoff, ]

pseudo_cutoff <- 45

rd_placebo <- rdrobust(
  y      = datos_control$mort_age59_related_postHS,
  x      = datos_control$povrate60,
  c      = pseudo_cutoff,
  p      = 1,
  kernel = "triangular",
  vce    = "hc1"
)

summary(rd_placebo)

tabla_placebo <- data.frame(
  Pseudo_Cutoff = pseudo_cutoff,
  Coeficiente = round(rd_placebo$coef[3, 1], 3),
  Error_Estandar = round(rd_placebo$se[3, 1], 3),
  Z = round(rd_placebo$z[3, 1], 3),
  P_Valor = round(rd_placebo$pv[3, 1], 3)
)

tabla_placebo_gt <- gt(tabla_placebo)

tabla_placebo_gt <- tab_header(
  tabla_placebo_gt,
  title = "Placebo test: pseudo-cutoff en la submuestra de control",
  subtitle = "Variable dependiente: mort_age59_related_postHS | Polinomio grado 1, kernel triangular, errores HC1"
)

tabla_placebo_gt <- cols_label(
  tabla_placebo_gt,
  Pseudo_Cutoff = "Pseudo-Cutoff",
  Coeficiente = "Coeficiente",
  Error_Estandar = "Error Estándar",
  Z = "Z",
  P_Valor = "p-valor"
)

tabla_placebo_gt <- tab_source_note(
  tabla_placebo_gt,
  source_note = "Cutoff verdadero: 59.1984. El pseudo-cutoff se testea dentro de la submuestra de control (povrate60 < 59.1984) para no capturar el salto real."
)

gtsave(tabla_placebo_gt, "tabla_placebo_cutoff.png")

# Placebo con variable mort_age59_injury_postHS
rd_placebo_injury <- rdrobust(
  y      = datos$mort_age59_injury_postHS,
  x      = x,
  c      = cutoff,
  p      = 1,
  kernel = "triangular",
  vce    = "hc1"
)

summary(rd_placebo_injury)

tabla_placebo_injury <- data.frame(
  Estimador = c("Convencional", "Bias-Corrected", "Robust"),
  Coeficiente = round(rd_placebo_injury$coef[, 1], 3),
  Error_Estandar = round(rd_placebo_injury$se[, 1], 3),
  Z = round(rd_placebo_injury$z[, 1], 3),
  P_Valor = round(rd_placebo_injury$pv[, 1], 3)
)

tabla_placebo_injury_gt <- gt(tabla_placebo_injury)

tabla_placebo_injury_gt <- tab_header(
  tabla_placebo_injury_gt,
  title = "Placebo de causa: mortalidad NO relacionada con HS",
  subtitle = "Variable dependiente: mort_age59_injury_postHS (edades 5-9, 1973-1983) | Cutoff: 59.1984"
)

tabla_placebo_injury_gt <- cols_label(
  tabla_placebo_injury_gt,
  Estimador = "Estimador",
  Coeficiente = "Coeficiente",
  Error_Estandar = "Error Estándar",
  Z = "Estadístico Z",
  P_Valor = "p-valor"
)

tabla_placebo_injury_gt <- tab_style(
  tabla_placebo_injury_gt,
  style = cell_text(weight = "bold"),
  locations = cells_body(rows = Estimador == "Robust")
)

tabla_placebo_injury_gt <- tab_source_note(
  tabla_placebo_injury_gt,
  source_note = "Polinomio local grado 1, kernel triangular, errores robustos (HC1). Causa de muerte NO afectada por los servicios de Head Start."
)

gtsave(tabla_placebo_injury_gt, "tabla_placebo_injury.png")

png("rdplot_placebo_injury.png", width = 1600, height = 1200, res = 200)

rdplot(
  y = datos$mort_age59_injury_postHS,
  x = x,
  c = cutoff,
  p = 1,
  kernel = "triangular",
  x.label = "Tasa de pobreza en 1960 (povrate60)",
  y.label = "Mortalidad por injuries, edades 5-9 (1973-1983)",
  title = "Placebo: mortalidad por injuries (no relacionada con HS)"
)

dev.off()

# Placebo mortalidad +25: tabla de resultads y rdplot

rd_placebo_adultos <- rdrobust(
  y      = datos$mort_age25plus_related_postHS,
  x      = x,
  c      = cutoff,
  p      = 1,
  kernel = "triangular",
  vce    = "hc1"
)

summary(rd_placebo_adultos)

tabla_placebo_adultos <- data.frame(
  Estimador = c("Convencional", "Bias-Corrected", "Robust"),
  Coeficiente = round(rd_placebo_adultos$coef[, 1], 3),
  Error_Estandar = round(rd_placebo_adultos$se[, 1], 3),
  Z = round(rd_placebo_adultos$z[, 1], 3),
  P_Valor = round(rd_placebo_adultos$pv[, 1], 3)
)

tabla_placebo_adultos_gt <- gt(tabla_placebo_adultos)

tabla_placebo_adultos_gt <- tab_header(
  tabla_placebo_adultos_gt,
  title = "Placebo de cohorte: adultos 25+ (mortalidad relacionada con HS)",
  subtitle = "Variable dependiente: mort_age25plus_related_postHS (1973-1983) | Cutoff: 59.1984"
)

tabla_placebo_adultos_gt <- cols_label(
  tabla_placebo_adultos_gt,
  Estimador = "Estimador",
  Coeficiente = "Coeficiente",
  Error_Estandar = "Error Estándar",
  Z = "Estadístico Z",
  P_Valor = "p-valor"
)

tabla_placebo_adultos_gt <- tab_style(
  tabla_placebo_adultos_gt,
  style = cell_text(weight = "bold"),
  locations = cells_body(rows = Estimador == "Robust")
)

tabla_placebo_adultos_gt <- tab_source_note(
  tabla_placebo_adultos_gt,
  source_note = "Polinomio local grado 1, kernel triangular, errores robustos (HC1). Cohorte demasiado grande para haber sido elegible a Head Start."
)

gtsave(tabla_placebo_adultos_gt, "tabla_placebo_adultos.png")


png("rdplot_placebo_adultos.png", width = 1600, height = 1200, res = 200)

rdplot(
  y = datos$mort_age25plus_related_postHS,
  x = x,
  c = cutoff,
  p = 1,
  kernel = "triangular",
  x.label = "Tasa de pobreza en 1960 (povrate60)",
  y.label = "Mortalidad relacionada con HS, edades 25+ (1973-1983)",
  title = "Placebo: mortalidad en adultos 25+ (relacionada con HS)"
)

dev.off()