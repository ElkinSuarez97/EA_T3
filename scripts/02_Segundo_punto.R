# =============================================================================
# Script_02: Ejercicio 2
# =============================================================================
# -----------------------------------------------------------------------------
# 0. Cargar bases de datos 
# -----------------------------------------------------------------------------

# Cargar configuración base

source(here::here("scripts", "00_Config.R"))

female_politics <- import(here::here("data", "female_politics.dta"))

# -----------------------------------------------------------------------------
# 1. Exploración inicial de los datos y graficar variable de focalización
# -----------------------------------------------------------------------------

##Variables de interés:

##a. primary_winner: Indicadora de si la candidata ganó o no la consulta interna.
##margen de victoria o derrota en la elección primaria (PrimMargin > 0) Indica victoria
##GenWin = 1  Gana las elecciones generales


##3) a. Genere un gráfico de la variable de focalización frente a la variable de interés:

##Variable de focalización: PrimMargin.
##Variable de interés:      GenWin

png(filename = store_file("rdplot.png"), width = 800, height = 600)

rdplot(y = female_politics$GenWin, x = female_politics$PrimMargin, c = 0, p = 2,
       x.label = "Margen de victoria en elección primaria (Primmargin)",
       y.label = "Probabilidad de ganar elección general (GenWin)",
       title = "Victoria en las elecciones primarias")

dev.off()


########################2) Estimaciones######################################

########a. Regresión lineal local #####

unique(female_politics$party)

##<labelled<double>[1]>
##[1] 1

#Labels:
#  value label
#1     R
#2     D

##IMPORTANTE: Como puede verificarse, la base de datos sólo tiene candidatas y 
##            candidatos del Partido Republicano y a esto debe ceñirse la inter
##            pretación.


# Modelo (1): sin controles:

m_quad_1 <- lm(
  GenWin ~ primary_winner +
    PrimMargin + I(PrimMargin^2) +
    primary_winner:PrimMargin +
    primary_winner:I(PrimMargin^2),
  data = female_politics
)

# Modelo (2): con controles
m_quad_2 <- lm(
  GenWin ~ primary_winner +
    PrimMargin + I(PrimMargin^2) +
    primary_winner:PrimMargin +
    primary_winner:I(PrimMargin^2) +
    prez + PrezYear + inc +
    Y82to90 + Y92to00 + Y02to10,
  data = female_politics
)

##Colocar en una tabla elegante tipo stargazer:


stargazer(
  m_quad_1, m_quad_2,
  type = "latex",
  title = "Probabilidad de ganar la elección general si la candidata nominada es mujer",
  dep.var.labels = "General election win",
  omit.stat = c("f", "ser", "adj.rsq"),
  no.space = TRUE,
  out = store_file("tabla_republicanos.tex")
)

#######################b. Regresiones locales####################################

# Se diseña una función para correr la regresión local lineal dentro de un 
# bandwidth h:

#Esta función tiene un kernel triangular que pondera por cercanía al punto de
#corte. 

##Función sin controles: 

run_ll_nocontrols <- function(h) {
  data <- subset(female_politics, abs(PrimMargin) <= h)
  data$w <- 1 - abs(data$PrimMargin) / h
  
  lm(
    GenWin ~ primary_winner + PrimMargin + primary_winner:PrimMargin,
    data = data,
    weights = w
  )
}

#Función con controles:

run_ll_controls <- function(h) {
  data <- subset(female_politics, abs(PrimMargin) <= h)
  data$w <- 1 - abs(data$PrimMargin) / h
  
  lm(
    GenWin ~ primary_winner + PrimMargin + prez + PrezYear + inc +
      Y82to90 + Y92to00 + Y02to10 +
      primary_winner:PrimMargin,
    data = data,
    weights = w
  )
}

m_nc_015 <- run_ll_nocontrols(0.15)
m_nc_010 <- run_ll_nocontrols(0.10)
m_nc_005 <- run_ll_nocontrols(0.05)

m_c_015  <- run_ll_controls(0.15)
m_c_010  <- run_ll_controls(0.10)
m_c_005  <- run_ll_controls(0.05)


###Tabla sin controles ###

stargazer(
  m_nc_015, m_nc_010, m_nc_005,
  type = "latex",
  title = "Estimaciones RD lineales locales sin controles",
  dep.var.labels = "Victoria en la elección general (1 = gana)",
  column.labels = c("BW 0.15", "BW 0.10", "BW 0.05"),
  covariate.labels = c(
    "Tratamiento",
    "Margen",
    "D $\\times$ Margen",
    "Constante"
  ),
  omit.stat = c("f", "ser", "adj.rsq"),
  model.numbers = FALSE,
  no.space = TRUE,
  digits = 3,
  notes = c(
    "Errores estándar robustos entre paréntesis.",
    "Las estimaciones corresponden a regresiones lineales locales estimadas mediante OLS ponderado con kernel triangular.",
    "La muestra en cada columna se restringe a observaciones con $|PrimMargin| \\leq h$.",
    "$^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$."
  ),
  notes.append = FALSE,
  notes.align = "l",
  header = FALSE
)


############Tabla con controles#################

stargazer(
  m_c_015, m_c_010, m_c_005,
  type = "latex",
  out = "tabla_rd_con_controles.tex",
  
  title = "",
  dep.var.labels = "",
  dep.var.caption = "",
  model.numbers = FALSE,
  header = FALSE,
  no.space = TRUE,
  digits = 2,
  
  column.labels = c("BW 0.15", "BW 0.10", "BW 0.05"),
  
  covariate.labels = c(
    "Mujer gana la primaria (1 = sí)",
    "Margen en la primaria",
    "Voto presidencial previo",
    "Año presidencial (1 = sí)",
    "Candidato incumbente (1 = sí)",
    "Periodo 1982--1990",
    "Periodo 1992--2000",
    "Periodo 2002--2010",
    "Mujer gana la primaria $\\times$ Margen",
    "Constante"
  ),
  
  omit.stat = c("f", "ser", "adj.rsq"),
  
  notes = c(
    "Errores estándar entre paréntesis.",
    "Las estimaciones corresponden a regresiones lineales locales estimadas mediante OLS ponderado con kernel triangular.",
    "La muestra en cada columna se restringe a observaciones con $|PrimMargin| \\leq h$.",
    "Todas las especificaciones incluyen controles por voto presidencial previo, año presidencial, incumbencia y efectos fijos de periodo electoral.",
    "$^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$."
  ),
  notes.append = FALSE,
  notes.align = "l"
)







