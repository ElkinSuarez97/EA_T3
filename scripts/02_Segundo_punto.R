# =============================================================================
# Script_02: Ejercicio 2
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Cargar bases de datos
# -----------------------------------------------------------------------------

source(here::here("scripts", "00_Config.R"))

card_data <- import(here::here("data", "Card", "Card.dta"))
card_data <- as.data.frame(card_data)

names(card_data) <- tolower(trimws(names(card_data)))

# -----------------------------------------------------------------------------
# a. Variables del ejercicio
# -----------------------------------------------------------------------------

controles <- "nres80 + ires80 + logsize80 + logsize90 + coll80 + coll90 + mfg80 + mfg90"

# -----------------------------------------------------------------------------
# c. Estimaciones MCO y MC2E
# -----------------------------------------------------------------------------

mco_hs <- lm(
  as.formula(paste("resgap2 ~ relshs +", controles)),
  data    = card_data,
  weights = count90,
  model = TRUE, x = TRUE, y = TRUE
)

iv_hs <- ivreg::ivreg(
  as.formula(paste("resgap2 ~ relshs +", controles, "| hsiv +", controles)),
  data    = card_data,
  weights = count90,
  model = TRUE, x = TRUE, y = TRUE
)

mco_col <- lm(
  as.formula(paste("resgap4 ~ relscoll +", controles)),
  data    = card_data,
  weights = count90,
  model = TRUE, x = TRUE, y = TRUE
)

iv_col <- ivreg::ivreg(
  as.formula(paste("resgap4 ~ relscoll +", controles, "| colliv +", controles)),
  data    = card_data,
  weights = count90,
  model = TRUE, x = TRUE, y = TRUE
)

# -----------------------------------------------------------------------------
# Primera etapa y estadístico F
# -----------------------------------------------------------------------------

fs_hs <- lm(
  as.formula(paste("relshs ~ hsiv +", controles)),
  data    = card_data,
  weights = count90
)

fs_col <- lm(
  as.formula(paste("relscoll ~ colliv +", controles)),
  data    = card_data,
  weights = count90
)

get_F <- function(model, instrument) {
  ct      <- lmtest::coeftest(model, vcov. = sandwich::vcovHC(model, type = "HC1"))
  t_value <- ct[instrument, "t value"]
  F_value <- as.numeric(t_value^2)
  p_value <- pf(F_value, df1 = 1, df2 = df.residual(model), lower.tail = FALSE)
  c(F = F_value, p = p_value)
}

F_hs_info  <- get_F(fs_hs,  "hsiv")
F_col_info <- get_F(fs_col, "colliv")

F_hs   <- F_hs_info["F"]
pF_hs  <- F_hs_info["p"]
F_col  <- F_col_info["F"]
pF_col <- F_col_info["p"]

# -----------------------------------------------------------------------------
# Funciones auxiliares
# -----------------------------------------------------------------------------

stars <- function(p) {
  ifelse(p < 0.01, "***",
         ifelse(p < 0.05, "**",
                ifelse(p < 0.10, "*", "")))
}

coef_rob <- function(model, variable) {
  ct   <- lmtest::coeftest(model, vcov. = sandwich::vcovHC(model, type = "HC1"))
  pval <- ct[variable, ncol(ct)]
  c(
    coef = paste0(sprintf("%.3f", ct[variable, "Estimate"]), stars(pval)),
    se   = paste0("(", sprintf("%.3f", ct[variable, "Std. Error"]), ")")
  )
}

r2 <- function(model) sprintf("%.3f", summary(model)$r.squared)

ar2 <- function(model) sprintf("%.3f", summary(model)$adj.r.squared)

# -----------------------------------------------------------------------------
# Tabla principal: MCO y MC2E
# -----------------------------------------------------------------------------

b_mco_hs  <- coef_rob(mco_hs,  "relshs")
b_iv_hs   <- coef_rob(iv_hs,   "relshs")
b_mco_col <- coef_rob(mco_col, "relscoll")
b_iv_col  <- coef_rob(iv_col,  "relscoll")

tabla_principal <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Estimaciones MCO y MC2E: inmigración y brecha salarial}",
  "\\begin{tabular}{lcccc}",
  "\\hline\\hline",
  " & \\multicolumn{2}{c}{Secundaria} & \\multicolumn{2}{c}{Universidad} \\\\",
  "\\cline{2-3} \\cline{4-5}",
  " & MCO & MC2E & MCO & MC2E \\\\",
  "\\hline",
  paste0(
    "Oferta relativa inmigrantes/nativos (log) & ",
    b_mco_hs["coef"], " & ", b_iv_hs["coef"], " & ",
    b_mco_col["coef"], " & ", b_iv_col["coef"], " \\\\"
  ),
  paste0(
    " & ", b_mco_hs["se"], " & ", b_iv_hs["se"], " & ",
    b_mco_col["se"], " & ", b_iv_col["se"], " \\\\"
  ),
  "\\hline",
  paste0(
    "Observaciones & ",
    nobs(mco_hs), " & ", nobs(iv_hs), " & ",
    nobs(mco_col), " & ", nobs(iv_col), " \\\\"
  ),
  paste0(
    "$R^{2}$ & ",
    r2(mco_hs), " & ", r2(iv_hs), " & ",
    r2(mco_col), " & ", r2(iv_col), " \\\\"
  ),
  paste0(
    "$R^{2}$ ajustado & ",
    ar2(mco_hs), " & ", ar2(iv_hs), " & ",
    ar2(mco_col), " & ", ar2(iv_col), " \\\\"
  ),
  "Controles & Sí & Sí & Sí & Sí \\\\",
  "Pesos & Población 1990 & Población 1990 & Población 1990 & Población 1990 \\\\",
  paste0(
    "F primera etapa & -- & ", sprintf("%.2f", F_hs),
    " & -- & ", sprintf("%.2f", F_col), " \\\\"
  ),
  paste0(
    "$p$-valor F & -- & ",
    ifelse(pF_hs  < 0.001, "$<0.001$", sprintf("%.3f", pF_hs)),
    " & -- & ",
    ifelse(pF_col < 0.001, "$<0.001$", sprintf("%.3f", pF_col)),
    " \\\\"
  ),
  "\\hline\\hline",
  "\\end{tabular}",
  "",
  "\\vspace{0.3em}",
  "",
  "\\parbox{0.95\\textwidth}{\\footnotesize",
  "\\textit{Nota:} La variable dependiente es la brecha salarial residual en logaritmos entre inmigrantes y nativos. Para secundaria se usa \\texttt{resgap2} y para universidad \\texttt{resgap4}. En MC2E, \\texttt{relshs} se instrumenta con \\texttt{hsiv} y \\texttt{relscoll} con \\texttt{colliv}. Todas las especificaciones incluyen controles por residuos salariales de nativos e inmigrantes en 1980, logaritmo de población en 1980 y 1990, proporción universitaria en 1980 y 1990, y participación manufacturera en 1980 y 1990. Errores estándar robustos HC1 entre paréntesis. $^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$.",
  "}",
  "\\end{table}"
)

writeLines(tabla_principal, store_file("tabla_principal_card.tex"))

# -----------------------------------------------------------------------------
# d. Tabla de balance / validación Bartik
# -----------------------------------------------------------------------------

dep_balance <- c("shric1", "shric2", "shric5", "shric6",
                 "shric7", "shric31", "hsiv", "colliv")

x_balance <- c("logsize80", "coll80", "nres80", "ires80", "mfg80")

balance_col <- function(yvar) {
  data_tmp <- card_data
  if (grepl("^shric", yvar)) {
    data_tmp$y_esc <- data_tmp[[yvar]] * 10000000
  } else {
    data_tmp$y_esc <- data_tmp[[yvar]]
  }
  m  <- lm(y_esc ~ logsize80 + coll80 + nres80 + ires80 + mfg80,
           data = data_tmp, weights = count90)
  ct <- lmtest::coeftest(m, vcov. = sandwich::vcovHC(m, type = "HC1"))
  salida <- c()
  for (x in x_balance) {
    salida <- c(
      salida,
      paste0(sprintf("%.3f", ct[x, "Estimate"]), stars(ct[x, ncol(ct)])),
      paste0("(", sprintf("%.3f", ct[x, "Std. Error"]), ")")
    )
  }
  c(salida, sprintf("%.3f", summary(m)$r.squared), nobs(m))
}

res_balance <- sapply(dep_balance, balance_col)

fila_balance <- function(nombre, pos) {
  paste0(nombre, " & ", paste(res_balance[pos, ], collapse = " & "), " \\\\")
}

tabla_balance <- c(
  "\\scalebox{0.6}{",
  "\\begin{tabular}{l*{8}{c}} \\toprule",
  " & \\multicolumn{1}{c}{Fracción de inm.} & \\multicolumn{1}{c}{Fracción de inm.} & \\multicolumn{1}{c}{Fracción de inm.} & \\multicolumn{1}{c}{Fracción de inm.} & \\multicolumn{1}{c}{Fracción de inm.} & \\multicolumn{1}{c}{Fracción de inmigr.} & \\multicolumn{1}{c}{Bartik para} & \\multicolumn{1}{c}{Bartik para} \\\\",
  " & \\multicolumn{1}{c}{de México} & \\multicolumn{1}{c}{de Filipinas} & \\multicolumn{1}{c}{de El Salvador} & \\multicolumn{1}{c}{de China} & \\multicolumn{1}{c}{de Cuba} & \\multicolumn{1}{c}{de Europa occ. + Otros} & \\multicolumn{1}{c}{secundaria} & \\multicolumn{1}{c}{universidad} \\\\",
  " & \\multicolumn{1}{c}{1980} & \\multicolumn{1}{c}{1980} & \\multicolumn{1}{c}{1980} & \\multicolumn{1}{c}{1980} & \\multicolumn{1}{c}{1980} & \\multicolumn{1}{c}{1980} & \\multicolumn{1}{c}{} & \\multicolumn{1}{c}{} \\\\",
  "\\midrule",
  fila_balance("Log población 1980",       1),
  fila_balance("",                          2),
  fila_balance("Fracción univ. 1980",       3),
  fila_balance("",                          4),
  fila_balance("Salario res. nativo 1980",  5),
  fila_balance("",                          6),
  fila_balance("Salario res. inm. 1980",    7),
  fila_balance("",                          8),
  fila_balance("Fracción ind. manuf. 1980", 9),
  fila_balance("",                          10),
  "\\midrule",
  fila_balance("$R^2$", 11),
  fila_balance("$N$",   12),
  "\\bottomrule",
  "\\multicolumn{9}{c}{Errores estándar robustos a heterocedasticidad en paréntesis}\\\\",
  "\\multicolumn{9}{c}{Columnas de shares reescaladas por 10,000,000; columnas Bartik sin reescalar}\\\\",
  "\\end{tabular}",
  "}"
)

writeLines(tabla_balance, store_file("tabla_balance_card.tex"))

# -----------------------------------------------------------------------------
# Guardar modelos
# -----------------------------------------------------------------------------

saveRDS(
  list(
    mco_hs  = mco_hs,
    iv_hs   = iv_hs,
    mco_col = mco_col,
    iv_col  = iv_col,
    fs_hs   = fs_hs,
    fs_col  = fs_col,
    F_hs    = F_hs,
    pF_hs   = pF_hs,
    F_col   = F_col,
    pF_col  = pF_col
  ),
  store_file("modelos_card.rds")
)