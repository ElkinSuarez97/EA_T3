# =============================================================================
# Script_03: Ejercicio 3
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Cargar bases de datos 
# -----------------------------------------------------------------------------

source(here::here("scripts", "00_Config.R"))

cuba   <- import(here::here("data", "Cuba_RC.dta"))

# -----------------------------------------------------------------------------
# a. Gráficas
# -----------------------------------------------------------------------------

l_paises  <- c("cuba", "honduras", "peru", "elsalvador", "costarica", "uruguay", "venezuela")
o_paises  <- c("cuba", "sweden", "belgium", "germany", "italy", "unitedstates")

paises <- c(
  "cuba"          = "Cuba",
  "honduras"      = "Honduras",
  "peru"          = "Perú",
  "elsalvador"    = "El Salvador",
  "costarica"     = "Costa Rica",
  "uruguay"       = "Uruguay",
  "venezuela"     = "Venezuela",
  "sweden"        = "Suecia",
  "belgium"       = "Bélgica",
  "germany"       = "Alemania",
  "italy"         = "Italia",
  "unitedstates"  = "Estados Unidos"
)

grafica_imr <- function(datos, paises_sel, titulo) {
  
  df <- datos %>%
    filter(country %in% paises_sel,
           year >= 1950, year <= 1975) %>%
    mutate(
      es_cuba = (country == "cuba"),
      nombre  = dplyr::recode(country, !!!paises)
    )
  
  ggplot(df, aes(x = year, y = imr,
                 group = nombre,
                 color = nombre,
                 linewidth = es_cuba)) +
    
    geom_line() +
    
    geom_vline(xintercept = 1959,
               linetype = "dashed",
               color = "black") +
    
    scale_color_manual(
      values = c(
        "Cuba"           = "black",
        "Honduras"       = "#1a5276",
        "Perú"           = "#2e86c1",
        "El Salvador"    = "#5dade2",
        "Costa Rica"     = "#85c1e9",
        "Uruguay"        = "#aed6f1",
        "Venezuela"      = "#d6eaf8",
        "Alemania"       = "#1a5276",
        "Italia"         = "#2e86c1",
        "Estados Unidos" = "#5dade2",
        "Bélgica"        = "#85c1e9",
        "Suecia"         = "#aed6f1"
      ),
      name = NULL
    ) +
    
    scale_linewidth_manual(
      values = c("TRUE" = 1.2, "FALSE" = 0.6),
      guide  = "none"
    ) +
    
    scale_x_continuous(
      breaks = seq(1950, 1975, 5)
    ) +
    
    guides(
      color = guide_legend(nrow = 2, byrow = TRUE)
    ) +
    
    labs(
      title   = titulo,
      x       = "Año",
      y       = "Mortalidad infantil (por 1,000 habitantes)",
      caption = "-- Revolución Cubana (1959)"
    ) +
    
    theme_minimal(base_size = 12) +
    
    theme(
      plot.title       = element_text(hjust = 0.5, face = "bold"),
      plot.caption     = element_text(hjust = 0, size = 9),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      legend.justification = "center",
      legend.box       = "horizontal",
      legend.text      = element_text(size = 11),
      legend.spacing.x = unit(0.3, "cm"),
      legend.spacing.y = unit(0.05, "cm"),
      legend.key.width  = unit(1.2, "cm"),
      legend.key.height = unit(0.4, "cm")
    )
}

g1 <- cuba %>%
  filter(country == "cuba",
         year >= 1950, year <= 1975) %>%
  ggplot(aes(x = year, y = imr)) +
  geom_line(color = "black", linewidth = 1.2) +
  geom_vline(xintercept = 1959, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = seq(1950, 1975, 5)) +
  labs(
    title   = "Mortalidad infantil en Cuba, 1950–1975",
    x       = "Año",
    y       = "Mortalidad infantil (por 1,000 habitantes)",
    caption = "-- Revolución Cubana (1959)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(hjust = 0.5, face = "bold"),
    plot.caption     = element_text(hjust = 0, size = 9),
    panel.grid.minor = element_blank()
  )

g2 <- grafica_imr(
  cuba, l_paises,
  "Mortalidad infantil: Cuba vs. países latinoamericanos"
)

g3 <- grafica_imr(
  cuba, o_paises,
  "Mortalidad infantil: Cuba vs. países occidentales"
)

ggsave(here::here("stores", "g1_cuba.pdf"),  g1, width = 7, height = 4.5)
ggsave(here::here("stores", "g2_cuba_l.pdf"), g2, width = 8, height = 5)
ggsave(here::here("stores", "g3_cuba_o.pdf"), g3, width = 8, height = 5)

# -----------------------------------------------------------------------------
# b. Control sintético con el pool de donantes de todos los países disponibles
# -----------------------------------------------------------------------------

cuba_df <- as.data.frame(cuba)

donantes <- cuba_df %>%
  filter(country != "cuba") %>%
  pull(id) %>%
  unique()

dataprep_out <- dataprep(
  foo                   = cuba_df,
  predictors            = c("imr", "gdppc", "urban"),
  predictors.op         = "mean",
  special.predictors    = list(
    list("leo",       1950, "mean"),
    list("leo",       1955, "mean"),
    list("leo",       1958, "mean"),
    list("schoolalt", 1950, "mean"),
    list("school",    1950, "mean"),
    list("school",    1955, "mean")
  ),
  dependent             = "imr",
  unit.variable         = "id",
  unit.names.variable   = "country",
  time.variable         = "year",
  treatment.identifier  = 9,
  controls.identifier   = donantes,
  time.predictors.prior = 1950:1958,
  time.optimize.ssr     = 1950:1958,
  time.plot             = 1950:1975
)

synth_out    <- synth(dataprep_out)
synth_tables <- synth.tab(synth.res = synth_out, dataprep.res = dataprep_out)

pesos <- synth_tables$tab.w
pesos[pesos$w.weights > 0.001, ]

tiempo    <- 1950:1975
cuba_real <- dataprep_out$Y1plot
cuba_sint <- dataprep_out$Y0plot %*% synth_out$solution.w

df_sintetico <- data.frame(
  year      = tiempo,
  real      = as.numeric(cuba_real),
  sintetico = as.numeric(cuba_sint)
) %>%
  pivot_longer(cols      = c(real, sintetico),
               names_to  = "serie",
               values_to = "imr") %>%
  mutate(serie = case_when(
    serie == "real"      ~ "Cuba real",
    serie == "sintetico" ~ "Cuba sintética"
  ))

g_sint <- ggplot(df_sintetico, aes(x = year, y = imr,
                                   color     = serie,
                                   linewidth = serie)) +
  geom_line() +
  geom_vline(xintercept = 1959, linetype = "dashed", color = "black") +
  scale_color_manual(
    values = c("Cuba real"      = "black",
               "Cuba sintética" = "steelblue"),
    name = NULL
  ) +
  scale_linewidth_manual(
    values = c("Cuba real"      = 1.2,
               "Cuba sintética" = 0.8),
    guide  = "none"
  ) +
  scale_x_continuous(breaks = seq(1950, 1975, 5)) +
  labs(
    title   = "Cuba real vs. Cuba sintética",
    x       = "Año",
    y       = "Mortalidad infantil (por 1,000 habitantes)",
    caption = "-- Revolución Cubana (1959)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(hjust = 0.5, face = "bold"),
    plot.caption     = element_text(hjust = 0, size = 9),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom"
  )

df_efecto <- data.frame(
  year   = tiempo,
  efecto = as.numeric(cuba_real) - as.numeric(cuba_sint)
)

g_efecto <- ggplot(df_efecto, aes(x = year, y = efecto)) +
  geom_line(color = "black", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "steelblue") +
  geom_vline(xintercept = 1959, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = seq(1950, 1975, 5)) +
  labs(
    title   = "Efecto estimado de la Revolución Cubana sobre la mortalidad infantil",
    x       = "Año",
    y       = "Diferencia en mortalidad (por 1,000 habitantes)",
    caption = "-- Revolución Cubana (1959)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(hjust = 0.5, face = "bold"),
    plot.caption     = element_text(hjust = 0, size = 9),
    panel.grid.minor = element_blank()
  )

ggsave(store_file("g4_sintetico.pdf"), g_sint,   width = 8, height = 5)
ggsave(store_file("g5_efecto.pdf"),    g_efecto, width = 8, height = 5)

# -----------------------------------------------------------------------------
# Tabla descriptiva: Cuba real vs. sintético vs. media latinoamericana vs. resto
# -----------------------------------------------------------------------------

pre   <- 1950:1958
latam <- cuba_df %>% filter(region == 0, country != "cuba") %>% pull(country) %>% unique()
otros <- cuba_df %>% filter(region == 1) %>% pull(country) %>% unique()

w_sweden     <- 0.619
w_honduras   <- 0.296
w_elsalvador <- 0.083

sint_var <- function(var, año = NULL) {
  if (is.null(año)) {
    w_sweden     * mean(cuba_df %>% filter(country=="sweden",     year %in% pre) %>% pull(!!var), na.rm=TRUE) +
      w_honduras   * mean(cuba_df %>% filter(country=="honduras",   year %in% pre) %>% pull(!!var), na.rm=TRUE) +
      w_elsalvador * mean(cuba_df %>% filter(country=="elsalvador", year %in% pre) %>% pull(!!var), na.rm=TRUE)
  } else {
    w_sweden     * (cuba_df %>% filter(country=="sweden",     year==año) %>% pull(!!var)) +
      w_honduras   * (cuba_df %>% filter(country=="honduras",   year==año) %>% pull(!!var)) +
      w_elsalvador * (cuba_df %>% filter(country=="elsalvador", year==año) %>% pull(!!var))
  }
}

mg <- function(paises, var, año = NULL) {
  if (is.null(año)) {
    cuba_df %>% filter(country %in% paises, year %in% pre) %>% pull(!!var) %>% mean(na.rm=TRUE)
  } else {
    cuba_df %>% filter(country %in% paises, year == año) %>% pull(!!var) %>% mean(na.rm=TRUE)
  }
}

cuba_pre  <- cuba_df %>% filter(country == "cuba", year %in% pre)
sint_vals <- data.frame(year = tiempo, sintetico = as.numeric(cuba_sint)) %>% filter(year %in% pre)

tab <- data.frame(
  Variable = c(
    "Mortalidad infantil (por 1,000 hab.)",
    "Tasa de urbanización",
    "PIB per cápita",
    "Esperanza de vida (1950)",
    "Esperanza de vida (1955)",
    "Esperanza de vida (1958)",
    "Años promedio de educación (1950)",
    "IDHA (1950)",
    "IDHA (1955)"
  ),
  Cuba_real = c(
    mean(cuba_pre$imr,   na.rm=TRUE),
    mean(cuba_pre$urban, na.rm=TRUE),
    mean(cuba_pre$gdppc, na.rm=TRUE),
    cuba_df %>% filter(country=="cuba", year==1950) %>% pull(leo),
    cuba_df %>% filter(country=="cuba", year==1955) %>% pull(leo),
    cuba_df %>% filter(country=="cuba", year==1958) %>% pull(leo),
    cuba_df %>% filter(country=="cuba", year==1950) %>% pull(schoolalt),
    cuba_df %>% filter(country=="cuba", year==1950) %>% pull(school),
    cuba_df %>% filter(country=="cuba", year==1955) %>% pull(school)
  ),
  Cuba_sint = c(
    mean(sint_vals$sintetico, na.rm=TRUE),
    sint_var("urban"),
    sint_var("gdppc"),
    sint_var("leo", 1950),
    sint_var("leo", 1955),
    sint_var("leo", 1958),
    sint_var("schoolalt", 1950),
    sint_var("school",    1950),
    sint_var("school",    1955)
  ),
  Media_latam = c(
    mg(latam, "imr"),
    mg(latam, "urban"),
    mg(latam, "gdppc"),
    mg(latam, "leo", 1950),
    mg(latam, "leo", 1955),
    mg(latam, "leo", 1958),
    mg(latam, "schoolalt", 1950),
    mg(latam, "school",    1950),
    mg(latam, "school",    1955)
  ),
  Media_otros = c(
    mg(otros, "imr"),
    mg(otros, "urban"),
    mg(otros, "gdppc"),
    mg(otros, "leo", 1950),
    mg(otros, "leo", 1955),
    mg(otros, "leo", 1958),
    mg(otros, "schoolalt", 1950),
    mg(otros, "school",    1950),
    mg(otros, "school",    1955)
  )
) %>% mutate(across(where(is.numeric), ~round(., 2)))

print(xtable(tab,
             caption = "Variables predictoras: Cuba real, sintética y grupos de comparación",
             label   = "tab:sintetico_balance"),
      include.rownames  = FALSE,
      booktabs          = TRUE,
      caption.placement = "top")

# -----------------------------------------------------------------------------
# c. Prueba de placebo espacial
# -----------------------------------------------------------------------------

run_synth_placebo <- function(treat_id, foo, pre_period) {
  
  controls <- sort(setdiff(unique(foo$id), treat_id))
  
  dp <- tryCatch(
    Synth::dataprep(
      foo                   = as.data.frame(foo),
      predictors            = c("imr", "gdppc", "urban"),
      predictors.op         = "mean",
      special.predictors    = list(
        list("leo",       1950, "mean"),
        list("leo",       1955, "mean"),
        list("leo",       1958, "mean"),
        list("schoolalt", 1950, "mean"),
        list("school",    1950, "mean"),
        list("school",    1955, "mean")
      ),
      dependent             = "imr",
      unit.variable         = "id",
      unit.names.variable   = "country",
      time.variable         = "year",
      treatment.identifier  = treat_id,
      controls.identifier   = controls,
      time.predictors.prior = pre_period,
      time.optimize.ssr     = pre_period,
      time.plot             = 1950:1975
    ),
    error = function(e) {
      message("Falló id ", treat_id, ": ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(dp)) return(tibble::tibble())
  
  s_res <- tryCatch(
    Synth::synth(dp),
    error = function(e) {
      message("Falló synth id ", treat_id, ": ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(s_res)) return(tibble::tibble())
  
  years  <- dp$tag$time.plot
  Y1_tmp <- dp$Y1plot[, 1]
  Y0_tmp <- dp$Y0plot
  Y_syn  <- as.numeric(Y0_tmp %*% s_res$solution.w)
  gap    <- Y1_tmp - Y_syn
  
  tibble::tibble(
    year   = years,
    id     = treat_id,
    effect = gap
  )
}

all_ids <- sort(unique(cuba_df$id))

effects_all <- purrr::map_dfr(
  all_ids,
  ~ run_synth_placebo(
    treat_id   = .x,
    foo        = cuba_df,
    pre_period = 1950:1958
  )
)

effects_all <- effects_all %>%
  left_join(cuba_df %>% select(id, country) %>% distinct(), by = "id")

effects_plot <- effects_all %>%
  mutate(es_cuba = (country == "cuba"))

g_placebo <- ggplot(effects_plot, aes(x = year, y = effect, group = country)) +
  geom_line(
    data      = subset(effects_plot, !es_cuba),
    color     = "grey70",
    linewidth = 0.3
  ) +
  geom_line(
    data      = subset(effects_plot, es_cuba),
    color     = "black",
    linewidth = 1
  ) +
  geom_vline(xintercept = 1959, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0,    linetype = "dashed", color = "steelblue") +
  scale_x_continuous(breaks = seq(1950, 1975, 5)) +
  labs(
    title   = "Placebo espacial: Cuba vs. países del pool",
    x       = "Año",
    y       = "Efecto estimado (por 1,000 habitantes)",
    caption = "-- Revolución Cubana (1959). Línea negra: Cuba. Líneas grises: placebos."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(hjust = 0.5, face = "bold"),
    plot.caption     = element_text(hjust = 0, size = 9),
    panel.grid.minor = element_blank()
  )

ggsave(store_file("g6_placebo.pdf"), g_placebo, width = 9, height = 5)

pvalores <- effects_all %>%
  filter(year >= 1959) %>%
  group_by(year) %>%
  summarise(
    efecto_cuba = effect[country == "cuba"],
    p_valor     = mean(abs(effect) >= abs(efecto_cuba)),
    .groups     = "drop"
  )

print(xtable(pvalores %>% select(year, efecto_cuba, p_valor) %>%
               rename(Año          = year,
                      `Efecto Cuba` = efecto_cuba,
                      `P-valor`     = p_valor) %>%
               mutate(Año = as.integer(Año)),
             caption = "P-valores por año post-tratamiento — Placebo espacial",
             label   = "tab:pvalores"),
      include.rownames  = FALSE,
      booktabs          = TRUE,
      caption.placement = "top")

# -----------------------------------------------------------------------------
# d. Prueba de placebo temporal
# -----------------------------------------------------------------------------

dataprep_placebo_t <- dataprep(
  foo                   = cuba_df,
  predictors            = c("imr", "gdppc", "urban"),
  predictors.op         = "mean",
  special.predictors    = list(
    list("leo",    1950, "mean"),
    list("school", 1950, "mean")
  ),
  dependent             = "imr",
  unit.variable         = "id",
  unit.names.variable   = "country",
  time.variable         = "year",
  treatment.identifier  = 9,
  controls.identifier   = donantes,
  time.predictors.prior = 1950:1954,
  time.optimize.ssr     = 1950:1954,
  time.plot             = 1950:1975
)

synth_placebo_t <- synth(dataprep_placebo_t)

cuba_real_t <- dataprep_placebo_t$Y1plot
cuba_sint_t <- dataprep_placebo_t$Y0plot %*% synth_placebo_t$solution.w

df_sint_t <- data.frame(
  year      = 1950:1975,
  real      = as.numeric(cuba_real_t),
  sintetico = as.numeric(cuba_sint_t)
) %>%
  pivot_longer(cols      = c(real, sintetico),
               names_to  = "serie",
               values_to = "imr") %>%
  mutate(serie = case_when(
    serie == "real"      ~ "Cuba real",
    serie == "sintetico" ~ "Cuba sintética"
  ))

g_plac_t <- ggplot(df_sint_t, aes(x = year, y = imr,
                                  color     = serie,
                                  linewidth = serie)) +
  geom_line() +
  geom_vline(xintercept = 1955, linetype = "dashed", color = "black") +
  scale_color_manual(
    values = c("Cuba real"      = "black",
               "Cuba sintética" = "steelblue"),
    name = NULL
  ) +
  scale_linewidth_manual(
    values = c("Cuba real"      = 1.2,
               "Cuba sintética" = 0.8),
    guide  = "none"
  ) +
  scale_x_continuous(breaks = seq(1950, 1975, 5)) +
  labs(
    title   = "Placebo temporal: Cuba real vs. Cuba sintética (tratamiento ficticio 1955)",
    x       = "Año",
    y       = "Mortalidad infantil (por 1,000 habitantes)",
    caption = "-- Año de tratamiento ficticio (1955)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(hjust = 0.5, face = "bold"),
    plot.caption     = element_text(hjust = 0, size = 9),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom"
  )

df_efecto_t <- data.frame(
  year   = 1950:1975,
  efecto = as.numeric(cuba_real_t) - as.numeric(cuba_sint_t)
)

g_efecto_t <- ggplot(df_efecto_t, aes(x = year, y = efecto)) +
  geom_line(color = "black", linewidth = 1) +
  geom_hline(yintercept = 0,    linetype = "dashed", color = "steelblue") +
  geom_vline(xintercept = 1955, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = seq(1950, 1975, 5)) +
  labs(
    title   = "Placebo temporal: efecto estimado (tratamiento ficticio 1955)",
    x       = "Año",
    y       = "Diferencia en mortalidad (por 1,000 habitantes)",
    caption = "-- Año de tratamiento ficticio (1955)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(hjust = 0.5, face = "bold"),
    plot.caption     = element_text(hjust = 0, size = 9),
    panel.grid.minor = element_blank()
  )

ggsave(store_file("g7_placebo_temporal.pdf"), g_plac_t,   width = 8, height = 5)
ggsave(store_file("g8_efecto_temporal.pdf"),  g_efecto_t, width = 8, height = 5)

# -----------------------------------------------------------------------------
# e. Limitando la muestra a latinoamerica
# -----------------------------------------------------------------------------

donantes_latam <- cuba_df %>%
  filter(region == 0, country != "cuba") %>%
  pull(id) %>%
  unique() %>%
  sort()

dataprep_latam <- dataprep(
  foo                   = cuba_df,
  predictors            = c("imr", "gdppc", "urban"),
  predictors.op         = "mean",
  special.predictors    = list(
    list("leo",       1950, "mean"),
    list("leo",       1955, "mean"),
    list("leo",       1958, "mean"),
    list("schoolalt", 1950, "mean"),
    list("school",    1950, "mean"),
    list("school",    1955, "mean")
  ),
  dependent             = "imr",
  unit.variable         = "id",
  unit.names.variable   = "country",
  time.variable         = "year",
  treatment.identifier  = 9,
  controls.identifier   = donantes_latam,
  time.predictors.prior = 1950:1958,
  time.optimize.ssr     = 1950:1958,
  time.plot             = 1950:1975
)

synth_latam     <- synth(dataprep_latam)
synth_tab_latam <- synth.tab(synth.res = synth_latam, dataprep.res = dataprep_latam)

pesos_latam <- synth_tab_latam$tab.w
pesos_latam[pesos_latam$w.weights > 0.001, ]

cuba_sint_latam <- dataprep_latam$Y0plot %*% synth_latam$solution.w

df_sint_latam <- data.frame(
  year      = 1950:1975,
  real      = as.numeric(dataprep_latam$Y1plot),
  sintetico = as.numeric(cuba_sint_latam)
) %>%
  pivot_longer(cols      = c(real, sintetico),
               names_to  = "serie",
               values_to = "imr") %>%
  mutate(serie = case_when(
    serie == "real"      ~ "Cuba real",
    serie == "sintetico" ~ "Cuba sintética"
  ))

g_sint_latam <- ggplot(df_sint_latam, aes(x = year, y = imr,
                                          color     = serie,
                                          linewidth = serie)) +
  geom_line() +
  geom_vline(xintercept = 1959, linetype = "dashed", color = "black") +
  scale_color_manual(
    values = c("Cuba real"      = "black",
               "Cuba sintética" = "steelblue"),
    name = NULL
  ) +
  scale_linewidth_manual(
    values = c("Cuba real"      = 1.2,
               "Cuba sintética" = 0.8),
    guide  = "none"
  ) +
  scale_x_continuous(breaks = seq(1950, 1975, 5)) +
  labs(
    title   = "Cuba real vs. Cuba sintética (pool latinoamericano)",
    x       = "Año",
    y       = "Mortalidad infantil (por 1,000 habitantes)",
    caption = "-- Revolución Cubana (1959)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(hjust = 0.5, face = "bold"),
    plot.caption     = element_text(hjust = 0, size = 9),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom"
  )

ggsave(store_file("g9_sintetico_latam.pdf"), g_sint_latam, width = 8, height = 5)