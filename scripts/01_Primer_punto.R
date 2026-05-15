# =============================================================================
# Script_01: Ejercicio 1
# =============================================================================
# -----------------------------------------------------------------------------
# 0. Cargar bases de datos 
# -----------------------------------------------------------------------------

# Cargar configuración base

source(here::here("scripts", "00_Config.R"))

facebook   <- import(here::here("data", "facebook.dta"))

# -----------------------------------------------------------------------------
# Exploración inicial de los datos
# -----------------------------------------------------------------------------

dim(facebook)        
str(facebook)       
names(facebook) 
skim(facebook)

facebook <- facebook %>%
  mutate(
    
    # Para never treated (fbexp = NA) queda NA 
    rel_time = semester - fbexp,
    
    # Para C&S
    fbexp_cs = ifelse(is.na(fbexp), 0, fbexp),
    
    # Para Sun & Abraham
    fbexp_sa = ifelse(is.na(fbexp), Inf, fbexp)
  )

# Rango de tiempo relativo entre unidades tratadas

range(facebook$rel_time, na.rm = TRUE)  # -12 a +8

# -----------------------------------------------------------------------------
# 2. Estimación estática 
# -----------------------------------------------------------------------------

twfe_sin <- feols(
  eq_index_mh_all ~ D | uni_id + semester,
  data    = facebook,
  cluster = ~uni_id
)

twfe_con <- feols(
  eq_index_mh_all ~ D + female + age + I(age^2) + year_in_school +
    white + black + hispanic + asian + indian + other_race +
    international | uni_id + semester,
  data    = facebook,
  cluster = ~uni_id
)

summary(twfe_sin)
summary(twfe_con)

etable(
  twfe_sin, twfe_con,
  headers = c("Sin controles", "Con controles"),
  digits  = 3,
  tex     = TRUE,
  dict    = c(
    eq_index_mh_all  = "Índice mala salud mental",
    D                = "Facebook disponible",
    female           = "Mujer",
    age              = "Edad",
    "I(age^2)"       = "Edad$^2$",
    year_in_school   = "Año en la universidad",
    white            = "Blanco",
    black            = "Negro",
    hispanic         = "Hispano",
    asian            = "Asiático",
    indian           = "Nativo americano",
    other_race       = "Otra raza",
    international    = "Internacional"
  )
)

# -----------------------------------------------------------------------------
# 3. Estimación dinámica
# -----------------------------------------------------------------------------

facebook <- facebook %>%
  mutate(
    rel_time_bin = case_when(
      is.na(rel_time) ~ NA_real_,
      rel_time < -8   ~ -8,
      rel_time >  8   ~  8,
      TRUE            ~ rel_time
    )
  )

twfe_din <- feols(
  eq_index_mh_all ~ i(rel_time_bin, ref = -1) | uni_id + semester,
  data    = facebook,
  cluster = ~uni_id
)

summary(twfe_din)

png(store_file("fig_twfe_dinamico.png"), width = 2400, height = 1600, res = 300)

par(
  bg  = "white",           # fondo blanco
  col.axis = "gray30",     # color de los ejes
  col.lab  = "gray20",     # color de los títulos de ejes
  col.main = "gray15",     # color del título
  font.main = 2,           # título en negrita
  cex.main  = 1.2,         # tamaño del título
  cex.lab   = 1.0,         # tamaño de los títulos de ejes
  mgp = c(2.5, 0.7, 0)    # márgenes de los ejes
)

iplot(
  twfe_din,
  main    = "TWFE dinámico: efecto de Facebook sobre mala salud mental",
  xlab    = "Semestres respecto a la llegada de Facebook",
  ylab    = "Coeficiente estimado (desviaciones estándar)",
  col     = "steelblue",
  pt.join = TRUE,
  ci.col  = "steelblue",
  zero    = TRUE,
  grid    = FALSE          # quitar grilla interna de iplot
)

abline(v = -0.5, lty = 3, col = "gray55", lwd = 1.5)  # línea vertical punteada

dev.off()


# -----------------------------------------------------------------------------
# 4. Estudio de eventos
# -----------------------------------------------------------------------------

cs <- att_gt(
  yname         = "eq_index_mh_all",
  tname         = "semester",
  idname        = "uni_id",
  gname         = "fbexp_cs",
  data          = facebook,
  panel         = FALSE,   
  control_group = "notyettreated",
  clustervars   = "uni_id",
  est_method    = "reg"
)

cs_din <- aggte(cs, type = "dynamic",
                min_e = -8, max_e = 2,
                na.rm = TRUE)
summary(cs_din)


p_cs <- ggdid(cs_din) +
  
  scale_color_manual(values = c("gray60", "steelblue")) +
  scale_fill_manual(values = c("gray60", "steelblue")) +
  
  labs(
    title    = "Efecto dinámico de Facebook sobre mala salud mental",
    subtitle = "Callaway & Sant'Anna (2021)",
    x        = "Semestres respecto a la llegada de Facebook",
    y        = "ATT estimado (desviaciones estándar)"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    legend.position    = "none",
    plot.title         = element_text(hjust = 0.5, face = "bold", color = "gray15", size = 16),
    plot.subtitle      = element_text(hjust = 0.5, color = "gray40", size = 12),
    axis.title         = element_text(face = "bold", color = "gray20"),
    axis.text          = element_text(color = "gray30"),
    panel.grid.minor   = element_blank(),
    panel.grid.major   = element_blank(),   # sin grilla
    panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.background   = element_rect(fill = "white", color = NA),
    plot.background    = element_rect(fill = "white", color = NA)
  ) +
  
  geom_hline(yintercept = 0,    linetype = "solid",  color = "black",  linewidth = 0.5) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray55", linewidth = 0.7)

ggsave(store_file("fig_cs_dinamico.png"),
       plot = p_cs, width = 8, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# 5. Comparación de estimadores
# -----------------------------------------------------------------------------

sa <- feols(
  eq_index_mh_all ~ sunab(fbexp_sa, semester) | uni_id + semester,
  data    = facebook,
  cluster = ~uni_id
)

summary(sa)

# Extraer coeficientes de cada modelo
coef_twfe <- iplot(twfe_din, only.params = TRUE)
coef_sa   <- iplot(sa,       only.params = TRUE)

# TWFE dinámico
df_twfe <- data.frame(
  rel_time  = coef_twfe$prms$estimate_names,
  estimate  = coef_twfe$prms$estimate,
  conf.low  = coef_twfe$prms$ci_low,
  conf.high = coef_twfe$prms$ci_high,
  metodo    = "TWFE"
) %>%
  bind_rows(data.frame(
    rel_time = -1, estimate = 0,
    conf.low = 0, conf.high = 0, metodo = "TWFE"
  ))

# Sun & Abraham
df_sa <- data.frame(
  rel_time  = coef_sa$prms$estimate_names,
  estimate  = coef_sa$prms$estimate,
  conf.low  = coef_sa$prms$ci_low,
  conf.high = coef_sa$prms$ci_high,
  metodo    = "Sun & Abraham"
) %>%
  bind_rows(data.frame(
    rel_time = -1, estimate = 0,
    conf.low = 0, conf.high = 0, metodo = "Sun & Abraham"
  ))

# Callaway & Sant'Anna — con panel = FALSE y max_e = 2
df_cs <- data.frame(
  rel_time  = cs_din$egt,
  estimate  = cs_din$att,
  conf.low  = cs_din$att - 1.96 * cs_din$se,
  conf.high = cs_din$att + 1.96 * cs_din$se,
  metodo    = "Callaway & Sant'Anna"
)

# Unir y filtrar al rango común [-8, +2]
df_comp <- bind_rows(df_twfe, df_sa, df_cs) %>%
  filter(rel_time >= -8, rel_time <= 2)

# Gráfico comparativo

p_comp <- ggplot(
  df_comp,
  aes(x = rel_time, y = estimate, color = metodo, shape = metodo)
) +
  geom_hline(yintercept = 0,    linetype = "solid",  color = "black",  linewidth = 0.5) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray55", linewidth = 0.7) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = 0.5),
    width = 0.2, linewidth = 0.6
  ) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  scale_color_manual(values = c(
    "TWFE"                 = "gray40",
    "Callaway & Sant'Anna" = "steelblue",
    "Sun & Abraham"        = "black"
  )) +
  scale_shape_manual(values = c(
    "TWFE"                 = 17,
    "Callaway & Sant'Anna" = 16,
    "Sun & Abraham"        = 15
  )) +
  scale_x_continuous(breaks = -8:2) +
  labs(
    title  = "Comparación de estimadores: efecto de Facebook sobre mala salud mental",
    x      = "Semestres respecto a la llegada de Facebook",
    y      = "Coef. estimado (desv. estándar)",
    color  = "Estimador", shape = "Estimador"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(hjust = 0.5, face = "bold", color = "gray15", size = 11),
    axis.title.y     = element_text(face = "bold", color = "gray20", size = 10),
    axis.title.x     = element_text(face = "bold", color = "gray20", size = 11),
    axis.text        = element_text(color = "gray30"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    plot.margin      = margin(10, 20, 10, 10),
    legend.position  = "bottom",
    legend.text      = element_text(size = 10),
    legend.title     = element_text(size = 10, face = "bold")
  )

ggsave(store_file("fig_comparacion_estimadores.png"),
       plot = p_comp, width = 9, height = 5, dpi = 300)