# =========================================================
# ESTILO GGPLOT2 BASADO EN MANUAL DE MARCA
# Zamora Chinchipe / Naturaleza y Cultura Internacional
# =========================================================

library(ggplot2)
library(scales)

# -----------------------------
# 1. Paleta institucional
# -----------------------------
pal_zch <- c(
  azul_institucional = "#0E74BC",
  verde_territorial  = "#44B749",
  verde_lima         = "#CADB2D",
  amarillo_energia   = "#FFD503",
  azul_gris_tecnico  = "#3A5562",
  blanco             = "#FFFFFF"
)

# Paleta principal para categorías
pal_zch_cat <- c(
  "#0E74BC", # azul institucional
  "#44B749", # verde territorial
  "#CADB2D", # verde lima
  "#FFD503", # amarillo
  "#3A5562"  # azul gris técnico
)

# -----------------------------
# 2. Tema general para gráficas
# -----------------------------
theme_zch <- function(base_size = 12, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # Fondo
      plot.background   = element_rect(fill = pal_zch["blanco"], color = NA),
      panel.background  = element_rect(fill = pal_zch["blanco"], color = NA),
      panel.border      = element_blank(),

      # Grillas
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(
        color = alpha(pal_zch["azul_gris_tecnico"], 0.18),
        linewidth = 0.35
      ),

      # Títulos
      plot.title = element_text(
        color = pal_zch["azul_institucional"],
        face = "bold",
        size = rel(1.35),
        margin = margin(b = 8)
      ),
      plot.subtitle = element_text(
        color = pal_zch["azul_gris_tecnico"],
        size = rel(1.0),
        margin = margin(b = 10)
      ),
      plot.caption = element_text(
        color = alpha(pal_zch["azul_gris_tecnico"], 0.85),
        size = rel(0.85),
        hjust = 1
      ),

      # Ejes
      axis.title.x = element_text(
        color = pal_zch["azul_gris_tecnico"],
        face = "bold",
        margin = margin(t = 8)
      ),
      axis.title.y = element_text(
        color = pal_zch["azul_gris_tecnico"],
        face = "bold",
        margin = margin(r = 8)
      ),
      axis.text = element_text(
        color = pal_zch["azul_gris_tecnico"]
      ),
      axis.ticks = element_blank(),
      axis.line = element_blank(),

      # Leyenda
      legend.position = "right",
      legend.title = element_text(
        color = pal_zch["azul_gris_tecnico"],
        face = "bold"
      ),
      legend.text = element_text(
        color = pal_zch["azul_gris_tecnico"]
      ),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.key = element_rect(fill = "transparent", color = NA),

      # Facetas
      strip.background = element_rect(
        fill = alpha(pal_zch["azul_institucional"], 0.10),
        color = NA
      ),
      strip.text = element_text(
        color = pal_zch["azul_gris_tecnico"],
        face = "bold"
      ),

      # Márgenes
      plot.margin = margin(12, 14, 12, 12)
    )
}

# -----------------------------
# 3. Escalas discretas
# -----------------------------
scale_color_zch <- function(...) {
  scale_color_manual(values = pal_zch_cat, ...)
}

scale_fill_zch <- function(...) {
  scale_fill_manual(values = pal_zch_cat, ...)
}

# -----------------------------
# 4. Escalas continuas
# -----------------------------
# Para gradientes técnicos: azul gris -> azul institucional
scale_fill_zch_continuous <- function(...) {
  scale_fill_gradient(
    low  = pal_zch["azul_gris_tecnico"],
    high = pal_zch["azul_institucional"],
    ...
  )
}

scale_color_zch_continuous <- function(...) {
  scale_color_gradient(
    low  = pal_zch["azul_gris_tecnico"],
    high = pal_zch["azul_institucional"],
    ...
  )
}

# Para gradiente ambiental: azul -> verde
scale_fill_zch_ambiental <- function(...) {
  scale_fill_gradientn(
    colours = c(
      pal_zch["azul_institucional"],
      pal_zch["verde_territorial"]
    ),
    ...
  )
}

# Para gradiente biodiversidad: lima -> amarillo
scale_fill_zch_biodiv <- function(...) {
  scale_fill_gradientn(
    colours = c(
      pal_zch["verde_lima"],
      pal_zch["amarillo_energia"]
    ),
    ...
  )
}

# -----------------------------
# 5. Función para resaltar un color de acento
# -----------------------------
# Útil cuando quieres una gráfica monocromática con un solo acento
scale_fill_zch_single <- function(accent = c("azul", "verde", "lima", "amarillo", "gris"), ...) {
  accent <- match.arg(accent)

  color_sel <- switch(
    accent,
    azul     = pal_zch["azul_institucional"],
    verde    = pal_zch["verde_territorial"],
    lima     = pal_zch["verde_lima"],
    amarillo = pal_zch["amarillo_energia"],
    gris     = pal_zch["azul_gris_tecnico"]
  )

  scale_fill_manual(values = color_sel, ...)
}

# -----------------------------
# 6. Tema para mapas con ggplot
# -----------------------------
theme_map_zch <- function(base_size = 12, base_family = "sans") {
  theme_void(base_size = base_size, base_family = base_family) +
    theme(
      plot.background  = element_rect(fill = pal_zch["blanco"], color = NA),
      panel.background = element_rect(fill = pal_zch["blanco"], color = NA),

      plot.title = element_text(
        color = pal_zch["azul_institucional"],
        face = "bold",
        size = rel(1.35),
        hjust = 0,
        margin = margin(b = 6)
      ),
      plot.subtitle = element_text(
        color = pal_zch["azul_gris_tecnico"],
        size = rel(1.0),
        hjust = 0,
        margin = margin(b = 10)
      ),
      plot.caption = element_text(
        color = alpha(pal_zch["azul_gris_tecnico"], 0.85),
        size = rel(0.85),
        hjust = 1
      ),

      legend.position = "right",
      legend.title = element_text(
        color = pal_zch["azul_gris_tecnico"],
        face = "bold"
      ),
      legend.text = element_text(
        color = pal_zch["azul_gris_tecnico"]
      ),
      legend.key = element_rect(fill = "transparent", color = NA),

      plot.margin = margin(10, 12, 10, 10)
    )
}

# -----------------------------
# 7. Escalas para mapas
# -----------------------------
# Mapa categórico
scale_fill_map_zch <- function(...) {
  scale_fill_manual(values = pal_zch_cat, ...)
}

scale_color_map_zch <- function(...) {
  scale_color_manual(values = pal_zch_cat, ...)
}

# Mapa continuo ambiental
scale_fill_map_zch_cont <- function(...) {
  scale_fill_gradientn(
    colours = c(
      pal_zch["azul_institucional"],
      pal_zch["verde_territorial"],
      pal_zch["verde_lima"],
      pal_zch["amarillo_energia"]
    ),
    ...
  )
}

# -----------------------------
# 8. Estilos sugeridos para capas espaciales
# -----------------------------
estilo_poligono_base <- list(
  fill = alpha(pal_zch["verde_territorial"], 0.65),
  color = pal_zch["azul_gris_tecnico"],
  linewidth = 0.30
)

estilo_limite <- list(
  fill = NA,
  color = pal_zch["azul_institucional"],
  linewidth = 0.55
)

estilo_puntos <- list(
  color = pal_zch["amarillo_energia"],
  fill = pal_zch["azul_institucional"],
  shape = 21,
  size = 2.8,
  stroke = 0.45
)

estilo_rio <- list(
  color = pal_zch["azul_institucional"],
  linewidth = 0.6,
  alpha = 0.9
)

# =========================================================
# 9. EJEMPLOS DE USO
# =========================================================

# ---------------------------------
# A. Gráfico de barras
# ---------------------------------
# ggplot(df, aes(x = categoria, y = valor, fill = categoria)) +
#   geom_col(width = 0.75, color = "white") +
#   scale_fill_zch() +
#   labs(
#     title = "Indicadores por categoría",
#     subtitle = "Estilo basado en manual de marca",
#     x = "Categoría",
#     y = "Valor"
#   ) +
#   theme_zch()

# ---------------------------------
# B. Serie temporal
# ---------------------------------
# ggplot(df, aes(x = fecha, y = valor)) +
#   geom_line(color = pal_zch["azul_institucional"], linewidth = 1.1) +
#   geom_point(color = pal_zch["verde_territorial"], size = 2.2) +
#   labs(
#     title = "Tendencia temporal",
#     subtitle = "Azul para estructura, verde para énfasis",
#     x = "Fecha",
#     y = "Valor"
#   ) +
#   theme_zch()

# ---------------------------------
# C. Mapa de polígonos simple
# sf_object debe ser un objeto sf
# ---------------------------------
# ggplot() +
#   geom_sf(
#     data = sf_object,
#     fill = estilo_poligono_base$fill,
#     color = estilo_poligono_base$color,
#     linewidth = estilo_poligono_base$linewidth
#   ) +
#   labs(
#     title = "Cobertura territorial",
#     subtitle = "Mapa base institucional"
#   ) +
#   theme_map_zch()

# ---------------------------------
# D. Mapa temático continuo
# ---------------------------------
# ggplot(sf_object) +
#   geom_sf(aes(fill = valor), color = NA) +
#   scale_fill_map_zch_cont(name = "Valor") +
#   labs(
#     title = "Mapa temático",
#     subtitle = "Gradiente ambiental institucional"
#   ) +
#   theme_map_zch()

# ---------------------------------
# E. Mapa con límites + puntos
# ---------------------------------
# ggplot() +
#   geom_sf(data = poligonos, fill = alpha(pal_zch["verde_territorial"], 0.25),
#           color = pal_zch["azul_gris_tecnico"], linewidth = 0.25) +
#   geom_sf(data = limites, fill = NA,
#           color = pal_zch["azul_institucional"], linewidth = 0.7) +
#   geom_sf(data = puntos,
#           shape = estilo_puntos$shape,
#           size = estilo_puntos$size,
#           color = estilo_puntos$color,
#           fill = estilo_puntos$fill,
#           stroke = estilo_puntos$stroke) +
#   labs(
#     title = "Sitios de muestreo",
#     subtitle = "Límites en azul institucional, puntos destacados en amarillo"
#   ) +
#   theme_map_zch()