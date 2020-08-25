
# ggplot2 -----------------------------------------------------------------

library(dplyr)
library(ggplot2)

ames <- readr::read_rds("data/ames.rds")

# Qualidade vs valor da venda

ames %>%
  ggplot() +
  geom_point(aes(x = geral_qualidade, y = venda_valor))

# Melhorando visual do gráfico

ames %>%
  ggplot() +
  geom_point(aes(x = geral_qualidade, y = venda_valor)) +
  labs(x = "Qualidade", y = "Valor da venda ($)") +
  scale_x_continuous(breaks = 1:10) +
  theme_minimal()

# Transformando "geral_qualidade" em
# uma variável categórica e mudando
# escala do valor

ames %>%
  mutate(
    geral_qualidade = as.character(geral_qualidade),
    venda_valor = venda_valor / 1000
  ) %>%
  ggplot() +
  geom_boxplot(aes(x = geral_qualidade, y = venda_valor)) +
  labs(x = "Qualidade", y = "Valor da venda (em milhares de dólares)") +
  theme_minimal()

# Transformando em fatores

ames %>%
  mutate(
    geral_qualidade = as.factor(geral_qualidade),
    venda_valor = venda_valor / 1000
  ) %>%
  ggplot() +
  geom_boxplot(aes(x = geral_qualidade, y = venda_valor)) +
  labs(x = "Qualidade", y = "Valor da venda (em milhares de dólares)") +
  theme_minimal()

# Mapas com ggplot2 -------------------------------------------------------


# Mapas -------------------------------------------------------------------



# Temas -------------------------------------------------------------------

rick_and_morty <- readr::read_rds("data/rick_and_morty.rds")

img_rick <- png::readPNG("R/rick.png") %>%
  grid::rasterGrob(interpolate = TRUE)

# Um gráfico de linha

rick_and_morty %>%
  ggplot(aes(x = num_episodio, y = qtd_espectadores_EUA)) +
  geom_line()

# Um gráfico de barras

rick_and_morty %>%
  mutate(num_temporada = as.factor(num_temporada)) %>%
  ggplot(aes(
    x = num_episodio,
    y = qtd_espectadores_EUA,
    fill = num_temporada)
  ) +
  geom_col()

# Construindo o tema

rick_and_morty %>%
  mutate(num_temporada = as.factor(num_temporada)) %>%
  ggplot(aes(
    x = num_episodio,
    y = qtd_espectadores_EUA,
    fill = num_temporada)
  ) +
  geom_col() +
  labs(
    x = "episodio",
    y = "audiencia",
    fill = "temporada",
    title = "Rick and Morty"
  ) +
  annotation_custom(img_rick, xmin = 35, ymin = 2) +
  theme(
    text = element_text(
      colour = "#11a2c6",
      family = "Get Schwifty",
      size = 16
    ),
    plot.title = element_text(
      family = "Get Schwifty",
      hjust = 0.5,
      size = 30
    ),
    axis.text = element_text(color = "white"),
    axis.ticks.x = element_line(color = "white"),
    panel.background = element_rect(fill = "black"),
    panel.grid.major.y = element_line(size = 0.1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.background = element_rect(fill = "black", color = "black"),
    plot.background = element_rect(fill = "black", color = "black")
  )

# Criando uma função que
# aplica o tema

tema_rick_and_morty <- function() {
  theme(
    text = element_text(
      colour = "#11a2c6",
      family = "Get Schwifty",
      size = 16
    ),
    plot.title = element_text(
      family = "Get Schwifty",
      hjust = 0.5,
      size = 30
    ),
    axis.text = element_text(color = "white"),
    axis.ticks.x = element_line(color = "white"),
    panel.background = element_rect(fill = "black"),
    panel.grid.major.y = element_line(size = 0.1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.background = element_rect(fill = "black", color = "black"),
    plot.background = element_rect(fill = "black", color = "black")
  )
}

# Aplicando a nossa função
# com o tema criado

rick_and_morty %>%
  mutate(num_temporada = as.factor(num_temporada)) %>%
  ggplot(aes(
    x = num_episodio,
    y = qtd_espectadores_EUA,
    fill = num_temporada)
  ) +
  geom_col() +
  labs(
    x = "episodio",
    y = "audiencia",
    fill = "temporada",
    title = "Rick and Morty"
  ) +
  annotation_custom(img_rick, xmin = 35, ymin = 2) +
  tema_rick_and_morty()

# Aplicando o tema no gráfico
# de linha

rick_and_morty %>%
  mutate(num_temporada = as.factor(num_temporada)) %>%
  ggplot(aes(x = num_episodio, y = qtd_espectadores_EUA)) +
  geom_line(color = "white") +
  labs(
    x = "episodio",
    y = "audiencia",
    title = "Rick and Morty"
  ) +
  annotation_custom(img_rick, xmin = 35, ymin = 2) +
  tema_rick_and_morty()


