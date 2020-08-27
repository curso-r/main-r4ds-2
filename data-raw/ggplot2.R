
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

# install.packages(sf)

library(sf)

# Simple Featuares (sf) é um conjunto de padrões
# que especificam um modelo de armazenamento e
# acesso de características geográficas (pontos,
# retas ou polígonos).

# Shape file que vem com o pacote sf
arquivo <- system.file("shape/nc.shp", package = "sf")

# Vamos importar esse shape file para o R
tab_nc <- sf::st_read(arquivo, quiet = TRUE)
tab_nc

# Plotando o mapa
tab_nc %>%
  ggplot() +
  geom_sf()

# Preenchendo as áreas segundo uma variável
tab_nc %>%
  ggplot() +
  geom_sf(aes(fill = AREA))

# Bordas
tab_nc %>%
  ggplot() +
  geom_sf(aes(fill = AREA), color = "white")

# Textos
tab_nc %>%
  ggplot() +
  geom_sf(aes(fill = AREA), color = "white") +
  geom_sf_text(aes(label = NAME), size = 1.5, color = "white")


# Mapas do brasil

# install.packages("geobr")

# Brasil

tab_brasil <- geobr::read_country()

tab_brasil %>%
  ggplot() +
  geom_sf()

# Estados brasileiros

tab_estados <- geobr::read_state()

tab_estados %>%
  ggplot() +
  geom_sf() +
  geom_sf_text(aes(label = abbrev_state), size = 2)

# Simplificando os polígonos

# dTolerance > 0
# Quanto maior, mais simples serão os
# polígonos
tab_estados %>%
  sf::st_simplify(dTolerance = 0.1) %>%
  ggplot() +
  geom_sf() +
  geom_sf_text(aes(label = abbrev_state), size = 2)

# Usando labels
tab_estados %>%
  sf::st_simplify(dTolerance = 0.1) %>%
  ggplot() +
  geom_sf() +
  geom_sf_label(aes(label = abbrev_state), size = 2)


# Trazendo mais informações

tab_covid <- readr::read_rds("data/covid.rds")

tab_estados <- sf::st_simplify(tab_estados, dTolerance = 0.1)

tab_covid_estado <- tab_covid %>%
  filter(
    data == max(data),
    is.na(codmun),
    !is.na(estado)
  ) %>%
  right_join(tab_estados, by = c("estado" = "abbrev_state"))

tab_covid_estado %>%
  ggplot() +
  geom_sf(aes(geometry = geom, fill = obitosAcumulado)) +
  geom_sf_label(aes(geometry = geom, label = estado), size = 2)

# Mudando a cor
tab_covid_estado %>%
  ggplot() +
  geom_sf(aes(geometry = geom, fill = obitosAcumulado)) +
  geom_sf_label(aes(geometry = geom, label = estado), size = 2) +
  scale_fill_gradient(low = "yellow", high = "red")



# Temas -------------------------------------------------------------------

rick_and_morty <- readr::read_rds("data/rick_and_morty.rds")

img_rick <- png::readPNG("data-raw/rick.png") %>%
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


# Extensões do ggplot2 ----------------------------------------------------

# Veja uma lista de extensões do ggplot2
# https://exts.ggplot2.tidyverse.org/gallery/

# gganimate

# install.packages("gganimate")
library(gganimate)

tab_covid <- readr::read_rds("data/covid.rds")

tab_covid %>%
  filter(is.na(codmun), !is.na(estado), estado == "SP") %>%
  ggplot(aes(x = data, y = obitosAcumulado)) +
  geom_line()


tab_covid %>%
  filter(is.na(codmun), !is.na(estado)) %>%
  mutate(estado = forcats::fct_reorder(estado, obitosAcumulado, max)) %>%
  ggplot(aes(x = estado, y = obitosAcumulado, fill = estado)) +
  geom_col() +
  coord_flip() +
  labs(
    title = 'Dia: {closest_state}',
    x = 'Estado',
    y = 'Número de casos acumulado'
  ) +
  transition_states(
    data,
    transition_length = 2,
    state_length = 1
  ) +
  ease_aes("linear")

 # gghighlight

## ggalluvial
