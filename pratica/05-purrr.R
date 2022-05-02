# Motivação: ler e empilhar as bases IMDB separadas por ano.

library(dplyr)
library(purrr)

arquivos <- list.files("data/imdb_por_ano/", full.names = TRUE)

arquivos %>%
  map(readr::read_rds) %>%
  bind_rows()

arquivos %>%
  map_dfr(readr::read_rds)

# -------------------------------------------------------------------------

# Motivação: fazer gráficos de dispersão do orçamento vs receita
# para todos os anos da base

library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

imdb <- readr::read_rds("data/imdb.rds")

# nest

imdb_nest <- imdb %>%
  group_by(ano) %>%
  nest()

# unnest

imdb_nest %>%
  unnest(cols = "data")

# podemos manipular list-columns usando a
# a funcão purrr::map()

fazer_grafico_dispersao <- function(tab) {
  tab %>%
    ggplot(aes(x = orcamento, y = receita)) +
    geom_point()
}

imdb_graficos <- imdb %>%
  group_by(ano) %>%
  nest() %>%
  mutate(
    grafico = purrr::map(data, fazer_grafico_dispersao)
  )

imdb_graficos$grafico[[1]]


# também poderíamos rodar um modelo para
# vários grupos

rodar_modelo <- function(tab) {
  lm(mpg ~ ., data = tab) |>
    broom::tidy()
}

tab_modelos <- mtcars %>%
  group_by(cyl) %>%
  nest() %>%
  mutate(
    modelo = map(data, rodar_modelo)
  )

tab_modelos$modelo[[3]]
summary(tab_modelos$modelo[[3]])


# -------------------------------------------------------------------------

# Motivação: iterar uma função não-vetorizada

library(purrr)

verifica_texto <- function(x) {
  if (x != "") {
    "Texto a ser retornado"
  } else {
    NULL
  }
}

textos <- sample(c(letters, ""), 1000, replace = TRUE)

verifica_texto(textos)

map(textos, verifica_texto)



# -------------------------------------------------------------------------

# Motivação: criar coluna de pontos do time da casa
# ganhos a partir de um placar ({brasileirao}) ***
# empate dá 1 ponto; ganhar dá 3 pontos; perder dá 0 pontos

library(purrr)
library(dplyr)

remotes::install_github("williamorim/brasileirao")
brasileirao::matches

calcular_pontos <- function(placar) {
  gols <- stringr::str_split(placar, "x", simplify = TRUE)
  if (gols[1] > gols[2]) {
    return(3)
  } else if (gols[1] < gols[2]) {
    return(0)
  } else {
    return(1)
  }
}

calcular_pontos("1x1")
calcular_pontos("2x0")
calcular_pontos("1x7")

brasileirao::matches %>%
  dplyr::mutate(
    pontos_casa = purrr::map_dbl(score, calcular_pontos)
  )

# Gols pro e gols contra

brasileirao::matches %>%
  dplyr::mutate(
    pontos_casa = purrr::map_dbl(score, calcular_pontos),
    gols_casa = purrr::map_dbl(
      score,
      ~as.numeric(stringr::str_split(.x, "x", simplify = TRUE)[1])
    ),
    gols_visitante = purrr::map_dbl(
      score,
      ~as.numeric(stringr::str_split(.x, "x", simplify = TRUE)[2])
    )
  )


