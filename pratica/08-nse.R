# Motivação: fazer uma função que recebe uma tab e uma coluna
# e devolve uma tab sem as linhas com NA na coluna especificada

library(dplyr)

imdb <- readr::read_rds("data/imdb.rds")

filtrar_na <- function(tab, coluna) {
  tab %>%
    filter(!is.na(coluna))
}

filtrar_na(imdb, orcamento)

filtrar_na <- function(tab, coluna) {
  tab %>%
    filter(!is.na({{coluna}}))
}

filtrar_na(imdb, orcamento)
filtrar_na(imdb, "orcamento")

filtrar_na <- function(tab, coluna) {
  tab %>%
    filter(!is.na(.data[[coluna]]))
}

filtrar_na(imdb, "orcamento")
filtrar_na(imdb, orcamento)

# Motivação: fazer uma função que recebe uma tab e uma coluna
# e faz um gráfico de barras da frequência dessa coluna

library(dplyr)
library(ggplot2)

criar_grafico <- function(tab, coluna) {
  tab %>%
    count({{coluna}}) %>%
    ggplot(aes(x = {{coluna}}, y = n)) +
    geom_col()
}

criar_grafico(mtcars, cyl)

criar_grafico <- function(tab, coluna) {
  tab %>%
    count({{coluna}}) %>%
    ggplot(aes(x = {{coluna}}, y = n)) +
    geom_col()
}

criar_grafico(mtcars, cyl)

# Motivação: fazer uma função que recebe uma coluna
# numérica da base IMDB e devolve um gráfico de dispersão
# do lucro vs coluna especificada

criar_grafico_disp <- function(coluna) {
  imdb %>%
    mutate(lucro = receita - orcamento) %>%
    ggplot(aes(x = {{coluna}}, y = lucro)) +
    geom_point()
}

criar_grafico_disp(receita)
criar_grafico_disp(nota_imdb)

# Motivação: fazer uma função que recebe uma coluna
# categórica da base IMDB e devolve uma tabela
# com o lucro médio para cada categoria dessa coluna

tabela_lucro_medio <- function(coluna) {
  imdb %>%
    mutate(lucro = receita - orcamento) %>%
    group_by({{coluna}}) %>%
    summarise(lucro_medio = mean(lucro, na.rm = TRUE))
}

tabela_lucro_medio(cor)
tabela_lucro_medio(diretor) %>%
  arrange(desc(lucro_medio))
