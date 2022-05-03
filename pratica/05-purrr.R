# Motivação: ler e empilhar as bases IMDB separadas por ano.

library(dplyr)
library(purrr)


# abrindo só um arquivo
read_rds("data-raw/imdb_por_ano/imdb_1916.rds")

# criando um vetor dos arquivos por ano da base do imdb
# com base R
arquivos <- list.files(path = "data-raw/imdb_por_ano",
                       pattern = ".rds$",
                       full.names = TRUE)

# com o pacote fs
arquivos_com_fs <- fs::dir_ls(path = "data-raw/imdb_por_ano", glob = "*.rds")


# estrutura com purrr:
#   map(vetor, funcao)

# só com map: gera uma lista de tibbles
lista_de_tibbles <- map(arquivos, read_rds)


# com bind_rows: gera uma tibble
tibbles_unica <- map(arquivos, read_rds) %>%
  bind_rows()

# com map_dfr, nao é preciso usar o bind_rows
imdb_purrr <- map_dfr(arquivos, read_rds)

# -------------------------------------------------------------------------

# Motivação: fazer gráficos de dispersão do orçamento vs receita
# para todos os anos da base

library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

imdb <- readr::read_rds("data/imdb.rds")

# nest
# agrupar e fazer um nest  (aninhando pelo grupo) por ano
imdb_nest <- imdb %>%
  group_by(ano) %>%
  nest()

# unnest

imdb_nest %>%
  unnest(cols = "data")

# podemos manipular list-columns usando a
# a funcão purrr::map()

# funcao para fazer o grafico!
fazer_grafico_dispersao <- function(tab) {
  tab %>%
    ggplot(aes(x = orcamento, y = receita)) +
    geom_point()
}

# experimentando a funcao
fazer_grafico_dispersao(imdb)

# usando map para gerar um gráfico por ano
imdb_graficos <- imdb %>%
  group_by(ano) %>%
  nest() %>%
  mutate(
    grafico = purrr::map(data, fazer_grafico_dispersao)
  )

# acessando os gráficos com base
imdb_graficos$grafico[[1]]

# acessando os gráficos com o pluck
pluck(imdb_graficos, "grafico", 1)


# também poderíamos rodar um modelo para
# vários grupos

# base que usaremos
mtcars

# exemplo de uso da funcao lm
lm(mpg ~ ., data = mtcars) %>%
  broom::tidy()

# criando uma função para rodar o lm

rodar_modelo <- function(tab) {
  lm(mpg ~ ., data = tab) |>
    broom::tidy()
}

# agrupando, fazendo nest (aninhando pelo grupo), e usando map para rodar um
# modelo por grupo

tab_modelos <- mtcars %>%
  group_by(cyl) %>%
  nest() %>%
  mutate(
    modelo = map(data, rodar_modelo)
  )

# usando pluck para acessar os resultados
pluck(modelos_cyl, "modelo", 1)


# vendo os resultados para todos os grupos
modelos_cyl  %>% unnest(cols = modelo)

# e se quiser para algum grupo especifico?
# continua sendo uma tibble! podemos filtrar

modelos_cyl %>% filter(cyl == 8) %>% unnest(cols = modelo)

# acessando os resultados com R base
tab_modelos$modelo[[3]]


summary(tab_modelos$modelo[[3]])


# outra forma (mais direta), sem criar uma função!!

# criando uma função anônima com ~ (o argumento tem que ser .x)
tab_modelos_2 <- mtcars %>%
  group_by(cyl) %>%
  nest() %>%
  mutate(
    modelo = map(data, ~ broom::tidy(lm(mpg ~ ., data = .x)))
    )

tab_modelos_2

tab_modelos_2$modelo

# criando uma função anônima com \() (o argumento pode ter o nome que quisermos,
# e podemos ter vários argumentos)
mtcars %>%
  group_by(cyl) %>%
  nest() %>%
  mutate(
    modelo = map(data, \(x) broom::tidy(lm(mpg ~ ., data = x)))
    )

# group_split -----------------

# outro exemplo do gráfico de dispersão: com a função group_split()

# primeiro para entender o que o group_split faz:
# vai separar por grupos, cada grupo vira uma lista.
imdb %>%
  group_split(ano)


# podemos usar um map para aplicar uma função em cada elemento da lista.
# ex: contar numero de linhas
imdb %>%
  group_split(ano) %>%
  map(nrow)


# criando uma função anonima para fazer um ggplot:

imdb_split_grafico <- imdb %>%
  drop_na(orcamento, receita) %>%
  group_split(ano) %>%
  map(~ ggplot(data = .x) +
        aes(x = orcamento, y = receita) +
        geom_point() +
        facet_wrap(~ano))

# acessando os gráficos com pluck
grafico_2016 <- pluck(imdb_split_grafico, 69)

# DICA: Podemos acessar os dados usados para gerar um grafico
# usando: objeto_ggplot$data
# exemplo:
grafico_2016$data

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



