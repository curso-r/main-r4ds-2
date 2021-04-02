library(dplyr)
library(tidyr)

pokemon <- readr::read_rds("data/pokemon.rds")
imdb <- readr::read_rds("data/imdb.rds")

# unite(), separate() -----------------------------------------------------

# 1. Junte as duas colunas de tipo em
# uma única coluna na base pokemon.

# 2. Junte as três colunas de cor em
# uma única coluna na base pokemon.
# Faça isso sem remover as 3 colunas
# originais.

#. 3 Crie 5 novas colunas de gêneros
# na base imdb, cada uma com um dos
# gêneros contidos na coluna generos.
# Para os filmes com menos de 5 gêneros,
# substitua os valores NA pela
# string "inexistente".


# pivot_longer(), pivot_wider() -------------------------------------------

# 4. Substitua os "????" no código abaixo
# para criar uma tabela do lucro
# médio dos filmes ao longo dos
# anos de 2000 a 2016, com cada
# ano sendo uma coluna da base.

imdb %>%
  mutate(lucro = receita - orcamento) %>%
  filter(ano %in% "????") %>%
  group_by("????") %>%
  summarise(lucro_medio = "????") %>%
  pivot_wider(names_from = "????", values_from = "????")


# 5.

# a. Utilize a função pivot_longer()
# para criar uma única coluna de tipo
# na base pokemon.


# b. Utilize a base criada no item a.
# e escreva um código para descobrir
# qual o tipo mais frequente na base,
# independentemente se ele é
# primário (tipo_1) ou secundário (tipo_2).


#. 6. (DESAFIO)
# Escreva uma função que receba
# uma base qualquer e o nome
# de uma coluna numérica dessa base
# e retorne uma figura com
# um gráfico de dispersão da
# coluna escolhida contra cada
# uma das outras variáveis numéricas
# da base.

# Estrutura da função

fazer_graficos_dispersao <- function(base, variavel) {

  tabela_longa <- base %>%
    # código para gerar a tabela ideal para o gráfico

  tabela_longa %>%
    ggplot() +
    geom_point(aes(
      #completar
    )) +
    facet_wrap(
      #completar
    )
}

# Se você rodar fazer_graficos_dispersao(mtcars, mpg)
# deverá retornar uma imagem igual a figura
# exercicios/exemplo_grafico_dispersao.png
