
# Motivação: fazer uma função que recebe uma tab e uma coluna
# e devolve uma tab sem as linhas com NA na coluna especificada
#
# objetivos:
# criar uma função
# argumentos: uma tabela e uma coluna,
# resultado: uma tabela que NÃO tenha NA na coluna que escolhemos

library(dplyr)
library(tidyr)
# vamos experimentar com a base do imdb
imdb <- readr::read_rds("data-raw/imdb.rds")

# pensar primeiro: vamos experimentar com qual coluna?
# tabela = imdb
# coluna = receita

# então vamos tentar escrever o código simples,
# fora de uma função, que faz o que o exercício espera:
# com drop_na()
imdb %>%
  drop_na(receita)

# outra opção, com filter:
imdb %>%
  filter(!is.na(receita))

# estrutura de uma função:
# nome_da_funcao <- function(argumentos){
#   código que queremos executar
# }

# passo 3: colocar o código que funciona dentro da estrutura da função
# e substituir os valores que variam pelo nome do argumento:

# PRIMEIRA TENTATIVA: o codigo a seguir ainda nao funciona!!
filtrar_na <- function(tabela, coluna) {
  tabela %>%
    filter(!is.na(coluna))
}

# experimentando a funcao:
filtrar_na(tabela = imdb, coluna = receita)


# Colocar o curly curly ao redor do argumento que é
# referente ao nome da coluna:

filtrar_na <- function(tabela, coluna) {
  tabela %>%
    filter(!is.na({{coluna}}))
}

# agora funciona com o curly curly:
imdb %>%
  filtrar_na(receita)

# experimentando a função com outras bases e colunas:

starwars %>%
  filtrar_na(hair_color)

library(dados)
pinguins %>%
  filtrar_na(sexo)

# experimente testar com alguma que você conhece!

# ----- MAS E SE ALGUÉM QUISER PASSAR O ARGUMENTO COM ASPAS??
# EX:
imdb %>%
  filtrar_na("receita")
# não filtra nada!!

# com base R, isso funciona para acessar valores de uma coluna:
imdb[["receita"]]

# se quiser usar o nome do argumento com aspas,
# em vez de usar o curly curly, podemos usar o
# .data[[ ]]
# Ex:
filtrar_na_coluna_com_aspas <- function(tabela, coluna) {
  tabela %>%
    filter(!is.na(.data[[coluna]]))
}

# agora funciona com aspas!
imdb %>%
  filtrar_na_coluna_com_aspas("receita")
# mas sem aspas nao funciona...
imdb %>%
  filtrar_na_coluna_com_aspas(receita)


# --------- DÚVIDAS: E QUANDO NÃO PRECISAMOS USAR  curly curly??

# se a gente não vai usar a coluna, nao precisa do curly curly:
filtrar_filmes_por_direcao <- function(direcao){
  imdb %>%
    filter(diretor == direcao)
}

filtrar_filmes_por_direcao("Tim Burton")

# outro exemplo onde nao precisamos usar o curly curly:
filtrar_filmes_acima_orcamento <- function(orcamento_maior_para_filtrar){
  imdb %>%
    filter(orcamento >= orcamento_maior_para_filtrar)
}

filtrar_filmes_acima_orcamento(100000000)

# Motivação: fazer uma função que recebe uma tab e uma coluna
# e faz um gráfico de barras da frequência dessa coluna

library(ggplot2)

# objetivo:
# - fazer uma função
# - recebe (argumentos): tabela e uma coluna
# - o que ela faz? gráfico de barras da frequencia dessa coluna

# passo 1: escrever um código que funciona!!
mtcars %>%
  count(cyl) %>%
  ggplot() +
  aes(x = cyl, y = n) +
  geom_col()


# passo 2: colocar dentro de uma função
# passo 3: substituir os argumentos
# passo 3: colocar curly curly ao redor dos argumentos
# que sao nomes de colunas:
criar_grafico_frequencia <- function(tabela, coluna) {
  # grafico de barras da frequencia dessa coluna!
  tabela %>%
    count({{coluna}}) %>%
    ggplot() +
    aes(x = {{coluna}}, y = n) +
    geom_col()
}

# usando a funcao!
criar_grafico_frequencia(mtcars, gear)

criar_grafico_frequencia(pinguins, sexo)

criar_grafico_frequencia(imdb, classificacao)


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
