# Motivação: ver quais colunas possuem NAs e quantos

library(dplyr)

casas <- readr::read_rds("data/casas.rds")

View(casas)

# Vendo colunas com NA

casas %>%
  summarise(
    across(
      .cols = everything(),
      ~sum(is.na(.x))
    )
  ) %>%
  View()

# Deixando no formato longo

casas %>%
  summarise(
    across(
      .cols = everything(),
      ~sum(is.na(.x))
    )
  ) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "variavel",
    values_to = "num_na"
  ) %>%
  arrange(desc(num_na))

# -------------------------------------------------------------------------
# Motivação: ver o número de categorias
# distintas em cada variável categórica ***

library(dplyr)

casas <- readr::read_rds("data/casas.rds")

casas %>%
  select(where(is.character)) %>%
  summarise(
    across(
      .fns = n_distinct
    )
  ) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "variavel",
    values_to = "num_categs"
  ) %>%
  arrange(desc(num_categs))


# -------------------------------------------------------------------------
# Motivação: substituir todos os NAs das variáveis
# categóricas por "sem informação" ***

casas %>%
  mutate(
    across(
      where(is.character),
      tidyr::replace_na,
      replace = "sem informação"
    )
  ) %>% View()


# -------------------------------------------------------------------------

# Motivação: Descobrir o ator com o maior lucro médio na base IMDB,
# considerando as 3 colunas de elenco.

library(tidyr)
library(dplyr)

imdb <- readr::read_rds("data/imdb.rds")


imdb %>%
  pivot_longer(
    cols = starts_with("ator"),
    names_to = "posicao",
    values_to = "ator"
  ) %>%
  mutate(lucro = receita - orcamento) %>%
  group_by(ator) %>%
  summarise(lucro_medio = mean(lucro, na.rm = TRUE)) %>%
  arrange(desc(lucro_medio))

# usando slice

imdb %>%
  pivot_longer(
    cols = starts_with("ator"),
    names_to = "posicao",
    values_to = "ator"
  ) %>%
  mutate(lucro = receita - orcamento) %>%
  group_by(ator) %>%
  summarise(lucro_medio = mean(lucro, na.rm = TRUE)) %>%
  slice_max(lucro_medio, n = 1)

# vendo número de filmes

imdb %>%
  pivot_longer(
    cols = starts_with("ator"),
    names_to = "posicao",
    values_to = "ator"
  ) %>%
  mutate(lucro = receita - orcamento) %>%
  group_by(ator) %>%
  summarise(
    lucro_medio = mean(lucro, na.rm = TRUE),
    n_filmes = n()
  ) %>%
  slice_max(lucro_medio, n = 10)

# pegando o maior lucro entre
# quem fez mais de 10 filmes

imdb %>%
  pivot_longer(
    cols = starts_with("ator"),
    names_to = "posicao",
    values_to = "ator"
  ) %>%
  mutate(lucro = receita - orcamento) %>%
  group_by(ator) %>%
  summarise(
    lucro_medio = mean(lucro, na.rm = TRUE),
    n_filmes = n()
  ) %>%
  filter(n_filmes > 10) %>%
  slice_max(lucro_medio, n = 10)

# -------------------------------------------------------------------------

# Motivação: Fazer gráficos de dispersão do lucro vs todas as
# outras variáveis núméricas da base IMDB ***

library(tidyr)
library(dplyr)
library(ggplot2)

imdb <- readr::read_rds("data/imdb.rds")

imdb %>%
  mutate(lucro = receita - orcamento) %>%
  select(where(is.numeric)) %>%
  pivot_longer(
    cols = -lucro,
    names_to = "variavel",
    values_to = "valor"
  ) %>%
  ggplot(aes(x = valor, y = lucro)) +
  geom_point() +
  facet_wrap(~variavel, scales = "free")



# -------------------------------------------------------------------------

# Motivação: fazer uma tabela do lucro médio anual dos filmes
# de comédia, ação e romance (2000 a 2016) ***

library(dplyr)
library(tidyr)
library(stringr)

imdb <- readr::read_rds("data/imdb.rds")

imdb %>%
  filter(ano >= 2000) %>%
  mutate(
    lucro = receita - orcamento,
    flag_comedia = str_detect(generos, "Comedy"),
    flag_acao = str_detect(generos, "Action"),
    flag_romance = str_detect(generos, "Romance")
  ) %>%
  select(ano, lucro, starts_with("flag")) %>%
  pivot_longer(
    starts_with("flag"),
    names_to = "genero",
    values_to = "flag"
  ) %>%
  filter(flag) %>%
  group_by(genero, ano) %>%
  summarise(lucro_medio = mean(lucro, na.rm = TRUE)) %>%
  pivot_wider(names_from = ano, values_from = lucro_medio) %>%
  View()
stringr::str_subset()
# -------------------------------------------------------------------------


# Motivação: calcular o lucro médio por gênero do filme
# na base IMDB. ***

library(dplyr)
library(tidyr)
library(stringr)

imdb <- readr::read_rds("data/imdb.rds")

imdb %>%
  mutate(
    generos = str_split(generos, "\\|")
  ) %>%
  View()

imdb %>%
  mutate(
    generos = str_split(generos, "\\|")
  ) %>%
  unnest(generos) %>%
  View()

imdb %>%
  mutate(
    lucro = receita - orcamento,
    generos = str_split(generos, "\\|")
  ) %>%
  unnest(generos) %>%
  group_by(generos) %>%
  summarise(lucro_medio = mean(lucro, na.rm = TRUE))

# Refazendo exemplo anterior

imdb %>%
  filter(ano >= 2000) %>%
  mutate(
    lucro = receita - orcamento,
    generos = str_split(generos, "\\|")
  ) %>%
  unnest(generos) %>%
  filter(generos %in% c("Action", "Comedy", "Romance")) %>%
  group_by(generos, ano) %>%
  summarise(lucro_medio = mean(lucro, na.rm = TRUE)) %>%
  pivot_wider(names_from = ano, values_from = lucro_medio) %>%
  View()


# -------------------------------------------------------------------------

# Motivação: número de espécies em cada filme do Star Wars ***

library(dplyr)
library(tidyr)
library(ggplot2)

glimpse(starwars)

starwars %>%
  select(films, species) %>%
  unnest(films) %>%
  count(films) %>%
  mutate(films = forcats::fct_reorder(films, n)) %>%
  ggplot(aes(y = films, x = n)) +
  geom_col()
