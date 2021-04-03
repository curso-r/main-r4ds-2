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
# outras variáveis núméricas da base IMDB

library(tidyr)
library(dplyr)
library(ggplot2)

imdb <- readr::read_rds("data/imdb.rds")

mtcars %>%
  pivot_longer(
    cols = -mpg,
    names_to = "variavel",
    values_to = "valor"
  ) %>%
  ggplot(aes(x = valor, y = mpg)) +
  geom_point() +
  facet_wrap(~variavel, scales = "free")



# -------------------------------------------------------------------------

# Motivação: fazer uma tabela do lucro médio anual dos filmes
# de comédia, ação e romance

library(dplyr)
library(tidyr)


# -------------------------------------------------------------------------


# Motivação: calcular o lucro médio por gênero do filme na base IMDB.

library(dplyr)
library(tidyr)

imdb <- readr::read_rds("data/imdb.rds")

