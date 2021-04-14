# Movitação: ver qual o gênero mais lucrativo na
# base IMDB

library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)

imdb <- readr::read_rds("data/imdb.rds")

imdb <- imdb %>%
  mutate(lucro = receita - orcamento)

# solução ingênua
imdb %>%
  group_by(generos) %>%
  summarise(lucro_medio = mean(lucro, na.rm = TRUE)) %>%
  ggplot(aes(x = generos, y = lucro_medio)) +
  geom_col()

# criando list-column
imdb %>%
  mutate(generos = str_split(generos, "\\|")) %>%
  View()

imdb %>%
  mutate(generos = str_split(generos, "\\|")) %>%
  unnest(generos) %>%
  View()

# gerando gráfico
imdb %>%
  mutate(generos = str_split(generos, "\\|")) %>%
  unnest(generos) %>%
  group_by(generos) %>%
  summarise(lucro_medio = mean(lucro, na.rm = TRUE)) %>%
  ggplot(aes(y = generos, x = lucro_medio)) +
  geom_col()

# ordenando colunas
imdb %>%
  mutate(generos = str_split(generos, "\\|")) %>%
  unnest(generos) %>%
  group_by(generos) %>%
  summarise(lucro_medio = mean(lucro, na.rm = TRUE)) %>%
  filter(!is.na(lucro_medio)) %>%
  mutate(generos = fct_reorder(generos, lucro_medio)) %>%
  ggplot(aes(y = generos, x = lucro_medio)) +
  geom_col()

grafico_lucro_medio <- function(tab) {
  tab %>%
    group_by(generos) %>%
    summarise(lucro_medio = mean(lucro, na.rm = TRUE)) %>%
    filter(!is.na(lucro_medio)) %>%
    mutate(generos = fct_reorder(generos, lucro_medio)) %>%
    ggplot(aes(y = generos, x = lucro_medio)) +
    geom_col()
}

# frequencia dos generos
imdb %>%
  mutate(generos = str_split(generos, "\\|")) %>%
  unnest(generos) %>%
  count(generos) %>%
  arrange(n)


# agrupando níveis menos frequentes

#### 15 gêneros mais frequentes
imdb %>%
  mutate(generos = str_split(generos, "\\|")) %>%
  unnest(generos) %>%
  mutate(generos = fct_lump(generos, n = 15)) %>%
  count(generos) %>%
  arrange(n)

imdb %>%
  mutate(generos = str_split(generos, "\\|")) %>%
  unnest(generos) %>%
  mutate(generos = fct_lump(generos, n = 15)) %>%
  grafico_lucro_medio()

#### gêneros que representam mais de 1% dos casos
imdb %>%
  mutate(generos = str_split(generos, "\\|")) %>%
  unnest(generos) %>%
  mutate(generos = fct_lump(generos, prop = 0.01)) %>%
  count(generos) %>%
  arrange(n)


#### gêneros com mais de 10 filmes
imdb %>%
  mutate(generos = str_split(generos, "\\|")) %>%
  unnest(generos) %>%
  mutate(generos = fct_lump_min(generos, min = 10)) %>%
  count(generos) %>%
  arrange(n)

imdb %>%
  mutate(generos = str_split(generos, "\\|")) %>%
  unnest(generos) %>%
  mutate(generos = fct_lump_min(generos, min = 10)) %>%
  grafico_lucro_medio()


# agrupar os níveis menos frequentes, garantindo
# que "other" seja o nível menos frequente
imdb %>%
  mutate(generos = str_split(generos, "\\|")) %>%
  unnest(generos) %>%
  mutate(generos = fct_lump_lowfreq(generos)) %>%
  count(generos) %>%
  arrange(n)

imdb %>%
  mutate(generos = str_split(generos, "\\|")) %>%
  unnest(generos) %>%
  mutate(generos = fct_lump_lowfreq(generos)) %>%
  grafico_lucro_medio()


# Repetindo análise por diretor

direcao <-c(
  "Lana Wachowski",
  "Quentin Tarantino",
  "Spike Lee",
  "Sofia Coppola"
)

imdb %>%
  filter(diretor %in% direcao) %>%
  mutate(generos = str_split(generos, "\\|")) %>%
  unnest(generos) %>%
  mutate(generos = fct_lump_lowfreq(generos)) %>%
  group_by(generos, diretor) %>%
  summarise(total_receita = sum(receita, na.rm = TRUE)) %>%
  filter(!is.na(total_receita)) %>%
  ggplot(aes(x = total_receita, y = diretor, fill = generos)) +
  geom_col(position = "dodge", show.legend = FALSE) +
  geom_text(
    aes(label = generos, x = 3),
    position = position_dodge(width = 0.9),
    size = 3
  )

# Mudando ordem dos diretores no gráfico

imdb %>%
  filter(diretor %in% direcao) %>%
  mutate(generos = str_split(generos, "\\|")) %>%
  unnest(generos) %>%
  mutate(generos = fct_lump_lowfreq(generos)) %>%
  group_by(generos, diretor) %>%
  summarise(
    total_receita = sum(receita, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(total_receita)) %>%
  mutate(
    diretor = lvls_reorder(diretor, c(4, 1, 3, 2))
  ) %>%
  ggplot(aes(x = total_receita, y = diretor, fill = generos)) +
  geom_col(position = "dodge", show.legend = FALSE) +
  geom_text(
    aes(label = generos, x = total_receita),
    position = position_dodge(width = 0.9),
    size = 3,
    hjust = 0
  )




