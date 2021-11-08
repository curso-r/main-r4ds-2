# Motivação: fazer uma análise descritiva do Ozônio

library(dplyr)
library(lubridate)
library(ggplot2)

cetesb <- readr::read_rds("data/cetesb.rds")

# Série

cetesb %>%
  filter(poluente == "O3", estacao_cetesb == "Pinheiros") %>%
  ggplot(aes(x = data, y = concentracao)) +
  geom_line()

# Criando colunas

ozonio <- cetesb %>%
  filter(poluente == "O3", estacao_cetesb == "Pinheiros") %>%
  mutate(
    ano = year(data),
    mes = month(data),
    dia_do_mes = day(data),
    dia_semana = wday(data)
  )

# Médias anuais

ozonio %>%
  group_by(ano) %>%
  summarise(media = mean(concentracao, na.rm = TRUE))

# Médias mensais

ozonio %>%
  group_by(mes) %>%
  summarise(media = mean(concentracao, na.rm = TRUE))

ozonio %>%
  group_by(mes) %>%
  summarise(media = mean(concentracao, na.rm = TRUE)) %>%
  ggplot(aes(x = mes, y = media)) +
  geom_col()


# Série da média mensal?

ozonio %>%
  mutate(
    mes_ano = make_date(ano, mes, "01")
  ) %>%
  View()


calcular_media <- function(tab) {
  tab %>%
    summarise(media = mean(concentracao, na.rm = TRUE))
}

ozonio %>%
  mutate(
    mes_ano = make_date(ano, mes, "01")
  ) %>%
  group_by(mes_ano) %>%
  calcular_media()

ozonio %>%
  mutate(
    mes_ano = make_date(ano, mes, "01")
  ) %>%
  group_by(mes_ano) %>%
  calcular_media() %>%
  ggplot(aes(x = mes_ano, y = media)) +
  geom_line()

# Qual o hoário do dia com mais ozônio?

ozonio %>%
  group_by(hora) %>%
  calcular_media() %>%
  ggplot(aes(x = hora, y = media)) +
  geom_line()

# Por dia da semana

ozonio %>%
  group_by(hora, dia_semana) %>%
  calcular_media() %>%
  ggplot(aes(x = hora, y = media)) +
  geom_line() +
  facet_wrap(vars(dia_semana))

ozonio %>%
  mutate(dia_semana = wday(data, label = TRUE)) %>%
  group_by(hora, dia_semana) %>%
  calcular_media() %>%
  ggplot(aes(x = hora, y = media)) +
  geom_line() +
  facet_wrap(vars(dia_semana))

ozonio %>%
  mutate(dia_semana = wday(
    data,
    label = TRUE,
    locale = "pt_BR.UTF-8"
  )) %>%
  group_by(hora, dia_semana) %>%
  calcular_media() %>%
  ggplot(aes(x = hora, y = media)) +
  geom_line() +
  facet_wrap(vars(dia_semana))

# correlação com lag ***

cetesb %>%
  mutate(
    concentracao_lag3 = lag(concentracao, 3)
  ) %>%
  select(starts_with("concentracao"))

cetesb %>%
  filter(
    poluente %in% c("O3", "NO2"),
    estacao_cetesb == "Ibirapuera",
    hora == 13
  ) %>%
  select(data, hora, poluente, concentracao) %>%
  tidyr::pivot_wider(names_from = poluente, values_from = concentracao) %>%
  mutate(
    NO2 = lag(NO2, 3)
  ) %>%
  ggplot(aes(x = NO2, y = O3)) +
  geom_point()

# Movitação: ver qual o gênero mais lucrativo na
# base IMDB ***

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




