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
