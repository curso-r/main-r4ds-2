
# ggplot2 -----------------------------------------------------------------

library(dplyr)
library(ggplot2)

ames <- readr::read_rds("data/ames.rds")

# Qualidade vs valor da venda

ames %>%
  ggplot() +
  geom_point(aes(x = geral_qualidade, y = venda_valor))

# Melhorando visual do gráfico

ames %>%
  ggplot() +
  geom_point(aes(x = geral_qualidade, y = venda_valor)) +
  labs(x = "Qualidade", y = "Valor da venda ($)") +
  scale_x_continuous(breaks = 1:10) +
  theme_minimal()

# Transformando "geral_qualidade" em
# uma variável categórica e mudando
# escala do valor

ames %>%
  mutate(
    geral_qualidade = as.character(geral_qualidade),
    venda_valor = venda_valor / 1000
  ) %>%
  ggplot() +
  geom_boxplot(aes(x = geral_qualidade, y = venda_valor)) +
  labs(x = "Qualidade", y = "Valor da venda (em milhares de dólares)") +
  theme_minimal()

# Transformando em fatores

ames %>%
  mutate(
    geral_qualidade = as.factor(geral_qualidade),
    venda_valor = venda_valor / 1000
  ) %>%
  ggplot() +
  geom_boxplot(aes(x = geral_qualidade, y = venda_valor)) +
  labs(x = "Qualidade", y = "Valor da venda (em milhares de dólares)") +
  theme_minimal()


# Temas -------------------------------------------------------------------



# Mapas com ggplot2 -------------------------------------------------------


