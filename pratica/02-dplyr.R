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

# Selecionando apenas colunas com NA

casas %>%
  summarise(
    across(
      .cols = everything(),
      ~sum(is.na(.x))
    )
  ) %>%
  select(
    where(~.x > 0)
  )

# Deixando no formato longo

casas %>%
  summarise(
    across(
      .cols = everything(),
      ~sum(is.na(.x))
    )
  ) %>%
  select(
    where(~.x > 0)
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
