# Motivação: ler e empilhar as bases IMDB separadas por ano.

arquivos <- list.files("data/imdb_por_ano/", full.names = TRUE)

arquivos %>%
  purrr::map(readr::read_rds) %>%
  bind_rows()

arquivos %>%
  purrr::map_dfr(readr::read_rds)

# -------------------------------------------------------------------------



# Motivação: fazer gráficos de dispersão do orçamento vs receita
# para todos os anos da base

# nest

imdb_nest <- imdb %>%
  group_by(ano) %>%
  nest()

# unnest

imdb_nest %>%
  unnest(cols = "data")

# podemos manipular list-columns usando a
# a funcão purrr::map()

fazer_grafico_dispersao <- function(tab) {
  tab %>%
    ggplot(aes(x = orcamento, y = receita)) +
    geom_point()
}

imdb_graficos <- imdb %>%
  group_by(ano) %>%
  nest() %>%
  mutate(
    grafico = purrr::map(data, fazer_grafico_dispersao)
  )

imdb_graficos$grafico[[1]]
