
# Motivação: descobrir a quantos gêneros cada filme pertence ***

library(dplyr)
library(stringr)

imdb <- readr::read_rds("data/imdb.rds")

imdb %>%
  mutate(
    num_generos = str_count(generos, pattern = "\\|") + 1
  ) %>%
  select(generos, num_generos)

# -------------------------------------------------------------------------

# Motivação: extrair o subtítulos dos filmes ***

library(dplyr)
library(stringr)

imdb <- readr::read_rds("data/imdb.rds")
imdb %>%
  mutate(
    sub_titulo = str_extract(titulo, ": .*"),
    sub_titulo = str_remove(sub_titulo, ": ")
  ) %>%
  filter(!is.na(sub_titulo)) %>%
  select(titulo, sub_titulo)

# -------------------------------------------------------------------------

# Motivação: criar uma tabela apenas com filmes cujo
# título comece com um número ***

library(dplyr)
library(stringr)

imdb <- readr::read_rds("data/imdb.rds")
imdb %>%
  filter(str_detect(titulo, "^[0-9]")) %>%
  View()

# -------------------------------------------------------------------------

# Motivação: qual a idade mínima para ver cada filme? ***

library(dplyr)
library(stringr)

imdb <- readr::read_rds("data/imdb.rds")

unique(imdb$classificacao)

imdb %>%
  mutate(
    idade_min = str_extract(classificacao, "[0-9]+"),
    idade_min = as.numeric(idade_min),
    idade_min = case_when(
      classificacao == "Livre" ~ 0,
      classificacao == "Outros" ~ NA_real_,
      TRUE ~ idade_min
    )
  ) %>%
  select(classificacao, idade_min) %>%
  View()


# -------------------------------------------------------------------------

# Motivação: Baixando e limpando dados do Rick and Morty

library(dplyr)
library(stringr)

### scrape ####
url <- "https://en.wikipedia.org/wiki/List_of_Rick_and_Morty_episodes"

res <- httr::GET(url)

wiki_page <- httr::content(res)

lista_tab <- wiki_page %>%
  xml2::xml_find_all(".//table") %>%
  magrittr::extract(2:5) %>%
  rvest::html_table(fill = TRUE) %>%
  purrr::map(janitor::clean_names) %>%
  purrr::map(~dplyr::rename_with(.x, ~stringr::str_remove(.x, "_37")))

num_temporadas <- 1:length(lista_tab)

tab <- lista_tab %>%
  purrr::map2(num_temporadas, ~dplyr::mutate(.x, no_season = .y)) %>%
  dplyr::bind_rows()
################

rick_and_morty <- tab %>%
  dplyr::relocate(no_season, .before = no_inseason) %>%
  dplyr::mutate(
    # Removendo aspas do título
    title = stringr::str_remove_all(title, '\\"'),

    # Removendo colchetes da audiência
    u_s_viewers_millions  = stringr::str_remove(
      u_s_viewers_millions,
      "\\[.*\\]"
    ),
    u_s_viewers_millions = as.numeric(u_s_viewers_millions),

    # Extraindo data formatada
    original_air_date = stringr::str_extract(
      original_air_date,
      "\\([0-9-]*\\)"
    ),
    original_air_date = stringr::str_remove_all(
      original_air_date,
      "\\(|\\)"
    ),
    original_air_date = lubridate::as_date(original_air_date)
  ) %>%
  dplyr::select(
    num_episodio = no_overall,
    num_temporada = no_season,
    num_dentro_temporada = no_inseason,
    titulo = title,
    direcao = directed_by,
    roteiro = written_by,
    data_transmissao_original = original_air_date,
    qtd_espectadores_EUA = u_s_viewers_millions
  ) %>%
  tibble::as_tibble()
