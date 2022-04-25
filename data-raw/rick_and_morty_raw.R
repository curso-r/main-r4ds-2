## code to prepare `rick_and_morty` dataset goes here

# carregar o pacote que contém o pipe
library(magrittr)

# faz o scraper
url <- "https://en.wikipedia.org/wiki/List_of_Rick_and_Morty_episodes"

res <- httr::GET(url)

wiki_page <- httr::content(res)

lista_tab <- wiki_page %>%
  xml2::xml_find_all(".//table") %>%
  magrittr::extract(2:6) %>%
  rvest::html_table(fill = TRUE) %>%
  purrr::map(janitor::clean_names) %>%
  purrr::map(~dplyr::rename_with(.x, ~stringr::str_remove(.x, "_37|_3")))

num_temporadas <- 1:length(lista_tab)

tab <- lista_tab %>%
  purrr::map2(num_temporadas, ~dplyr::mutate(.x, no_season = .y)) %>%
  dplyr::bind_rows()

# arrumar a base

rick_and_morty <- tab %>%
  dplyr::relocate(no_season, .before = no_inseason) %>%
  dplyr::mutate(

    title = stringr::str_remove_all(title, '\\"'),
    u_s_viewers_millions  = stringr::str_remove(
      u_s_viewers_millions,
      "\\[.*\\]"
    ),
    u_s_viewers_millions = as.numeric(u_s_viewers_millions),
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

# Objetivo: Limpar a base `tab` e criar essa tabela:
# > dplyr::glimpse(rick_and_morty)
# Rows: 51
# Columns: 8
# $ num_episodio              <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,…
# $ num_temporada             <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,…
# $ num_dentro_temporada      <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 1, 2, 3, 4, 5, 6, 7, 8, 9, …
# $ titulo                    <chr> "Pilot", "Lawnmower Dog", "Anatomy Park", "M. Night Shaym-Alie…
# $ direcao                   <chr> "Justin Roiland", "John Rice", "John Rice", "Jeff Myers", "Bry…
# $ roteiro                   <chr> "Dan Harmon & Justin Roiland", "Ryan Ridley", "Eric Acosta & W…
# $ data_transmissao_original <date> 2013-12-02, 2013-12-09, 2013-12-16, 2014-01-13, 2014-01-20, 2…
# $ qtd_espectadores_EUA      <dbl> 1.10, 1.51, 1.30, 1.32, 1.61, 1.75, 1.76, 1.48, 1.54, 1.75, 2.…
