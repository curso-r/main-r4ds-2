## code to prepare `rick_and_morty` dataset goes here

# scrape

library(magrittr)

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

# tidy

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


readr::write_rds(tab, "data/rick_and_morty_raw.rds")
readr::write_rds(rick_and_morty, "data/rick_and_morty.rds")
