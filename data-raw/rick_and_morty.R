## code to prepare `rick_and_morty` dataset goes here

url <- "https://en.wikipedia.org/wiki/List_of_Rick_and_Morty_episodes"

res <- httr::GET(url)

wiki_page <- httr::content(res)

tab <- wiki_page %>%
  xml2::xml_find_all(".//table") %>%
  magrittr::extract(2:5) %>%
  rvest::html_table(fill = TRUE) %>%
  purrr::map(janitor::clean_names) %>%
  purrr::map(~dplyr::rename_with(.x, ~stringr::str_remove(.x, "_36"))) %>%
  dplyr::bind_rows()

rick_and_morty <- tab %>%
  mutate(
    title = stringr::str_remove_all(title, '\\"'),
    u_s_viewers_millions  = stringr::str_remove(
      u_s_viewers_millions,
      "\\[.*\\]"
    ),
    u_s_viewers_millions = as.numeric(u_s_viewers_millions),
    original_air_date = stringr::str_extract(
      original_air_date,
      "\\(.*\\)"
    ),
    original_air_date = stringr::str_remove_all(
      original_air_date,
      "\\(|\\)"
    ),
    original_air_date = lubridate::as_date(original_air_date)
  ) %>%
  tibble::as_tibble()


readr::write_rds(tab, "data/rick_and_morty_raw.rds")
readr::write_rds(rick_and_morty, "data/rick_and_morty.rds")
