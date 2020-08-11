library(dplyr)

# -------------------------------------------------------------------------
# Misc --------------------------------------------------------------------
# -------------------------------------------------------------------------

# bind_rows ---------------------------------------------------------------

# Juntando duas bases
imdb_2015 <- readr::read_rds("data-raw/rds/imdb_2015.rds")
imdb_2016 <- readr::read_rds("data-raw/rds/imdb_2016.rds")

bind_rows(imdb_2015, imdb_2016)
rbind(imdb_2015, imdb_2016)

# Juntando várias bases

arquivos <- list.files("data-raw/rds/imdb_por_ano/", full.names = TRUE)

arquivos %>%
  purrr::map(readr::read_rds) %>%
  bind_rows()

# rbind() não funciona com listas
arquivos %>%
  purrr::map(readr::read_rds) %>%
  rbind()

# Juntando bases com colunas diferentes

tab1 <- tibble::tibble(
  var1 = c(1, 2, 3),
  var2 = c("a", "b", "c"),
  var3 = c(10, 20, 30)
)

tab2 <- tibble::tibble(
  var2 = c("d", "e", "f"),
  var1 = c(4, 5, 6)
)

bind_rows(tab1, tab2)
rbind(tab1, tab2)

# A função bind_cols() possui propriedades equivalentes
# para a tarefa de juntar colunas.


# case_when ---------------------------------------------------------------



# coalesce ----------------------------------------------------------------

tab <- tibble::tibble(
  var1 = c(1, 2, NA, 2, 10, NA, NA),
  var2 = c(NA, 2, 2, 10, 3, 0, NA)
)

tab %>%
  mutate(var3 = coalesce(var1, var2))

# Você também pode usar para substituir os NAs de
# uma variável por um único valor

tab %>%
  mutate(var3 = coalesce(var1, 33))

# Mas, neste caso, você também poderia usar a função
# tidyr::replace_na().

tab %>%
  tidyr::replace_na(replace = list(var1 = 33, var2 = 66))


# lag, lead ---------------------------------------------------------------

tab <- tibble::tibble(
  tempo = c(1, 2, 3, 4, 5),
  var = c(1, 4, 10, 33, 20)
)

# Pegar valor defasado e valor futuro
tab %>%
  dplyr::mutate(
    var_lag1 = lag(var),
    var_lead1 = lead(var)
  )

# Muito útil para testes
tab %>%
  dplyr::mutate(
    teste = var > lead(var)
  )

# pull --------------------------------------------------------------------

# Devolve uma coluna da base (como vetor)
mtcars %>%
  pull(mpg)

# first, last -------------------------------------------------------------

# Retornam o primeiro e último valor de um vetor

x <- c(1, 12, 30, 41, 15)

first(x)
last(x)

# São funções úteis quando temos algum tipo de ordem
tab <- tibble::tibble(
  tempo = c(1, 2, 3, 4, 5, 12, 2, 3, 4, 5, 1, 2, 3, 4, 5),
  var = c(1, 4, 10, 33, 20, 1, 3, 0, 3, 21, 12, 7, 9, 17, 2),
  grupo = c(rep("a", 5), rep("b", 5), rep("c", 5))
)

tab %>%
  group_by(grupo) %>%
  arrange(tempo, .by_group = TRUE) %>%
  mutate(
    inicio = first(var),
    fim = last(var)
  )


# na_if -------------------------------------------------------------------

tab <- tibble::tibble(
  var = c(1, 10, 2, -99, 10, -99)
)

tab %>%
  mutate(var = na_if(var, -99))


# sample_n, sample_frac ---------------------------------------------------

# Pega amostra de tamanho 10 da base mtcars
sample_n(mtcars, 10)

# Pega 50% da base mtcars
sample_frac(mtcars, 0.5)


# -------------------------------------------------------------------------
# dplyr 1.0 ---------------------------------------------------------------
# -------------------------------------------------------------------------

# Vamos utilizar a base ames

ames <- readr::read_rds("data/ames.rds")

glimpse(ames)

# Documentação

# remotes::install_github("curso-r/basesCursoR")
help(ames, package = "basesCursoR")

# across ------------------------------------------------------------------

# A função across substitui a família de verbos
# _all_, _if, _at

# Para sumarizar a base para mais de uma variável,
# antigamente fazíamos
ames %>%
  group_by(geral_qualidade) %>%
  summarise(
    lote_area_media = mean(lote_area, na.rm = TRUE),
    venda_valor_medio = mean(venda_valor, na.rm = TRUE)
  )

# ou
ames %>%
  group_by(geral_qualidade) %>%
  summarise_at(
    .vars = vars(lote_area, venda_valor),
    ~mean(.x, na.rm = TRUE)
  )

# Agora, usamos a função across
starwars %>%
  group_by(homeworld) %>%
  summarise(across(
    .cols = c(lote_area, venda_valor),
    .fns = mean, na.rm = TRUE
  ))

# Podemos aplicar facilmente uma função a todas
# as colunas de uma base
ames %>%
  summarise(across(.fns = n_distinct))

# Antes usávamos o sufixo "_all"
ames %>%
  summarise_all(.funs = ~n_distinct(.x))

# where -------------------------------------------------------------------

# o where é uma nova opção do framework "tidyselect"
# para seleção de colunas

# Com ele, podemos aplicar uma função a todas as
# colunas de um tipo
ames %>%
  summarise(across(where(is.character), n_distinct))

ames %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# Antes fazíamos
ames %>%
  summarise_if(is.character, n_distinct)

ames %>%
  summarise_if(is.numeric, ~mean(.x, na.rm = TRUE))

# Agora, podemos fazer sumarizações complexas
ames %>%
  group_by(fundacao_tipo) %>%
  summarise(
    across(contains("area"), mean, na.rm = TRUE),
    across(where(is.character), ~sum(is.na(.x))),
    n_obs = n(),
  ) %>%
  View()

# Isso não era possível com as funções antigas!

# Também podemos usar across/where com os outros
# verbos do dplyr

# mutate()

# Vamos transformar todas as colunas de área
# para obtermos medidas em metros quadrados
# em vez de pés quadrados.
ames %>%
  mutate(across(
    contains("area"),
    ~ .x / 10.764
  ))

# filter()
# Pegar todas as casas com varanda aberta,
# cerca e lareira
ames %>%
  filter(across(
    c(varanda_aberta_area, cerca_qualidade, lareira_qualidade),
    ~!is.na(.x)
  )) %>%
  # select(c(varanda_aberta_area, cerca_qualidade, lareira_qualidade)) %>%
  View()


# Para selecionar colunas, não usamos across!
ames %>%
  select(where(is.numeric))

# -------------------------------------------------------------------------
# tidyr -------------------------------------------------------------------
# -------------------------------------------------------------------------

# unite -------------------------------------------------------------------


# separate ----------------------------------------------------------------


# pivotagem ---------------------------------------------------------------

# pivot_wider

# pivot_longer


# nest/unnest -------------------------------------------------------------








