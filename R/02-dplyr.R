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


# across ------------------------------------------------------------------

# Antigamente fazíamos
starwars %>%
  group_by(homeworld) %>%
  summarise(
    altura_media = mean(height, na.rm = TRUE),
    peso_medio = mean(mass, na.rm = TRUE),
  )

# Ou
starwars %>%
  group_by(homeworld) %>%
  summarise_at(
    .vars = vars(height, mass),
    ~mean(.x, na.rm = TRUE)
  )

# Agora, usamos a função across
starwars %>%
  group_by(homeworld) %>%
  summarise(across(.cols = c(height, mass), .fns = mean, na.rm = TRUE))

# Podemos aplicar facilmente uma função a todas
# as colunas de uma base
starwars %>%
  mutate(across(.fns = na_if, y = "unknown"))

# Antes usávamos o sufixo "_all"
starwars %>%
  mutate_all(.funs = ~na_if(.x, "unknown"))

# where -------------------------------------------------------------------

# Também podemos aplicar uma função a todas as colunas de um tipo
starwars %>%
  summarise(across(where(is.character), n_distinct))

# Antes fazíamos
starwars %>%
  summarise_if(is.character, n_distinct)

# Agora, podemos fazer sumarizações complexas
starwars %>%
  group_by(sex) %>%
  summarise(
    across(where(is.numeric), mean, na.rm = TRUE),
    across(where(is.character), n_distinct),
    n_obs = n(),
  ) %>%
  View()

# Isso não era possível com as funções antigas!

# Podemos usar também com o select (neste caso, não precisamos do across)
starwars %>%
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








