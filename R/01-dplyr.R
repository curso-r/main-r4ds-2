
# 1. dplyr ++ -------------------------------------------------------------

library(dplyr)

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

# O case_when() é uma generalização do ifelse()

x <- sample(-10:10, 10)

case_when(
  x < 0 ~ "negativo",
  x == 0 ~ "zero",
  x > 0 ~ "positivo"
)

# Com ifelse(), precisaríamos usar a
# função duas vezes

ifelse(x < 0, "negativo", ifelse(x == 0, "zero", "positivo"))

# O teste complementar pode ser substituído
# por um TRUE

case_when(
  x < 0 ~ "negativo",
  x == 0 ~ "zero",
  TRUE ~ "positivo"
)

# Muito útil dentro do mutate()

mtcars %>%
  mutate(
    mpg_cat = case_when(
      mpg < 15 ~ "economico",
      mpg < 22 ~ "regular",
      mpg >= 22 ~ "bebe bem"
    )
  )

# Substituindo último teste por TRUE
mtcars %>%
  mutate(
    mpg_cat = case_when(
      mpg < 15 ~ "economico",
      mpg < 22 ~ "regular",
      TRUE ~ "bebe bem"
    )
  )

# first, last -------------------------------------------------------------

# Retornam o primeiro e último valor de um vetor

x <- c(1, 12, 30, 41, 15)

first(x)
last(x)

# São funções úteis quando temos algum tipo de ordem
tab <- tibble::tibble(
  tempo = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
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

# Transforma um valor especificado em `NA`

tab <- tibble::tibble(
  var = c(1, 10, 2, -99, 10, -99)
)

tab %>%
  mutate(var = na_if(var, -99))

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
    var_lag = lag(var),
    var_lead = lead(var)
  )

# Útil para testes
tab %>%
  dplyr::mutate(
    teste = var > lead(var)
  )

# pull --------------------------------------------------------------------

# Devolve uma coluna da base (como vetor)
mtcars %>%
  pull(mpg)


# sample_n, sample_frac ---------------------------------------------------

# Pega amostra de tamanho 10 da base mtcars
sample_n(mtcars, 10)

# Pega 50% da base mtcars
sample_frac(mtcars, 0.5)

# 2. dplyr 1.0 ------------------------------------------------------------

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
ames %>%
  group_by(geral_qualidade) %>%
  summarise(across(
    .cols = c(lote_area, venda_valor),
    .fns = mean, na.rm = TRUE
  ))

# Podemos aplicar facilmente uma função a todas
# as colunas de uma base
ames %>% summarise(across(.fns = n_distinct))

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

# Também podemos combinar as ações do
# `summarise_if()` e `summarise_at()`
# em um único `across()`.

ames %>%
  summarise(
    across(
      where(is.numeric) & contains("area"),
      mean,
      na.rm = TRUE
    )
  )

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


# select, rename ----------------------------------------------------------

# Para selecionar colunas, não usamos across!
ames %>%
  select(where(is.numeric))

ames %>%
  select(contains("qualidade") & where(is.character))

# Para renomer várias colunas, utilizamos
# a função rename_with()

ames %>%
  rename_with(toupper, contains("venda"))

# relocate ----------------------------------------------------------------

# Por padrão, traz uma coluna
# para o começo da base

ames %>%
  relocate(venda_valor)


# Os argumentos .before e .after
# podem ser usados para fazer
# mudanças mais complexas

ames %>%
  relocate(venda_ano, .after = construcao_ano)

ames %>%
  relocate(venda_ano, .before = construcao_ano)

# rowwise -----------------------------------------------------------------

tab_notas <- tibble(
  student_id = 1:5,
  prova1 = sample(0:10, 5),
  prova2 = sample(0:10, 5),
  prova3 = sample(0:10, 5),
  prova4 = sample(0:10, 5)
)

# E se quisermos uma coluna com
# a nota média de cada aluno?

# Isso não vai funcionar

tab_notas %>% mutate(media = mean(c(prova1, prova2, prova3, prova4)))

# Podemos usar o group_by()

tab_notas %>%
  group_by(student_id) %>%
  mutate(media = mean(c(prova1, prova2, prova3, prova4)))

# E também a função c_across()

tab_notas %>%
  group_by(student_id) %>%
  mutate(media = mean(c_across(starts_with("prova"))))

# Podemos trocar o group_by() por rowwise()

tab_notas %>%
  rowwise(student_id) %>%
  mutate(media = mean(c_across(starts_with("prova"))))

# Não é preciso passar uma coluna id

tab_notas %>%
  rowwise() %>%
  mutate(media = mean(c_across(starts_with("prova"))))


# A função rowwise() vai ser útil quando estivermos
# trabalhando com list-columns!

# 3. tidyr ----------------------------------------------------------------

library(tidyr)

imdb <- readr::read_rds("data/imdb.rds")

View(imdb)

# unite -------------------------------------------------------------------

# Como pegar todos os filmes que um determinado
# ator participa, independentemente da
# importância do papel?

# Vamos usar a função unite()

imdb %>%
  unite(col = "atores", starts_with("ator"), sep = "|") %>%
  View()

# Pegando todos os filmes que o Will Smith aparece
imdb %>%
  unite(col = "atores", starts_with("ator"), sep = "|") %>%
  filter(stringr::str_detect(atores, "Will Smith")) %>%
  View()

# separate ----------------------------------------------------------------

# A função separate() é a operação contrária da função unite()

imdb %>%
  unite(col = "atores", starts_with("ator"), sep = "|") %>%
  separate(
    col = "atores",
    into = c("ator_1", "ator_2", "ator_3"),
    sep = "\\|"
  )

# Essa função também pode ser utilizada
# quando o número de categorias em cada
# linha não é igual.

imdb %>%
  separate(
    col = "generos",
    into = c("genero_1", "genero_2", "genero_3"),
    sep = "\\|"
  ) %>%
  View()

# pivotagem ---------------------------------------------------------------

# pivot_longer

imdb %>%
  pivot_longer(
    cols = starts_with("ator"),
    names_to = "posicao",
    values_to = "ator"
  ) %>%
  View()

# Podemos usar essa nova estrutura para
# filtrar os filmes em que um
# determinado ator participa

imdb %>%
  pivot_longer(
    cols = starts_with("ator"),
    names_to = "posicao",
    values_to = "ator"
  ) %>%
  filter(ator == "Will Smith") %>%
  View()

# pivot_wider

tab_romance_terror <- imdb %>%
  filter(ano >= 2010) %>%
  mutate(
    genero = case_when(
      stringr::str_detect(generos, "Romance") ~ "Romance",
      stringr::str_detect(generos, "Horror") ~ "Horror",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(genero)) %>%
  group_by(ano, genero) %>%
  summarise(
    receita_media = mean(receita, na.rm = TRUE)
  )

tab_romance_terror %>%
  pivot_wider(
    names_from = "ano",
    values_from = "receita_media"
  ) %>%
  View()

# É a operação contrária do pivot_longer

imdb %>%
  pivot_longer(
    cols = starts_with("ator"),
    names_to = "posicao",
    values_to = "ator"
  ) %>%
  pivot_wider(
    names_from = "posicao",
    values_from = "ator"
  )

# Essas funções são muito utilizadas na
# hora de fazer alguns gráficos

library(ggplot2)

mtcars %>%
  pivot_longer(
    cols = -mpg,
    names_to = "variavel",
    values_to = "valor"
  ) %>%
  ggplot(aes(x = valor, y = mpg)) +
  geom_point() +
  facet_wrap(~variavel, scales = "free")


# Listas ------------------------------------------------------------------

# Listas são uma estrutura importante dentro
# do R pois todos os data.frames (tibbles)
# são listas.

class(mtcars)
is.list(mtcars)

class(starwars)
is.list(starwars)

# Criamos uma lista usando a função list()

list(1, "a", TRUE)

# Veja que, diferente de um vetor, podemos
# guardar objetos de classes diferentes.

# Podemos dar nomes à cada posição

list(cliente = "Ana Silva", idade = 25, estado_civil = NA)

# Cada posição pode ser um vetor, de mesmo
# tamanho ou não

list(
  cliente = c("Ana Silva", "Bruno Santos"),
  idade = c(25, 30),
  estado_civil = c(NA, "Solteira(o)")
)

# Um data frame nada mais é do que uma lista nomeda
# com vetores de mesmo tamanho

list(
  cliente = c("Ana Silva", "Bruno Santos"),
  idade = c(25, 30),
  estado_civil = c(NA, "Solteira(o)")
) %>%
  as.data.frame()

# A única diferença (relevante) entre eles
# é que um data frame possui o atributo "dimensão"

lista <- list(
  cliente = c("Ana Silva", "Bruno Santos"),
  idade = c(25, 30),
  estado_civil = c(NA, "Solteira(o)")
)

dim(lista)

dim(as.data.frame(lista))

# nest/unnest -------------------------------------------------------------

# Utilizamos as funções nest() e unnest()
# para criarmos/desfazermos list-columns

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

# Veremos mais sobre list-columns na aula de purrr!

