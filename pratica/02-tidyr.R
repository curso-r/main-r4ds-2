# 2. tidyr ----------------------------------------------------------------

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

