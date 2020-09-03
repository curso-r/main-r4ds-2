
library(dplyr)

# curly-curly -------------------------------------------------------------

# Uma função simples

f <- function(x) {
  x + 1
}

f(x = 10)

a <- 20

f(x = a)
f(x = b)

f(a)
f(b)

f(x = "texto")

# No dplyr

mtcars %>%
  select(mpg)

mpg

mtcars %>%
  filter(mpg > 20)

# A função select() é construída usando
# data masking (tidy eval ou Non-standard evaluation ou NSE)

meu_select <- function(nome_coluna) {
  mtcars %>%
    select(nome_coluna)
}

meu_select(nome_coluna = mpg)
meu_select(mpg)

meu_select(nome_coluna = "mpg")

mtcars %>%
  select("mpg")

mpg <- "cyl"
meu_select(nome_coluna = mpg)

mtcars %>%
  filter(mpg > 20)

mtcars %>%
  filter("mpg" > 20)

meu_filter <- function(nome_coluna, valor) {
  mtcars %>%
    filter(nome_coluna > valor)
}

meu_filter(mpg, 20)
meu_filter("mpg", 20)

# Solução: {{ }} (curly-curly)

meu_select <- function(nome_coluna) {
  mtcars %>%
    select( {{nome_coluna}} )
}

meu_select("mpg")
meu_select("cyl")

meu_filter <- function(nome_coluna, valor) {
  mtcars %>%
    filter({{nome_coluna}} > valor)
}

meu_filter(mpg, 20)
meu_filter(cyl, 6)

library(ggplot2)

mtcars %>%
  ggplot() +
  geom_histogram(aes(x = mpg))

meu_histograma <- function(nome_coluna) {
  mtcars %>%
    ggplot() +
    geom_histogram(aes(x = {{nome_coluna}}))
}

meu_histograma(mpg)
meu_histograma(wt)

# Environments ------------------------------------------------------------

a <- 10

vetor <- c(1, 2, 3)

mtcars

library(dplyr)

mtcars %>%
  filter(mpg > 10)

stats::filter()

filter <- function(x) {
  x + 1
}

filter(1)

filter(mtcars, mpg > 10)
rm(filter)


f <- function(x) {
  x + 1
}

f(x = a)

g <- function(y) {
  b <- 10

  y + b
}

c <- g(y = a)

# Non-standard evaluation -------------------------------------------------

f <- function(nome_coluna) {
  nome_coluna
}

f(1)
f("texto")
f(mpg)



substitute(1:10)
substitute(x)
substitute(x + y ^ 2)

codigo <- substitute(1:10)
eval(codigo)

deparse(substitute(x + y ^2))
deparse(substitute(x))

meu_select <- function(nome_coluna) {
  codigo <- substitute(nome_coluna)
  nome_coluna_string <- deparse(codigo)

  mtcars[, nome_coluna_string, drop = FALSE]
}

meu_select(wt)

a <- 1
# Referências -------------------------------------------------------------

# Advanced R:  https://adv-r.hadley.nz/
   # Nomes e valores: https://adv-r.hadley.nz/names-values.html
   # Environments: https://adv-r.hadley.nz/environments.html
   # NSE: https://adv-r.hadley.nz/metaprogramming.html

# Programando com dplyr (curly-curly):
# https://dplyr.tidyverse.org/articles/programming.html

# Pacote rlang
# https://www.tidyverse.org/blog/2019/06/rlang-0-4-0/

