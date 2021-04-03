# remotes::install_github("cienciadedatos/dados")

casas <- dados::casas

readr::write_rds(casas, "data/casas.rds", compress = "gz")
