## code to prepare `ames` dataset goes here

# remotes::install_github("curso-r/basesCursoR")

data(ames, package = "basesCursoR")

readr::write_rds(ames, "data/ames.rds", compress = "gz")
