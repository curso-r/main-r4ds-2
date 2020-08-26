## code to prepare `covid` dataset goes here

covid <- basesCursoR::covid

readr::write_rds(covid, "data/covid.rds")
