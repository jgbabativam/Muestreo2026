rm(list = ls())
library(pacman)

p_load(tidyverse, survey, srvyr, janitor)

N <- 265

datos <- read_excel("data/Estudiantes.xlsx") |> clean_names() |>  
         mutate(sexo=factor(sexo, levels = 1:2,
                            labels = c("Hombre", "Mujer")),
                fpc = N)

n <- nrow(datos)


dis <- datos |> 
  as_survey(ids = 1,
            fpc = fpc)


glimpse(datos)

dis |> 
  group_by(sexo) |> 
  summarise(matricula = survey_total(matricula, vartype = "cv"),
            sistematizacion = survey_total(sistematizacion, vartype = "cv")) |> 
bind_rows(
  dis |> 
    summarise(matricula = survey_total(matricula, vartype = "cv"),
              sistematizacion = survey_total(sistematizacion, vartype = "cv")) |> 
    mutate(sexo = "Total")
)



