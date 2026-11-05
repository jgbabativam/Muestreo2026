rm(list = ls())

library(pacman)
p_load(tidyverse, janitor, survey, srvyr, readxl)

datos <- read_excel("data/EncuestaCaficultores.xlsx")
         
datos |> 
  count(estrato)

glimpse(datos)


encuesta <- datos |> 
            group_by(estrato) |> 
            mutate(dk = Nh/n()) |> 
            ungroup()

glimpse(encuesta)

?as_survey_design

enc_dis <- encuesta |> 
           as_survey_design(ids = 1,
                            strata = estrato,
                            fpc = Nh,
                            weights = dk,
                            nest = TRUE
                            )


cuadro1 <- enc_dis |> 
           group_by(variedad) |>
           summarise(tfincas = survey_total(vartype = "cv"),
                     tproduccion = survey_total(produccion, vartype = "cv"),
                     tarea = survey_total(area, vartype = "cv"))
           

cuadro1t <- enc_dis |> 
  summarise(tfincas = survey_total(vartype = "cv"),
            tproduccion = survey_total(produccion, vartype = "cv"),
            tarea = survey_total(area, vartype = "cv")) |> 
  mutate(variedad = "Total")


cuadro1 <- bind_rows(cuadro1, cuadro1t)



  




  
  
  

