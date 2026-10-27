library(pacman)
p_load(tidyverse, readxl, TeachingSampling, janitor)

marco <- read_excel("data/MarcoEmpresas2026.xlsx") |> arrange(id)

glimpse(marco)
get_dupes(marco, id)


marco |> 
  ggplot(aes(x = ingresos_2025, y = estrato)) +
  geom_boxplot() +
  theme_bw()

tam <- marco |>
      count(estrato, name = "Nh") |>
      mutate(nh = c(20, 60, 80, 80, 100))          # en el orden de los estratos

set.seed(2026)
sel <- S.STSI(marco$estrato, tam$Nh, tam$nh)   # M.A.S(N_h, n_h) en cada estrato

muestra <- marco[sel, ] |>
  left_join(tam, by = "estrato") |>
  mutate(pik = nh / Nh, dk = Nh / nh)           # pi_k y factor de expansion


tapply(muestra$dk, muestra$estrato, sum)

###############################

dir("data/")
library(stratification)

df <- read_excel("data/PoblacionMunicipios2026.xlsx")

strata.LH(df$poblacion, CV = 0.03, algo = "Kozak", Ls = 5)



 ######

p_load(survey, srvyr)
df <- read_excel("data/EjercicioESTMAS.xlsx")

glimpse(df)

table(df$estrato)

base <- df |> 
  mutate(Nh = case_when(
               estrato == "1 empleado" ~ 1200,
               estrato == "2 a 3 empleados" ~ 800,
               estrato == "4 a 5 empleados" ~ 400,
               TRUE ~ 200
  )) |> 
  group_by(estrato) |> 
  mutate(dk = Nh/n())

sum(base$dk)
tapply(base$dk, base$estrato, sum)

dis <- base |> 
       as_survey_design(ids = 1,
                              strata = estrato,
                              weights = dk)

dis |> 
  group_by(estrato, registro) |> 
  summarise(total = survey_total(ventas, vartype = "cv"))

dis |> 
  group_by(registro) |> 
  summarise(total = survey_total(ventas, vartype = "cv"))

dis |> 
  group_by(estrato) |> 
  summarise(total = survey_total(ventas, vartype = "cv"))

dis |> 
  ungroup() |> 
  summarise(total = survey_total(ventas, vartype = "cv"))






