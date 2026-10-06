rm(list = ls())
library(pacman)
p_load(tidyverse, readxl)

dir("data/")
datos <- read_excel("data/EjerDominios.xlsx")

glimpse(datos)
variables <- paste0("P", 2:7)


datos$P8 <- rowSums(datos[, variables], na.rm = TRUE)
variables <- paste0("P", 2:8)

dominios <- datos |> 
  mutate(d1 = ifelse(P1 == 1,1,0),
         d2 = ifelse(P1 == 2,1,0),
         d3 = ifelse(P1 == 3, 1,0),
         fex = 35/10)

df <- dominios |> 
  mutate(across(variables, ~.x*d1, .names = "{.col}_d1v"),
         across(variables, ~.x*d2, .names = "{.col}_d2v"),
         across(variables, ~.x*d3, .names = "{.col}_d3v"),
         across(variables, ~.x*d1*fex, .names = "{.col}_d1fex"),
         across(variables, ~.x*d2*fex, .names = "{.col}_d2fex"),
         across(variables, ~.x*d3*fex, .names = "{.col}_d3fex"))

totales <- df |> 
  summarise(across(ends_with("fex"),~sum(.x))) |> 
  select(-fex) |> 
  pivot_longer(cols = everything(),names_to = "variable", values_to = "totales")

varianzas <- df |> 
  summarise(across(ends_with("v"),~sqrt(35^2/10 * (1-10/35)*var(.x)))) |> 
  pivot_longer(cols = everything(),names_to = "variable", values_to = "EE")
            
resultado <- bind_cols(totales, varianzas |> select(-variable)) |> 
  mutate(cve = EE/totales) |> 
  relocate(variable, totales, cve)
