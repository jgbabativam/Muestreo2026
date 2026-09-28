
rm(list = ls())

library(pacman)
p_load(tidyverse, janitor, haven)

agpop <- haven::read_sav("data/agpop.sav") |> clean_names()

glimpse(agpop)



res <- agpop |>
  summarise(
    across(
      where(is.numeric), 
      list(media = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE))
    )
  ) |> 
  select(-region_media, -region_sd) |> 
  pivot_longer(cols = everything(),
               names_to = "estadistica", values_to = "valor") |> 
  separate(estadistica, c("variable", "estadistica"), sep = "_") |> 
  pivot_wider(names_from = estadistica, values_from = valor) |> 
  mutate(CV = sd/media)
  

(CV <- max(res$CV))

error <- 0.1  

n0 <- 1.96^2*CV^2/error^2
N <- nrow(agpop)

(n <- ceiling(n0/(1+n0/N)))  


library(samplesize4surveys)

?samplesize4surveys::ss4dpH




