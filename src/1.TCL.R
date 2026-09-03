rm(list = ls())

library(pacman)
p_load(readxl, tidyverse, janitor)

set.seed(123456)


df <- map_dfc(
  1:100,
  ~ tibble(!!paste0("M", .x) := runif(1000, 0, 1))
)

glimpse(df)


df |> select(M1) |> 
  ggplot(aes(x = M1)) +
  geom_histogram(bins = 10, color = "white", fill = "lightblue") +
  theme_bw()


n <- 100

df |> 
  pivot_longer(cols = everything()) |> 
  group_by(name) |> 
  summarise(suma = sum(value)) |> 
  slice(1:n) |> 
  ggplot(aes(x = suma)) +
  geom_histogram(bins = 10, color = "white", fill = "blue")+
  theme_classic()












