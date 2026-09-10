
#### Muestreo Aleatorio Simple

rm(list = ls())

library(pacman)
p_load(tidyverse, janitor, TeachingSampling, writexl)

divipola <- read.csv2("data/DIVIPOLA.csv") |> clean_names()

glimpse(divipola)

(N <- nrow(divipola))
(n <- 25)
set.seed(123456)

divipola$zeta <- runif(N)

muestra <- divipola |> 
            arrange(zeta) |> 
            slice(1:n)


writexl::write_xlsx(muestra, "data/Muestra CN 25mun.xlsx")


##### Fan- Muller

set.seed(123456)
indices <- TeachingSampling::S.SI(N, n)
muestra2 <- divipola[indices,]

set.seed(123456)
indices <- sample(N, n, replace = FALSE)
muestra3 <- divipola[indices,]


