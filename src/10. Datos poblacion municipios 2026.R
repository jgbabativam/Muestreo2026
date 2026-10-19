rm(list = ls())
library(pacman)

p_load(tidyverse, readxl, writexl)

# Proyecciones de población municipal (DANE, actualización post COVID-19, 2020-2035)
url <- paste0("https://www.dane.gov.co/files/censo2018/proyecciones-de-poblacion/",
              "Municipal/DCD-area-proypoblacion-Mun-2020-2035-ActPostCOVID-19.xlsx")
archivo <- tempfile(fileext = ".xlsx")
download.file(url, archivo, mode = "wb")

# Las primeras 6 filas son títulos; columnas: DP, DPNOM, MPIO, DPMP, AÑO, ÁREA, Población
proy <- read_excel(archivo, skip = 6, col_types = "text")
names(proy) <- c("cod_dpto", "departamento", "cod_mpio", "municipio",
                 "anio", "area", "poblacion")

municipios <- proy |>
  filter(anio == "2026", area == "Total") |>
  mutate(poblacion = as.numeric(poblacion)) |>
  select(cod_dpto, departamento, cod_mpio, municipio, poblacion) |>
  arrange(cod_mpio)

nrow(municipios)            # municipios y áreas no municipalizadas
sum(municipios$poblacion)   # población total proyectada 2026

write_xlsx(municipios, "data/PoblacionMunicipios2026.xlsx")
