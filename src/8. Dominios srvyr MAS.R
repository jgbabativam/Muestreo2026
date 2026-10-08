rm(list = ls())

# Estimación de dominios bajo M.A.S: gasto de los hogares según ingresos
# Marco: una manzana con N = 35 hogares; muestra M.A.S de n = 10 hogares
# (data/EjerDominios.xlsx). Dominios: P1 = ingresos del hogar.

library(pacman)
p_load(tidyverse, readxl, survey, srvyr, gtsummary, gt)

N <- 35  ## Universo

hogares <- read_excel("data/EjerDominios.xlsx", sheet = "Hoja1") |>
  mutate(Ingresos = factor(P1, levels = 1:3,
                    labels = c("< 2 SMMLV", "2 a 4 SMMLV", "> 4 SMMLV")),
          Total_gasto = P2 + P3 + P4 + P5 + P6 + P7,
          fpc = N) |>
  rename(Vivienda = P2, Alimentacion = P3, Educacion = P4,
         Transporte = P5, Esparcimiento = P6, Otros = P7)

dominios <- levels(hogares$Ingresos)
n <- nrow(hogares)

# Diseño M.A.S(N, n): FACTOR EXPANSION d_k = N/n y corrección por población finita
dis_mas <- hogares |>
           as_survey_design(ids = 1, fpc = fpc)

rubros <- c("Vivienda", "Alimentacion", "Educacion", "Transporte",
            "Esparcimiento", "Otros", "Total_gasto")

# 1. Totales por dominio y cve (%) --------------------------------------------
# srvyr trata cada dominio como subpoblación (y_dk = 0 fuera del dominio), de
# modo que la varianza es la del pi-estimador: N^2/n (1 - n/N) S^2_{y_d s}


(por_dominio <- dis_mas |>
                group_by(Ingresos) |>
                summarise(across(all_of(rubros), ~ survey_total(.x, vartype = "cv"))))

(general <- dis_mas |>
            summarise(across(all_of(rubros), ~ survey_total(.x, vartype = "cv"))) |>
            mutate(Ingresos = "Total"))

(cuadro <- bind_rows(por_dominio |> mutate(Ingresos = as.character(Ingresos)),
                    general) |>
          pivot_longer(-Ingresos) |>
          mutate(stat = ifelse(str_ends(name, "_cv"), "cve", "total"),
                 Gasto = str_remove(name, "_cv$")) |>
          select(-name) |>
          pivot_wider(names_from = c(Ingresos, stat), values_from = value,
                      names_sep = "__") |>
          mutate(Gasto = recode(Gasto, Alimentacion = "Alimentación",
                                Educacion = "Educación", Total_gasto = "Total"),
                 across(ends_with("__cve"), ~ 100 * .x)))


dominios <- levels(hogares$Ingresos)

(tabla_salida <- cuadro |>
  gt(rowname_col = "Gasto") |>
  tab_header(
    title = "Cuadro 1. Total de gasto de las familias en la población según rango de ingresos familiares",
    subtitle = "Estimación por dominios bajo M.A.S.(35, 10)"
  ))


# Spanner

for (d in dominios) {
  tabla_salida <- tabla_salida |>
    tab_spanner(
      label = d,
      columns = starts_with(paste0(d, "__")),
      id = paste0("sp_", d)
    )
}

 
tabla_salida <- tabla_salida |>
  tab_spanner(
    label = "Total",
    columns = starts_with("Total__"),
    id = "sp_total") |>
  cols_label(
    .list = setNames(
      as.list(
        ifelse(
          str_ends(names(cuadro)[-1], "__cve"),
          "cve (%)",
          "Total"
        )
      ),
      names(cuadro)[-1]
    )) |>
  fmt_number(
    columns = ends_with("__total"),
    decimals = 0) |>
  fmt_number(
    columns = ends_with("__cve"),
    decimals = 1) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = Gasto == "Total")) |>
  tab_source_note(
    "cve: coeficiente de variación estimado del pi-estimador del total del dominio.")

tabla_salida

