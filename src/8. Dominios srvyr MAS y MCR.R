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

#============================
#---- Diseño M.A.S(N, n): 
#--- FACTOR EXPANSION d_k = N/n y corrección por población finita

#-------Estimación

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

#---------- Maquillaje

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
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = px(16),
    heading.align = "left",
    row.striping.include_table_body = TRUE,
    table.border.top.style = "solid",
    table.border.bottom.style = "solid"
  )

tabla_salida

#===========================================
#-------------------- MCR
#----------------------------

hogares$fex_mcr <- 1 / (nrow(hogares) * (1/35))   # 1/(m p_k)

#---- Estimación
dis_mcr <- svydesign(ids = ~1, weights = ~fex_mcr, data = hogares)

(formula_rubros <- reformulate(rubros))

(resultados <- svyby(formula_rubros, ~Ingresos, design = dis_mcr, FUN = svytotal, vartype="cv"))
(estimacion_general <- svytotal(formula_rubros, design = dis_mcr)) #No tiene un argumento vartype

#---------- Maquillaje

# 1. Resultados por dominio
dominios_mcr <- resultados |> as.data.frame() |>
  pivot_longer(cols = -Ingresos, names_to = "nombre", values_to = "value") |>
  mutate(stat = if_else(str_starts(nombre, "cv\\."), "cve", "total"),
         Gasto = str_remove(nombre, "^cv\\.")) |>
  select(Ingresos, Gasto, stat, value)

# 2. Resultados generales: total y CV en porcentaje
coef_general <- coef(estimacion_general)
cv_general <- cv(estimacion_general)

general <- tibble(Gasto = names(coef_general),
                  total = as.numeric(coef_general),
                  cve = 100 * as.numeric(cv_general)) |>
           pivot_longer(cols = c(total, cve), names_to = "stat", values_to = "value") |>
           mutate(Ingresos = "Total")

# 3. Unir resultados
cuadro_mcr <- bind_rows(dominios_mcr, general) |>
  pivot_wider(names_from = c(Ingresos, stat), values_from = value, names_sep = "__") |>
  mutate(Gasto = recode(Gasto,
                              Alimentacion = "Alimentación",
                              Educacion = "Educación",
                              Total_gasto = "Total"),
    # Los CV de los dominios vienen como proporciones
        across(ends_with("__cve") & !starts_with("Total__"), ~ 100 * .x),
        Gasto = factor(Gasto, levels = c("Vivienda", "Alimentación", "Educación", "Transporte", "Esparcimiento", "Otros", "Total"))) |>
  arrange(Gasto) |>
  mutate(Gasto = as.character(Gasto))

# 4. Orden de las columnas
orden_columnas <- c("Gasto", unlist(lapply(dominios, \(d) c(paste0(d, "__total"),  paste0(d, "__cve")))), "Total__total", "Total__cve")

cuadro_mcr <- cuadro_mcr |>
              select(all_of(orden_columnas))

# 5. Tabla con gt
tabla_mcr <- cuadro_mcr |>
  gt(rowname_col = "Gasto") |>
  tab_header(title = md("**Cuadro 2. Gasto de los hogares según ingresos familiares**"),
             subtitle = "Estimación de totales y coeficientes de variación bajo MCR")

for (d in dominios) {
  tabla_mcr <- tabla_mcr |>
    tab_spanner(label = d, 
                columns = all_of(c(paste0(d, "__total"), paste0(d, "__cve"))),
                id = paste0("mcr_", make.names(d)))
}

tabla_mcr <- tabla_mcr |>
  tab_spanner(label = "Total general",
              columns = all_of(c("Total__total", "Total__cve")),
              id = "mcr_total") |>
  cols_label(.list = setNames(as.list(ifelse(str_ends(names(cuadro_mcr)[-1], "__cve"),
                              "cve (%)", "Total estimado")),
             names(cuadro_mcr)[-1])) |>
  fmt_number(columns = ends_with("__total"),
             decimals = 0,
             use_seps = TRUE) |>
  fmt_number(columns = ends_with("__cve"),
             decimals = 1) |>
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(rows = Gasto == "Total")) |>
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = px(16),
    heading.align = "left",
    row.striping.include_table_body = TRUE,
    table.border.top.style = "solid",
    table.border.bottom.style = "solid"
  )

tabla_mcr

###--- Coincide con Excel en clase