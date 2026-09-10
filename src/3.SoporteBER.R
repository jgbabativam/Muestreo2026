
library(TeachingSampling)

N <- 9
n <- 3

df <- data.frame(combn(1:N, n) |> t())

TeachingSampling::Support(N, n)


datos <- tribble(
  ~"id", ~"valor",
      1, 0,
      2, 300,
      3, 0,
      4, 435
)

df <- data.frame(TeachingSampling::SupportRS(4)) |> 
      left_join(datos |> rename(yk=valor),
                by = c("X1" = "id")) |>
      left_join(datos |> rename(yl=valor),
                by = c("X2" = "id"))  |>
      left_join(datos |> rename(ym=valor),
            by = c("X3" = "id")) |>
      left_join(datos |> rename(yo=valor),
            by = c("X4" = "id")) |> 
      mutate(across(everything(), ~ifelse(is.na(.), 0, .)))
