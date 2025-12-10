# Pacotes ----

library(tidyverse)

library(sf)

library(phyloregion)

# Dados ----

## Importando ----

linhas <- read_lines("tsab_a_2039796_sm0766.xyd")

## Visualizando ----

linhas

## Transformando em um dataframe ----

df_occ <- linhas |>
  tibble::tibble() |>
  dplyr::slice(-c(1:4)) |>
  tidyr::separate(linhas,
                  into = c("long", "lat"),
                  sep = ",") |>
  dplyr::mutate(Espécie = dplyr::case_when(lat |> is.na() ~ long,
                                           .default = NA)) |>
  tidyr::fill(Espécie) |>
  tidyr::drop_na() |>
  dplyr::mutate(long = long |> as.numeric(),
                lat = lat |> as.numeric())

df_occ

df_occ |> dplyr::glimpse()
