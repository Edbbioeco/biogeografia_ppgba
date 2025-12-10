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

# Grade -----

## Transformando as ocorrencias em um shapefile ----

df_sf <- df_occ |>
  sf::st_as_sf(coords = c("long", "lat"),
               crs = 4674)

ggplot() +
  geom_sf(data = df_sf)

## Criando a grade ----

grade <- df_sf |>
  sf::st_make_grid(cellsize = c(6.500, 6.980),
                          offset = c(-96.000, -48.000))

ggplot() +
  geom_sf(data = grade) +
  geom_sf(data = df_sf)

## Calculando a intersecção ----

