# Pacotes ----

library(readxl)

library(tidyverse)

library(sf)

library(terra)

library(tidyterra)

# Dados ----

## Importando ----

registros <- readxl::read_xlsx("registros.xlsx")

## Visualizando ----

registros

registros |> dplyr::glimpse()

# Grade -----

## Transformando as ocorrencias em um shapefile ----

registros_sf <-registros |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674)

registros_sf

ggplot() +
  geom_sf(data = registros_sf)

## Criando a grade ----

grade <- sf::st_read("grade_cep.shp")

grade

ggplot() +
  geom_sf(data = grade) +
  geom_sf(data = registros_sf)

## Calculando a intersecção ----

grade_id <- registros_sf |>
  sf::st_join(grade)

grade_id

# Endemismo ----

## Abrangência da espécie ----

abg_sps <- grade_id |>
  sf::st_drop_geometry() |>
  dplyr::summarise(celulas = dplyr::n_distinct(FID),
                   .by = species)

abg_sps

## Endemicidade ----

valores_we <- grade_id |>
  sf::st_drop_geometry() |>
  dplyr::left_join(abg_sps,
                   by = "species") |>
  dplyr::summarise(WE = sum(1 / celulas),
                   .by = FID)

valores_we

## Mapa ----

### Unindo os valores ----

grade_id_we <- grade |>
  dplyr::left_join(valores_we,
                   by = "FID") |>
  dplyr::mutate(Area = dplyr::case_when(WE >= 10 ~ "Endemismo",
                                        WE < 10 ~ "ground",
                                        .default = NA))

grade_id_we

### Mapa ----

ggplot() +
  geom_sf(data = grade_id_we, aes(color = WE, fill = WE)) +
  scale_color_viridis_c(na.value = "transparent") +
  scale_fill_viridis_c(na.value = "transparent")

ggplot() +
  geom_sf(data = grade_id_we, aes(color = Area, fill = Area)) +
  scale_color_viridis_d(na.value = "transparent", direction = -1) +
  scale_fill_viridis_d(na.value = "transparent", direction = -1)
