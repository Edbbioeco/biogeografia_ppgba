# Pacotes ----

library(geobr)

library(tidyverse)

library(sf)

# Dados ----

## Estados ----

### Importando ----

estados <- geobr::read_state(year = 2019) |>
  dplyr::filter(abbrev_state %in% c("AL", "PE", "PB", "RN")) |>
  sf::st_union()

### Visualizando ----

estados

estados |>
  ggplot() +
  geom_sf(color = "black", fill = "gold")

## Mata Atlântica ----

### Importando ----

ma <- geobr::read_biomes() |>
  dplyr::filter(name_biome == "Mata Atlântica")

## Visualizando ----

ma

ma |>
  ggplot() +
  geom_sf(color = "black", fill = "green4")

## Centro de Endemismo Pernambuco ----

### Checando os crs ----

sf::st_crs(ma) == sf::st_crs(estados)

### Recortando ----

cep <- ma |> sf::st_intersection(estados)

### Visualizando ----

cep

cep |>
  ggplot() +
  geom_sf(color = "black", fill = "yellowgreen")

# Grade ----

## Criando a grade ----

grade <- cep |>
  sf::st_transform(crs = 32725) |>
  sf::st_make_grid(cellsize = 10000) |>
  sf::st_make_valid()

## Visualizando ----

grade

ggplot() +
  geom_sf(data = grade, color = "black", fill = "green4") +
  geom_sf(data = cep |>
            sf::st_transform(crs = 32725), color = "red", fill = "transparent")

## Recorte ----

### Recortando ----

grade_cep <- grade[grade |>
                     sf::st_intersection(cep |>
                                           sf::st_transform(crs = 32725))]

### Visualizando ----

grade_cep

grade_cep |>
  ggplot() +
  geom_sf(color = "black", fill = "green4")

## Exportando ----

grade_cep |>
  sf::st_transform(crs = 4674) |>
  sf::st_write("grade_cep.shp")

