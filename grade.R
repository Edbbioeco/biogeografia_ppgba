# Pacotes ----

library(geobr)

library(tidyverse)

library(sf)

# Dados ----

## Estados ----

### Importando ----

estados <- geobr::read_state(year = 2019) %>%
  dplyr::filter(abbrev_state %in% c("AL", "PE", "PB", "RN")) %>%
  sf::st_union()

### Visualizando ----

estados

estados %>%
  ggplot() +
  geom_sf(color = "black", fill = "gold")

## Mata Atlântica ----

### Importando ----

ma <- geobr::read_biomes() %>%
  dplyr::filter(name_biome == "Mata Atlântica")

## Visualizando ----

ma

ma %>%
  ggplot() +
  geom_sf(color = "black", fill = "green4")

## Centro de Endemismo Pernambuco ----

### Checando os crs ----

sf::st_crs(ma) == sf::st_crs(estados)

### Recortando ----

cep <- ma %>% sf::st_intersection(estados)

### Visualizando ----

cep

cep %>%
  ggplot() +
  geom_sf(color = "black", fill = "yellowgreen")

# Grade ----

## Criando a grade ----

grade <- sf::st_make_grid(cep %>%
                   st_transform(crs = 5880),
                 cellsize = 10000) %>%
  sf::st_make_valid()

## Visualizando ----

grade

grade %>%
  ggplot() +
  geom_sf(color = "black", fill = "green4") +
  geom_sf(data = cep, color = "red", fill = "transparent")

## Recorte ----

### Alterando o crs ---

grade <- grade %>%
  sf::st_transform(crs = sf::st_crs(cep))

### Recortando ----

grade_cep <- grade %>%
  sf::st_intersection(cep)

### Visualizando ----

grade_cep

grade_cep %>%
  ggplot() +
  geom_sf(color = "black", fill = "green4")

## Exportando ----

grade_cep %>%
  sf::st_write("grade_cep.shp")
