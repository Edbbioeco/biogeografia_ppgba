# Pacotes ----

library(readxl)

library(tidyverse)

library(sf)

library(geobr)

library(terra)

library(tidyterra)

library(betapart)

library(sampbias)

# Dados ----

## Registros ----

### Importando ----

registros <- readxl::read_xlsx("registros.xlsx")

### Visualizando ----

registros

registros |> dplyr::glimpse()

### Convertendo em shapefile ----

registros_sf <- registros |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674)

registros_sf

ggplot() +
  geom_sf(data = registros_sf)

## CEP ----

### Importando ----

cep <- sf::st_read("grade_cep.shp")

### Visualizando -----

cep

ggplot() +
  geom_sf(data = cep) +
  geom_sf(data = registros_sf)

## Unidades de conservação ----

### Importando ----

### Visualizando ----

# Riqueza ----

# Diversidade beta ----

# Vi[es de amostragem ----
