# Pacotes ----

library(tidyverse)

library(sf)

library(writexl)

# Dados ----

## Ocorrências ----

### Importando ----

oc_sibbr <- readr::read_csv("C:/Users/LENOVO/OneDrive/Documentos/anuros_caatinga/data_sibbr.csv")

### Visualizanddo ----

oc_sibbr

oc_sibbr |> dplyr::glimpse()

### Tratando ----

oc_sibbr_trat <- oc_sibbr |>
  dplyr::select(scientificName, decimalLatitude:decimalLongitude) |>
  dplyr::rename("Longitude" = decimalLongitude,
                "Latitude" = decimalLatitude,
                "species" = scientificName) |>
  dplyr::filter(!Longitude |> is.na() &
                  !Latitude |> is.na() &
                  !species |> is.na() &
                  !species |> stringr::str_detect(" sp| cf| af") &
                  species |> stringr::word(2) != "NA") |>
  dplyr::distinct(species, Longitude, Latitude, .keep_all = TRUE)

oc_sibbr_trat

## Grade ----

### Importando ----

grade_cep <- sf::st_read("grade_cep.shp")

### Visualizando ----

grade_cep

grade_cep |>
  ggplot() +
  geom_sf(color = "black", fill = "green4")

# Comunidades ----

## Criando um shapefile das ocorrências ----

oc_sibbr_shp <- oc_sibbr_trat |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674)

oc_sibbr_shp

ggplot() +
  geom_sf(data = grade_cep, color = "black", fill = "green4") +
  geom_sf(data = oc_sibbr_shp)

## Espécies por grade ----

### Intersecção ----

oc_sibbr_inter <- grade_cep |> sf::st_join(oc_sibbr_shp, join = st_intersects) |>
  dplyr::filter(!is.na(species) & species |> stringr::word(2) != "NA") |>
  tibble::as_tibble() |>
  dplyr::select(FID, species) |>
  dplyr::mutate(presence = 1,
                Source = "SiBBr") |>
  dplyr::bind_cols(grade_cep |> sf::st_join(oc_sibbr_shp, join = st_intersects) |>
                     dplyr::filter(!is.na(species) & species |> stringr::word(2) != "NA") |>
                     sf::st_centroid() |>
                     sf::st_coordinates() |>
                     tibble::as_tibble() |>
                     dplyr::select(1:2) |>
                     dplyr::rename("Longitude" = X,
                                   "Latitude" = Y))

oc_sibbr_inter

### Matriz ----

oc_sibbr_inter |>
  tidyr::pivot_wider(names_from = species,
                     values_from = presence,
                     values_fn = function(x) 1,
                     values_fill = 0)

### Exportando ----

oc_sibbr_inter |>
  writexl::write_xlsx("registros_sibbr.xlsx")

