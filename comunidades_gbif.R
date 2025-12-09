# Pacotes ----

library(readxl)

library(tidyverse)

library(parzer)

library(sf)

library(writexl)

# Dados ----

## Ocorrências ----

### Importando ----

oc_gbif <- readxl::read_xlsx("ocorrencias_gbif.xlsx")

### Visualizanddo ----

oc_gbif |> dplyr::glimpse()

oc_gbif

### Tratando ----

oc_gbif_trat <- oc_gbif |>
  dplyr::select(species, stateProvince, decimalLatitude:decimalLongitude) |>
  dplyr::rename("Latitude" = decimalLatitude,
                "Longitude" = decimalLongitude) |>
  dplyr::mutate(Longitude = Longitude |>
                  stringr::str_replace("^(-?\\d{2})(\\d+)$", "\\1.\\2") |>
                  as.numeric(),
                Latitude = case_when(stringr::str_detect(as.character(Latitude), "^(-?[1-2])") ~ str_replace(as.character(Latitude), "^(-?\\d{2})(\\d+)$", "\\1.\\2"),
                                     stringr::str_detect(as.character(Latitude), "^(-?[3-9])") ~ stringr::str_replace(as.character(Latitude), "^(-?\\d{1})(\\d+)$", "\\1.\\2"),
                                     TRUE ~ as.character(Latitude)) |>
                  as.numeric()) |>
  dplyr::filter(!is.na(species) &
                  !is.na(Latitude) &
                  !is.na(Longitude) &
                  !species |> stringr::str_detect(" sp| cf| af") &
                  species |> stringr::word(2) != "NA") |>
  dplyr::distinct(species, Longitude, Latitude, .keep_all = TRUE)

oc_gbif_trat

oc_gbif_trat |> dplyr::glimpse()

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

oc_gbif_shp <- oc_gbif_trat |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674)

oc_gbif_shp

ggplot() +
  geom_sf(data = grade_cep) +
  geom_sf(data = oc_gbif_shp)

## Espécies por grade ----

### Intersecção ----

oc_gbif_inter <- grade_cep |> sf::st_join(oc_gbif_shp, join = st_intersects) |>
  dplyr::filter(!is.na(species)) |>
  tibble::as_tibble() |>
  dplyr::select(FID, species) |>
  dplyr::mutate(presence = 1,
                Source = "GBIF") |>
  dplyr::bind_cols(grade_cep |> sf::st_join(oc_gbif_shp, join = st_intersects) |>
                     dplyr::filter(!is.na(species)) |>
                     sf::st_centroid() |>
                     sf::st_coordinates() |>
                     tibble::as_tibble() |>
                     dplyr::select(1:2) |>
                     dplyr::rename("Longitude" = X,
                                   "Latitude" = Y))

oc_gbif_inter

### Matriz ----

oc_gbif_inter |>
  tidyr::pivot_wider(names_from = species,
                     values_from = presence,
                     values_fn = function(x) 1,
                     values_fill = 0)

### Exportando ----

oc_gbif_inter |>
  writexl::write_xlsx("registros_gbif.xlsx")

