# Pacotes ----

library(readxl)

library(tidyverse)

library(sf)

library(writexl)

# Dados ----

## Ocorrências ----

### Importando ----

oc_specieslink <- readxl::read_xlsx("C:/Users/LENOVO/OneDrive/Documentos/anuros_caatinga/dados_specieslink.xlsx")

### Visualizanddo ----

oc_specieslink

oc_specieslink |> dplyr::glimpse()

### Tratando ----

oc_specieslink_trat <- oc_specieslink |>
  dplyr::select(scientificname, longitude:latitude) |>
  dplyr::mutate(longitude = longitude |> as.numeric(),
                latitude = latitude |> as.numeric()) |>
  dplyr::filter(!scientificname |> stringr::str_detect(" sp| cf| af")) |>
  tidyr::drop_na() |>
  dplyr::distinct(scientificname, longitude, latitude, .keep_all = TRUE)

oc_specieslink_trat |> as.data.frame()

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

oc_specieslink_shp <- oc_specieslink_trat |>
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = 4674)

oc_specieslink_shp

ggplot() +
  geom_sf(data = grade_cep, color = "black", fill = "green4") +
  geom_sf(data = oc_specieslink_shp)

## Espécies por grade ----

### Intersecção ----

oc_specieslink_inter <- grade_cep |> sf::st_join(oc_specieslink_shp,
                                                 join = st_intersects) |>
  dplyr::filter(!is.na(scientificname) & scientificname |>
                  stringr::word(2) != "NA") |>
  tibble::as_tibble() |>
  dplyr::select(FID, scientificname) |>
  dplyr::mutate(presence = 1,
                Source = "SpeciesLink") |>
  dplyr::rename("species" = scientificname) |>
  dplyr::distinct(FID, species, .keep_all = TRUE) |>
  dplyr::bind_cols(grade_cep |>
                     sf::st_join(oc_specieslink_shp, join = st_intersects) |>
                     dplyr::filter(!is.na(scientificname) & scientificname |>
                                     stringr::word(2) != "NA") |>
                     dplyr::select(FID, scientificname) |>
                     dplyr::mutate(presence = 1) |>
                     dplyr::rename("species" = scientificname) |>
                     dplyr::distinct(FID, species, .keep_all = TRUE) |>
                     sf::st_centroid() |>
                     sf::st_coordinates() |>
                     tibble::as_tibble() |>
                     dplyr::rename("Longitude" = X,
                                   "Latitude" = Y))

oc_specieslink_inter

### Matriz ----

oc_specieslink_inter |>
  tidyr::pivot_wider(names_from = species,
                     values_from = presence,
                     values_fn = function(x) 1,
                     values_fill = 0)

### Exportando ----

oc_specieslink_inter |>
  writexl::write_xlsx("registros_specieslink.xlsx")
