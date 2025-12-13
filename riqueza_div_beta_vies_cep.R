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
  geom_sf(data = registros_sf, size = 0.5)

## Unidades de conservação ----

### Importando ----

uc <- geobr::read_conservation_units() |>
  sf::st_make_valid()

### Visualizando ----

uc

ggplot() +
  geom_sf(data = uc)

### Recortando para o CEP ----

uc_code <- uc |>
  sf::st_intersection(cep |> sf::st_union() |> sf::st_as_sf()) |>
  dplyr::pull(code_conservation_unit)

uc_code

uc_cep <- uc |>
  dplyr::filter(code_conservation_unit %in% uc_code)

ggplot() +
  geom_sf(data = cep) +
  geom_sf(data = uc_cep, color = "red", fill = NA) +
  geom_sf(data = registros_sf, size = 0.5)

### Vetorizando ----

uc_cep_vec <- uc_cep |>
  terra::vect()

uc_cep_vec

# Riqueza ----

## Valores de riqueza por gride ----

riqueza_fid <- registros_sf |>
  sf::st_join(cep) |>
  sf::st_drop_geometry() |>
  dplyr::summarise(Riqueza = dplyr::n_distinct(species),
                   .by = FID)

riqueza_fid

cep_trat <- cep |>
  dplyr::left_join(riqueza_fid) |>
  dplyr::mutate(Riqueza = dplyr::case_when(Riqueza |> is.na() ~ 0,
                                           .default = Riqueza))

cep_trat

ggplot() +
  geom_sf(data = cep_trat, aes(fill = Riqueza, color = Riqueza)) +
  scale_fill_viridis_c(option = "turbo") +
  scale_color_viridis_c(option = "turbo")

## Rasterizando ----

resolucao <- cep[1, ] |> terra::vect() |> terra::ext()

resolucao

cep_vetor <- cep |>
  terra::vect() |>
  terra::ext() |>
  terra::rast(res = c((resolucao$xmax - resolucao$xmin),
                      (resolucao$ymax - resolucao$ymin)))

cep_vetor

cep_riqueza_rast <- cep_trat |>
  terra::vect() |>
  terra::rasterize(cep_vetor, field = "Riqueza")

cep_riqueza_rast

ggplot() +
  tidyterra::geom_spatraster(data = cep_riqueza_rast) +
  scale_fill_viridis_c(option = "turbo",
                       na.value = "transparent")

# Diversidade beta ----

## Criando uma matriz de dissimilaridade ----

# Vi[es de amostragem ----
