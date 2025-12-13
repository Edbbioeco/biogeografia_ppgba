# Pacotes ----

library(data.table)

library(tidyverse)

library(CoordinateCleaner)

library(geobr)

library(sf)

library(writexl)

library(terra)

library(tidyterra)

library(betapart)

library(sampbias)

# Dados ----

## Registros de ocorrência ----

### Importando ----

gbif <- data.table::fread("SpiroBR.csv",
                          h = T,
                          encoding = 'UTF-8',
                          quote = "")  |>
  tibble::as_tibble()

### Visualizando ----

gbif

gbif |> dplyr::glimpse()

### Tratando ----

gbif_trat <- gbif |>
  dplyr::select(taxonRank,
                species,
                scientificName,
                decimalLongitude,
                decimalLatitude) |>
  dplyr::filter(taxonRank == "SPECIES" &
                  !is.na(decimalLongitude) &
                  !is.na(decimalLatitude)) |>
  dplyr::select(species,
                scientificName,
                decimalLongitude,
                decimalLatitude) |>
  CoordinateCleaner::clean_coordinates(tests = c("capitals",
                                                  "centroids",
                                                  "equal")) |>
  dplyr::filter(.summary == TRUE) |>
  dplyr::select(species, decimalLongitude, decimalLatitude)

gbif_trat

gbif_trat |> dplyr::glimpse()

## Shapefile do Brasil ----

### Importando ----

br <- geobr::read_country(year = 2019)

### Visualizando ----

br

ggplot() +
  geom_sf(data = br)

## Shapefile dos estados ----

### Importando ----

estados <- geobr::read_state(year = 2019)

### Visualizando ----

estados

ggplot() +
  geom_sf(data = estados)

## Shapefile da área ----

### Importando ----

study_area <- sf::st_read("SHAPE/ne_10m_land/ne_10m_land.shp")

### Visualizando ----

study_area

ggplot() +
  geom_sf(data = study_area)

## Shapefile dos pontios ----

### Convertendo em shapefile ----

gbif_sf <- gbif_trat |>
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
               crs = 4674)

### Visualizando ----

gbif_sf

ggplot() +
  geom_sf(data = study_area) +
  geom_sf(data = gbif_sf)

### Filtrando pontos apenas dentro do continente ----

gbif_sf_trat <- gbif_sf |>
  sf::st_intersection(study_area |>
                        sf::st_transform(crs = 4674))

gbif_sf_trat

ggplot() +
  geom_sf(data = study_area) +
  geom_sf(data = gbif_sf_trat)

### Extraindo as novas coordenadas ----

gbif_coords_trat <- gbif_sf_trat |>
  sf::st_drop_geometry() |>
  dplyr::bind_cols(gbif_sf_trat |>
                     sf::st_coordinates()) |>
  dplyr::rename("decimalLongitude" = X,
                "decimalLatitude" = Y)

gbif_coords_trat

## Aeroportos ----

### Importando ----

aeroporto <- sf::st_read("DRIVERS/airports.shp")

### Visualizando ----

aeroporto

ggplot() +
  geom_sf(data = aeroporto)

## Lagos ----

### Importando ----

lago <- sf::st_read("DRIVERS/lakes.shp")

### Visualizando ----

lago

ggplot() +
  geom_sf(data = lago)

## Portos ----

### Importando ----

porto <- sf::st_read("DRIVERS/ports.shp")

### Visualizando ----

porto

ggplot() +
  geom_sf(data = porto)

## Áreas urbanas ----

### Importando ----

urb <- sf::st_read("DRIVERS/urbanareas.shp")

### Visualizando ----

urb

ggplot() +
  geom_sf(data = urb)

# Raster para a área do Brasil ----

## Gerando um grid ----

br_grid <- br |>
  sf::st_make_grid(cellsize = 0.5) |>
  sf::st_as_sf() |>
  dplyr::mutate(id = dplyr::row_number())

br_grid

ids <- br_grid |>
  sf::st_intersection(br) |>
  dplyr::pull(id)

ids

br_grid <- br_grid |>
  dplyr::filter(id %in% ids)

ggplot() +
  geom_sf(data = br) +
  geom_sf(data = br_grid, fill = NA) +
  geom_sf(data = gbif_sf_trat)

## Número de registros por grade -----

riqueza <- gbif_sf_trat |>
  sf::st_join(br_grid) |>
  sf::st_drop_geometry() |>
  dplyr::reframe(Riqueza = dplyr::n_distinct(species),
                   .by = id)

br_grid_trat <- br_grid |>
  dplyr::left_join(riqueza,
                   by = "id") |>
  dplyr::mutate(Riqueza = dplyr::case_when(Riqueza |> is.na() ~ 0,
                                           .default = Riqueza))

ggplot() +
  geom_sf(data = br_grid_trat, aes(color = Riqueza, fill = Riqueza)) +
  scale_color_viridis_c() +
  scale_fill_viridis_c()

## Rasterizando ----

resolucao <- br_grid_trat[1, ] |>
  terra::vect() |>
  terra::ext()

resolucao

vetor <- br_grid_trat |>
  terra::vect() |>
  terra::ext() |>
  terra::rast(res = c((resolucao$xmax - resolucao$xmin),
                      (resolucao$ymax - resolucao$ymin)))

vetor

br_riqueza_rast <- br_grid_trat |>
  terra::vect() |>
  terra::rasterize(vetor, field = "Riqueza")

br_riqueza_rast

ggplot() +
  tidyterra::geom_spatraster(data = br_riqueza_rast) +
  scale_fill_viridis_c(na.value = "transparent")

# Diversidade beta ----

## Matriz de presença-ausência ----

gbif_matriz <- gbif_sf_trat |>
  sf::st_join(br_grid) |>
  sf::st_drop_geometry() |>
  dplyr::mutate(Presenca = 1) |>
  dplyr::distinct(species, id, .keep_all = TRUE) |>
  tidyr::pivot_wider(names_from = species,
                     values_from = Presenca,
                     values_fill = 0)

gbif_matriz

## Calcular Diversidade beta ----

beta_calc <- gbif_matriz |>
  tibble::column_to_rownames(var = "id") |>
  dplyr::select(4:55) |>
  betapart::betapart.core()

beta_calc

beta_pair <- gbif_matriz |>
  tibble::column_to_rownames(var = "id") |>
  dplyr::select(4:55) |>
  beta.pair()

beta_pair

## Ajustando as matrizes para os componentes de diversidade beta ----

beta_sor <- beta_pair$beta.sor |> as.matrix()

beta_sim <- beta_pair$beta.sim |> as.matrix()

beta_sne <- beta_pair$beta.sne |>  as.matrix()

## Valores médios por célula ----

beta_sor_mean <- beta_sor |> rowMeans(na.rm = TRUE)

beta_sim_mean <- beta_sim |> rowMeans(na.rm = TRUE)

beta_sne_mean <- beta_sne |> rowMeans(na.rm = TRUE)

beta_df <- data.frame(id = gbif_matriz$id,
                      beta_total = beta_sor_mean,
                      turnover = beta_sim_mean,
                      nestedness = beta_sne_mean)

beta_df

beta_df |> dplyr::glimpse()

## Rasterizando os três componentes ----

rasters_beta <- function(componente){

  rast_beta <- br_grid |>
    dplyr::left_join(beta_df,
                     by = "id") |>
    terra::vect() |>
    terra::rasterize(vetor, field = componente)

  assign(paste0("raster_beta_", componente),
         rast_beta,
         envir = globalenv())

}

componentes <- beta_df[-1] |> names()

componentes

purrr::walk(componentes, rasters_beta)

## Unindo os rasters ----

beta_rasters <- ls(pattern = "raster_beta_") |>
  mget(envir = globalenv()) |>
  terra::rast()

beta_rasters

beta_rasters |> plot()

## Visualizando ----

ggplot() +
  geom_sf(data = estados, fill = "gray") +
  tidyterra::geom_spatraster(data = beta_rasters) +
  facet_wrap(~lyr) +
  scale_fill_viridis_c(na.value = "transparent")

ggplot() +
  geom_sf(data = estados, fill = "gray") +
  tidyterra::geom_spatraster(data = beta_rasters[[1]]) +
  facet_wrap(~lyr) +
  scale_fill_viridis_c(na.value = "transparent")

ggplot() +
  geom_sf(data = estados, fill = "gray") +
  tidyterra::geom_spatraster(data = beta_rasters[[2]]) +
  facet_wrap(~lyr) +
  scale_fill_viridis_c(na.value = "transparent")

ggplot() +
  geom_sf(data = estados, fill = "gray") +
  tidyterra::geom_spatraster(data = beta_rasters[[3]]) +
  facet_wrap(~lyr) +
  scale_fill_viridis_c(na.value = "transparent")

# Vieses de amosragem ----

## Definindo os Gazetteers ----

gazetteers <- list(air = aeroporto |> terra::vect(),
                   lak = lago |> terra::vect(),
                   urb = urb |> terra::vect(),
                   por = porto |> terra::vect())

gazetteers

## Análise com resolução de 2.0 (~ 200km) e sem definir uma máscara aos dados ----


bias.out <- sampbias::calculate_bias(x = gbif_coords_trat |>
                                       dplyr::select(5:6),
                                     gaz = gazetteers,
                                     res = 2)

bias.out

bias.out |> dplyr::glimpse()

## Projetar os resultados ----

proj <- bias.out |>
  sampbias::project_bias() |>
  terra::crop(br) |>
  terra::mask(br)

proj

ggplot() +
  tidyterra::geom_spatraster(data = proj) +
  scale_fill_viridis_c(na.value = "transparent") +
  facet_wrap(~lyr)

## Mpas de viéses ----

proj |> sampbias::map_bias(type = "log_sampling_rate")
