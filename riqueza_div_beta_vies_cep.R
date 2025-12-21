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

## Capitais ----

### Importando ----

capitais <- geobr::read_municipality() |>
  dplyr::filter(name_muni %in% c("Maceió",
                                 "Recife",
                                 "João Pessoa",
                                 "Natal"))

### Visualizando ----

capitais

ggplot() +
  geom_sf(data = capitais)

### Vetorixando ----

capitais_vec <- capitais |>
  terra::vect()

capitais_vec

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

terra::crs(cep_vetor) <- cep |> terra::crs()

terra::ext(cep_vetor) <- cep |> terra::ext()

cep_riqueza_rast <- cep_trat |>
  terra::vect() |>
  terra::rasterize(cep_vetor, field = "Riqueza")

cep_riqueza_rast

terra::crs(cep_riqueza_rast) <- cep |> terra::crs()

terra::ext(cep_riqueza_rast) <- cep |> terra::ext()

cep_riqueza_rast

ggplot() +
  tidyterra::geom_spatraster(data = cep_riqueza_rast) +
  geom_sf(data = cep, fill = NA, color = "black") +
  scale_fill_viridis_c(option = "turbo",
                       na.value = "transparent")

# Diversidade beta ----

## Criando uma matriz de dissimilaridade ----

matriz_beta <- registros_sf |>
  sf::st_join(cep) |>
  sf::st_drop_geometry() |>
  dplyr::select(-c(1, 4)) |>
  tidyr::pivot_wider(names_from = species,
                     values_from = presence,
                     values_fill = 0,
                     values_fn = ~max(.)) |>
  tibble::column_to_rownames(var = "FID")

matriz_beta

matriz_beta |> dplyr::glimpse()

## Calculando a diversidade beta de Jaccard ----

beta_calc <- matriz_beta |>
  betapart::betapart.core()

beta_calc

beta_pair <- matriz_beta |>
  beta.pair(index.family = "jaccard")

beta_pair

## Ajustando as matrizes para os componentes de diversidade beta ----

beta_sor <- beta_pair$beta.jac |> as.matrix()

beta_tur <- beta_pair$beta.jtu |> as.matrix()

beta_sne <- beta_pair$beta.jne |>  as.matrix()

## Valores médios por célula ----

beta_sor_mean <- beta_sor |> rowMeans(na.rm = TRUE)

beta_tur_mean <- beta_tur |> rowMeans(na.rm = TRUE)

beta_sne_mean <- beta_sne |> rowMeans(na.rm = TRUE)

beta_df <- data.frame(id = matriz_beta |> rownames() |> as.numeric(),
                      jaccard = beta_sor_mean,
                      turnover = beta_tur_mean,
                      nestedness = beta_sne_mean)

beta_df

beta_df |> dplyr::glimpse()

## Rasterizando os três componentes ----

rasters_beta <- function(componente){

  rast_beta <- cep |>
    dplyr::rename("id" = FID) |>
    dplyr::left_join(beta_df,
                     by = "id") |>
    terra::vect() |>
    terra::rasterize(cep_vetor, field = componente)

  terra::crs(rast_beta) <- cep |> terra::crs()

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
  geom_sf(data = cep, fill = "gray") +
  tidyterra::geom_spatraster(data = beta_rasters) +
  geom_sf(data = cep, fill = NA, color = "black") +
  facet_wrap(~lyr) +
  scale_fill_viridis_c(option = "turbo",
                       na.value = "transparent")

ggplot() +
  geom_sf(data = cep, fill = "gray") +
  tidyterra::geom_spatraster(data = beta_rasters[[1]]) +
  geom_sf(data = cep, fill = NA, color = "black") +
  facet_wrap(~lyr) +
  scale_fill_viridis_c(option = "turbo",
                       na.value = "transparent")

ggplot() +
  geom_sf(data = cep, fill = "gray") +
  tidyterra::geom_spatraster(data = beta_rasters[[2]]) +
  geom_sf(data = cep, fill = NA, color = "black") +
  facet_wrap(~lyr) +
  scale_fill_viridis_c(option = "turbo",
                       na.value = "transparent")

ggplot() +
  geom_sf(data = cep, fill = "gray") +
  tidyterra::geom_spatraster(data = beta_rasters[[3]]) +
  geom_sf(data = cep, fill = NA, color = "black") +
  facet_wrap(~lyr) +
  scale_fill_viridis_c(option = "turbo",
                       na.value = "transparent")

# Vi[es de amostragem ----

## Definindo os Gazetteers ----

gazetteers <- list(`Unidades de Conservação` = uc_cep_vec,
                   Capitais = capitais_vec)

gazetteers

## Análise com resolução de 2.0 (~ 200km) e sem definir uma máscara aos dados ----


bias.out <- sampbias::calculate_bias(x = registros |>
                                       dplyr::select(5:6) |>
                                       dplyr::rename("decimalLongitude" = Longitude,
                                                     "decimalLatitude" = Latitude),
                                     gaz = gazetteers,
                                     res = terra::res(cep_riqueza_rast)[1],
                                     plot_raster = TRUE)

bias.out

bias.out |> dplyr::glimpse()

bias.out |> plot()

## Projetar os resultados ----

proj <- bias.out |>
  sampbias::project_bias() |>
  terra::crop(cep) |>
  terra::mask(cep)

proj

ggplot() +
  tidyterra::geom_spatraster(data = proj) +
  scale_fill_viridis_c(na.value = "transparent") +
  geom_sf(data = uc_cep, fill = NA, color = "red") +
  facet_wrap(~lyr) +
  coord_sf(ylim = c(-11, -5),
           xlim = c(-37, -34.8))

ggplot() +
  tidyterra::geom_spatraster(data = proj[[1]]) +
  scale_fill_viridis_c(na.value = "transparent") +
  geom_sf(data = uc_cep, fill = NA, color = "red") +
  facet_wrap(~lyr) +
  coord_sf(ylim = c(-11, -5),
           xlim = c(-37, -34.8))

ggplot() +
  tidyterra::geom_spatraster(data = proj[[2]]) +
  scale_fill_viridis_c(na.value = "transparent") +
  geom_sf(data = uc_cep, fill = NA, color = "red") +
  geom_sf(data = capitais, fill = NA, color = "red") +
  facet_wrap(~lyr) +
  coord_sf(ylim = c(-11, -5),
           xlim = c(-37, -34.8))

ggplot() +
  tidyterra::geom_spatraster(data = proj[[3]]) +
  scale_fill_viridis_c(na.value = "transparent") +
  geom_sf(data = uc_cep, fill = NA, color = "red") +
  geom_sf(data = capitais, fill = NA, color = "red") +
  facet_wrap(~lyr) +
  coord_sf(ylim = c(-11, -5),
           xlim = c(-37, -34.8))

ggplot() +
  tidyterra::geom_spatraster(data = proj[[4]]) +
  scale_fill_viridis_c(na.value = "transparent") +
  facet_wrap(~lyr) +
  coord_sf(ylim = c(-11, -5),
           xlim = c(-37, -34.8))

## Mpas de viéses ----

proj |> sampbias::map_bias(type = "log_sampling_rate")
