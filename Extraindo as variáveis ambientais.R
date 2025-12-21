# Pacotes ----

library(readxl)

library(tidyverse)

library(geobr)

library(sf)

library(elevatr)

library(terra)

library(tidyterra)

library(geodata)

library(vegan)

library(hillR)

library(writexl)

library(reshape2)

# Dados ----

## Coordenadas e matriz ----

### Importando ----

matriz_fin <- readxl:::read_xlsx("registros_finais.xlsx")

### Visualizando ----

matriz_fin %>% dplyr::glimpse()

matriz_fin

## Centro de Endemismo Pernambuco ----

### Importando ----

cep <- geobr::read_biomes(year = 2019) %>%
  dplyr::filter(name_biome == "Mata Atlântica") %>%
  sf::st_intersection(geobr::read_state(year = 2019) %>%
                        dplyr::filter(abbrev_state %in% c("AL", "PE", "PB", "RN")) %>%
                        sf::st_union())

### Visualizando ----

cep

cep %>%
  ggplot() +
  geom_sf(color = "black", fill = "green4")

## Grade ----

### Importando ----

grade_cep <- sf::st_read("grade_cep.shp")

### Visualizando ----

grade_cep

grade_cep %>%
  ggplot() +
  geom_sf(color = "black", fill = "green4")

## Altitude ----

### Importando ----

alt <- geodata::elevation_30s(country = "BRA",
                              res = 0.5,
                              path = here::here()) %>%
  terra::crop(cep) %>%
  terra::mask(cep)

### Visualizando ----

alt

ggplot() +
  tidyterra::geom_spatraster(data = alt) +
  geom_sf(data = cep, color = "red", fill = "transparent") +
  scale_fill_viridis_c(option = "turbo",
                       na.value = "transparent")

## Uso e cobertura do solo ----

### Importando ----

cobertura <- terra::rast("brasil_coverage_2023.tif") %>%
  terra::crop(cep) %>%
  terra::mask(cep)

### Visualizando ----

cobertura

ggplot() +
  tidyterra::geom_spatraster(data = cobertura) +
  geom_sf(data = cep, color = "black", fill = "transparent") +
  scale_fill_viridis_c(option = "turbo",
                       na.value = "transparent")

## Precipitação total ----

### Importando ----

prec <- geodata::worldclim_country(country = "BRA",
                                   var = "prec",
                                   res = 0.5,
                                   path = here::here()) %>%
  terra::crop(cep) %>%
  terra::mask(cep)

### Visualizando ----

prec

names(prec) <- c("Jan", "Fev", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

ggplot() +
  tidyterra::geom_spatraster(data = prec) +
  geom_sf(data = cep, color = "black", fill = "transparent") +
  facet_wrap(~lyr) +
  scale_fill_viridis_c(option = "turbo",
                       na.value = "transparent")

# Análises ----

## Riqueza ----

matriz_riq <- matriz_fin %>%
  dplyr::filter(Assemblage != 341) %>%
  as.data.frame()

rownames(matriz_riq) <- paste0("Assemblage ", matriz_fin$Assemblage[matriz_fin$Assemblage != 341])

riqueza <- matriz_riq %>%
  dplyr::select(`Leptodactylus fuscus`:`Leptodactylus mystacinus`) %>%
  vegan::specnumber()

riqueza

## Altitude ----

alt_valores <- alt %>%
  terra::extract(grade_cep %>%
                   dplyr::filter(FID %in% matriz_fin$Assemblage[matriz_fin$Assemblage != 341]) %>%
                   sf::st_centroid()) %>%
  tibble::as_tibble() %>%
  dplyr::rename("Assemblage" = ID,
                "Altitude" = BRA_elv_msk) %>%
  dplyr::mutate(Assemblage = matriz_fin$Assemblage[matriz_fin$Assemblage != 341])

alt_valores %>% as.data.frame()

## Precipitação

prec_valores <- prec$Jul %>%
  terra::extract(grade_cep %>%
                   dplyr::filter(FID %in% matriz_fin$Assemblage[matriz_fin$Assemblage != 341]) %>%
                   sf::st_centroid()) %>%
  tibble::as_tibble() %>%
  dplyr::rename("Assemblage" = ID,
                "Precipitação" = Jul) %>%
  dplyr::mutate(Assemblage = matriz_fin$Assemblage[matriz_fin$Assemblage != 341])

prec_valores

## Uso e cobertura do solo ----

### Classes ----

classes_agua <- c(26, 27, 31, 33)

classes_vegetacao <- c(1, 2, 3, 4, 5, 9, 10, 11, 12, 13, 29)

### Calcular proporções e diversidade ----

calcular_metricas <- function(x) {

  x <- x[!is.na(x)]

  if(length(x) == 0) return(c(agua = NA, vegetacao = NA, hill1 = NA, hill2 = NA))

  total_pixels <- length(x)

  agua <- sum(x %in% classes_agua) / total_pixels * 100

  vegetacao <- sum(x %in% classes_vegetacao) / total_pixels * 100

  freq <- table(x)

  hill <- freq %>% hillR::hill_taxa(q = 2)

  return(c(agua = agua, vegetacao = vegetacao, hill2 = hill[1]))
}

cobertura_valores <- extract(cobertura,
                      grade_cep %>%
                        dplyr::filter(FID %in% matriz_fin$Assemblage[matriz_fin$Assemblage != 341]),
                      fun = calcular_metricas) %>%
  tibble::as_tibble() %>%
  dplyr::rename("Assemblage" = ID,
                "% de corpos d'água" = brasil_coverage_2023,
                "% de vegetação nativa" = brasil_coverage_2023.1,
                "Diversidade da paisagem" = brasil_coverage_2023.2) %>%
  dplyr::mutate(Assemblage = matriz_fin$Assemblage[matriz_fin$Assemblage != 341])

cobertura_valores

## Unindo os dataframes ----

valores <- alt_valores %>%
  dplyr::left_join(prec_valores,
                   by = "Assemblage") %>%
  dplyr::left_join(cobertura_valores,
                   by = "Assemblage") %>%
  dplyr::mutate(Riqueza = riqueza,
                Assemblage = Assemblage %>% as.factor(),
                Longitude = matriz_fin %>%
                  dplyr::filter(Assemblage != 341) %>%
                  dplyr::pull(Longitude),
                Latitude = matriz_fin %>%
                  dplyr::filter(Assemblage != 341) %>%
                  dplyr::pull(Latitude))

valores %>% dplyr::glimpse()

valores

## Exportando ---

valores %>%
  writexl::write_xlsx("valores_riqueza.xlsx")

