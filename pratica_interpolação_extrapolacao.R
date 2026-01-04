# Pacotes -----

library(rnaturalearth)

library(tidyverse)

library(rgbif)

library(sf)

library(spThin)

library(geodata)

library(terra)

library(tidyterra)

library(elevatr)

library(reshape2)

library(viridis)

library(ggview)

library(sdm)

library(usdm)

library(ggtext)

library(ggimage)

# Dados ----

## América do Norte ----

### Importando ----

america_norte <- rnaturalearth::ne_countries() |>
  dplyr::filter(name %in% c("Mexico", "Canada", "United States of America"))

### Visualizando -----

america_norte

ggplot() +
  geom_sf(data = america_norte)

## América do Sul ----

### Importando ----

america_sul <- rnaturalearth::ne_countries() |>
  dplyr::filter(continent == "South America")

### Visualizando ----

america_sul

ggplot() +
  geom_sf(data = america_sul)

## Ocorrência de Espécies ----

### Importando ----

gbif <- rgbif::occ_data(scientificName = "Lithobates catesbeianus",
                        hasCoordinate  = TRUE,
                        limit = 1e4,
                        geometry = america_norte |> sf::st_bbox()) %>%
  .$data

### Visualizando ----

gbif

### Tratando os dados ----

gbif_trat <- gbif |>
  dplyr::select(species,
                scientificName,
                decimalLongitude,
                decimalLatitude,
                country,
                basisOfRecord)  |>
  dplyr::filter(!is.na(decimalLongitude) &
                  !is.na(decimalLatitude) &
                  basisOfRecord %in% c("HUMAN_OBSERVATION",
                                       "OBSERVATION",
                                       "MACHINE_OBSERVATION","PRESERVED_SPECIMEN")) |>
  dplyr::distinct(decimalLongitude, decimalLatitude, .keep_all = TRUE)

gbif_trat

ggplot() +
  geom_point(data = gbif_trat,
             aes(decimalLongitude, decimalLatitude))

### Limpando os dados por distância de ~5km ----

gbif_trat |>
  spThin::thin(lat.col = "decimalLatitude",
               long.col = "decimalLongitude",
               spec.col = "species",
               thin.par = 5,
               reps = 5,
               out.dir = getwd())

gbif_thin <- readr::read_csv("thinned_data_thin1.csv")

gbif_thin

### Convertendo em shapefile ----

gbif_vect <- gbif_thin |>
  dplyr::mutate(sp = 1 |> as.numeric()) |>
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
               crs = 4326) |>
  sf::st_intersection(america_norte) |>
  dplyr::select(1:2) |>
  terra::vect()

gbif_vect

gbif_sf <- gbif_thin |>
  dplyr::mutate(sp = 1) |>
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
               crs = 4326) |>
  sf::st_intersection(america_norte) |>
  dplyr::select(1:2)

gbif_sf

ggplot() +
  geom_sf(data = america_norte) +
  geom_sf(data = america_sul) +
  geom_sf(data = gbif_sf)

## Variáveis ambientais ----

### Importando ----

bioclim <- geodata::worldclim_global(var = "bio",
                                     res = 2.5,
                                     path = getwd())

### Visualizando ----

bioclim

ggplot() +
  tidyterra::geom_spatraster(data = bioclim) +
  scale_fill_viridis_c(na.value = NA)

### Recortando ----

bioclim_trat_norte <- bioclim |>
  terra::crop(america_norte |>
                sf::st_union() |>
                sf::st_as_sf()) |>
  terra::mask(america_norte |>
                sf::st_union() |>
                sf::st_as_sf())

bioclim_trat_norte

bioclim_trat_sul <- bioclim |>
  terra::crop(america_sul |>
                sf::st_union() |>
                sf::st_as_sf()) |>
  terra::mask(america_sul |>
                sf::st_union() |>
                sf::st_as_sf())

bioclim_trat_sul

ggplot() +
  tidyterra::geom_spatraster(data = bioclim_trat_norte) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

ggplot() +
  tidyterra::geom_spatraster(data = bioclim_trat_norte[[1]]) +
  scale_fill_viridis_c(na.value = NA)

## Elevação ----

### Importando ----

elev_norte <- elevatr::get_elev_raster(locations = america_norte |>
                                   sf::st_union() |>
                                   sf::st_as_sf(),
                                 z = 5,
                                 prj = 4326,
                                 clip = "locations") |>
  terra::rast()

elev_norte

elev_sul <- elevatr::get_elev_raster(locations = america_sul |>
                                         sf::st_union() |>
                                         sf::st_as_sf(),
                                       z = 5,
                                       prj = 4326,
                                       clip = "locations") |>
  terra::rast()

elev_sul

elev_norte |> terra::writeRaster("elev_norte.tif")

elev_sul |> terra::writeRaster("elev_sul.tif")

### Visualizando ----

elev_norte <- terra::rast("elev_norte.tif")

elev_sul <- terra::rast("elev_sul.tif")

elev_norte

elev_sul

ggplot() +
  tidyterra::geom_spatraster(data = elev_norte) +
  tidyterra::geom_spatraster(data = elev_sul) +
  scale_fill_viridis_c(na.value = NA)

### Tratando a extenção e resolução ----

elev_norte <- elev_norte |>
  terra::resample(bioclim_trat_norte)

elev_norte

elev_sul <- elev_sul |>
  terra::resample(bioclim_trat_sul)

elev_sul

ggplot() +
  tidyterra::geom_spatraster(data = elev_norte) +
  tidyterra::geom_spatraster(data = elev_sul) +
  scale_fill_viridis_c(na.value = NA)

### Unindo os rasters ----

bioclim_trat_norte2 <- c(bioclim_trat_norte, elev_norte)

bioclim_trat_norte2

bioclim_trat_sul2 <- c(bioclim_trat_sul, elev_sul)

bioclim_trat_sul2

names(bioclim_trat_norte2) <- c(paste0("Bio0", 1:9),
                                paste0("Bio", 10:19),
                                "Elevacao")

names(bioclim_trat_sul2) <- c(paste0("Bio0", 1:9),
                              paste0("Bio", 10:19),
                              "Elevacao")
bioclim_trat_norte2

bioclim_trat_sul2

ggplot() +
  tidyterra::geom_spatraster(data = bioclim_trat_norte2) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

ggplot() +
  tidyterra::geom_spatraster(data = bioclim_trat_sul2) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

# Pré-modelagem -----

## Extraindo os dados ----

bioclim_dados <- bioclim_trat_norte2 |>
  terra::extract(gbif_sf)

bioclim_dados

## Testando a existência de NA ----

linhas <- bioclim_dados |>
  dplyr::filter(dplyr::if_any(dplyr::everything(), ~is.na(.x))) |>
  dplyr::pull(ID)

linhas

## Removendo o registro com NA ----

gbif_sf_trat <- gbif_sf[-linhas, ]

gbif_sf_trat

gbif_vect_trat <- gbif_vect[-linhas, ]

gbif_vect_trat

bioclim_dados_trat <- bioclim_dados[-linhas, ]

bioclim_dados_trat

## Multicolinearidade ----

### Criando a matriz ----

cor_matriz <- bioclim_dados_trat |>
  dplyr::select(-1) |>
  cor(method = "spearman") |>
  as.matrix()

cor_matriz

### Tratando a matriz ----

cor_matriz[upper.tri(cor_matriz)] <- NA

cor_matriz

### Gráfico comparativo -----

cor_matriz |>
  reshape2::melt() |>
  tidyr::drop_na() |>
  dplyr::mutate(igual = dplyr::case_when(Var1 == Var2 ~ "Sim",
                                         .default = "Não"),
                value = value |> round(2)) |>
  dplyr::filter(igual == "Não") |>
  dplyr::select(Var1, Var2, `Índice de Correlação` = value) |>
  ggplot(aes(Var1, Var2, fill = `Índice de Correlação`, label = `Índice de Correlação`)) +
  geom_tile(color = "black") +
  geom_text() +
  coord_equal() +
  labs(x = NULL,
       y = NULL) +
  scale_fill_gradientn(colours = c(viridis::viridis(n = 10) |> rev(),
                                   viridis::viridis(n = 10)),
                       limits = c(-1, 1),
                       guide = guide_colorbar(title.hjust = 0.5,
                                              frame.colour = "black",
                                              ticks.colour = "black",
                                              barheight = 20)) +
  theme_minimal() +
  theme(axis.text = element_text(size = 15, color = "black"),
        axis.text.x = element_text(size = 15, color = "black", angle = 90, hjust = 1),
        legend.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 15, color = "black")) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "coorelacao_vars.png", height = 10, width = 12)

## Seleção de variáveis ----

# Remover: bio1, bio3, bio4, bio5, bio6, bio7, bio9, bio10, bio12, bio14, bio15, bio 16

bioclim_dados_sele <- bioclim_dados_trat |>
  dplyr::select(-c(2, 4:11, 13, 15:17))

bioclim_dados_sele

bioclim_sele_norte <- bioclim_trat_norte2[[-c(1, 3:10, 12, 14:16)]]

bioclim_sele_norte

bioclim_sele_sul <- bioclim_trat_sul2[[-c(1, 3:10, 12, 14:16)]]

bioclim_sele_sul

ggplot() +
  tidyterra::geom_spatraster(data = bioclim_sele_norte) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

ggplot() +
  tidyterra::geom_spatraster(data = bioclim_sele_sul) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~lyr)

# Modelagem ----

## Objeto sdmData ----

sdmdata <- sdm::sdmData(sp ~ .,
                        train = gbif_vect_trat,
                        predictors = bioclim_sele_norte,
                        bg = list(method = "gRandom", n = 1000))

sdmdata

## Ajuste do modelo ----

sdm::getmethodNames()

sdm_modelo <- sdm::sdm(sp ~ .,
                       data = sdmdata,
                       methods = c("rf",
                                   "glmpoly",
                                   "maxent"),
                       replication = "sub",
                       test.percent = 30,
                       n = 5)

sdm_modelo

## Exportando o modelo ----

sdm_modelo |> sdm::write.sdm("modelo_aquarana.sdm",
                             overwrite = TRUE)

## Predição ----

sdm_modelo <- sdm::read.sdm("modelo_aquarana.sdm")

sdm_modelo

sdm_predict <- terra::predict(sdm_modelo,
                              bioclim_sele_sul,
                              overwrite = TRUE)

sdm_predict |> terra::writeRaster("raster_predict.tif",
                                  overwrite = TRUE)

sdm_predict <- terra::rast("raster_predict.tif")

sdm_predict

ggplot() +
  tidyterra::geom_spatraster(data = sdm_predict) +
  facet_wrap(~lyr) +
  scale_fill_viridis_c(na.value = NA)

## Ensemble do modelo ----

### Dataframe da imagem da aquarana ----

aquarana_df <- tibble::tibble(x = -40,
                              y = 9.5,
                              img = "aquarana_catesbeiana.png")

aquarana_df

### Por AUC ----

sdm_ensemble_auc <- sdm::ensemble(sdm_modelo,
                              newdata = sdm_predict,
                              setting = list(method = "weighted",
                                             stat = "AUC"))

sdm_ensemble_auc |> terra::writeRaster("raster_ensemble_auc.tif",
                                       overwrite = TRUE)

sdm_ensemble_auc <- terra::rast("raster_ensemble_auc.tif")

sdm_ensemble_auc

ggplot() +
  tidyterra::geom_spatraster(data = sdm_ensemble_auc) +
  geom_sf(data = america_sul, color = "black", fill = NA, linewidth = 1) +
  labs(title = "Adequabilidade ambiental de <i>Aquarana castebeina</i> na América do Sul, baseado em AUC",
       x = NULL,
       y = NULL,
       fill = "Adequabilidade Ambiental") +
  scale_fill_viridis_c(na.value = NA,
                       limits = c(0, 1),
                       guide = guide_colorbar(title.hjust = 0.5,
                                              frame.colour = "black",
                                              ticks.colour = "black",
                                              barheight = 20)) +
  ggimage::geom_image(data = aquarana_df, aes(x, y, image = img),
                      size = 0.2) +
  theme_minimal() +
  theme(plot.title = ggtext::element_markdown(size = 15, hjust = 0.5),
        axis.text = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 15, color = "black")) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "aquarana_modelo_auc.png", height = 10, width = 12)

### Por TSS ----

sdm_ensemble_tss <- sdm::ensemble(sdm_modelo,
                                  newdata = sdm_predict,
                                  setting = list(method = "weighted",
                                                 stat = "TSS"))

sdm_ensemble_tss |> terra::writeRaster("raster_ensemble_tss.tif",
                                       overwrite = TRUE)

sdm_ensemble_tss <- terra::rast("raster_ensemble_tss.tif")

sdm_ensemble_tss

ggplot() +
  tidyterra::geom_spatraster(data = sdm_ensemble_tss) +
  geom_sf(data = america_sul, color = "black", fill = NA, linewidth = 1) +
  labs(title = "Adequabilidade ambiental de <i>Aquarana castebeina</i> na América do Sul, baseado em TSS",
       x = NULL,
       y = NULL,
       fill = "Adequabilidade Ambiental") +
  scale_fill_viridis_c(na.value = NA,
                       limits = c(0, 1),
                       guide = guide_colorbar(title.hjust = 0.5,
                                              frame.colour = "black",
                                              ticks.colour = "black",
                                              barheight = 20)) +
  ggimage::geom_image(data = aquarana_df, aes(x, y, image = img),
                      size = 0.2) +
  theme_minimal() +
  theme(plot.title = ggtext::element_markdown(size = 15, hjust = 0.5),
        axis.text = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 15, color = "black")) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "aquarana_modelo_tss.png", height = 10, width = 12)

# Pós-Modelagem ----

## AUC ----

sdm_modelo |> sdm::getEvaluation(stat = c("AUC", "TSS"))

sdm_modelo |> sdm::roc()

## Histograma dos valores das predições ----

sdm_ensemble_auc |>
  terra::values() |>
  tibble::tibble() |>
  dplyr::rename("Environmental Suitability" = 1) |>
  tidyr::drop_na() |>
  ggplot(aes(`Environmental Suitability`)) +
  geom_histogram(binwidth = 0.01, color = "black")

## Variáveis mais relevantes ----

sdm_modelo |> sdm::getVarImp()

sdm_modelo |> sdm::getVarImp() |> plot() + theme_minimal()

## Curva de resposta ----

sdm_modelo |> sdm::getResponseCurve()

sdm_modelo |> sdm::getResponseCurve() |> plot() + theme_bw()

## Área de nicho ----

### Área de nicho ----

area_nicho_ensemble <- sdm::pa(sdm_ensemble_tss,
                               sdm_modelo) |>
  terra::as.polygons() |>
  sf::st_as_sf(crs = 4326) |>
  dplyr::rename("tipo" = ensemble_weighted) |>
  dplyr::mutate(tipo = dplyr::case_when(tipo == 1 ~ "Presente",
                                        .default = "Ausente"))

area_nicho_ensemble

ggplot() +
  geom_sf(data = area_nicho_ensemble, aes(fill = tipo, color = tipo))+
  geom_sf(data = america_sul, color = "black", fill = NA, linewidth = 1) +
  labs(title = "Área de nicho de <i>Aquarana castebeina</i> na América do Sul",
       x = NULL,
       y = NULL,
       fill = NULL,
       color = NULL) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  ggimage::geom_image(data = aquarana_df, aes(x, y, image = img),
                      size = 0.2) +
  theme_minimal() +
  theme(plot.title = ggtext::element_markdown(size = 15, hjust = 0.5),
        axis.text = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 15, color = "black")) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "aquarana_modelo_area_nicho_ensemble.png", height = 10, width = 12)

### Com threshold menor ----

area_nicho_predict <- sdm::pa(sdm_predict,
                              sdm_modelo) |>
  terra::as.polygons() |>
  sf::st_as_sf(crs = 4326) |>
  dplyr::rename("tipo" = id_1__sp_sp__m_rf__re_subs) |>
  dplyr::mutate(tipo = dplyr::case_when(tipo == 1 ~ "Presente",
                                        .default = "Ausente"))

area_nicho_predict

ggplot() +
  geom_sf(data = area_nicho_predict, aes(fill = tipo, color = tipo))+
  geom_sf(data = america_sul, color = "black", fill = NA, linewidth = 1) +
  labs(title = "Área de nicho de <i>Aquarana castebeina</i> na América do Sul",
       x = NULL,
       y = NULL,
       fill = NULL,
       color = NULL) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  ggimage::geom_image(data = aquarana_df, aes(x, y, image = img),
                      size = 0.2) +
  theme_minimal() +
  theme(plot.title = ggtext::element_markdown(size = 15, hjust = 0.5),
        axis.text = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 15, color = "black")) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "aquarana_modelo_area_nicho_predict.png", height = 10, width = 12)
