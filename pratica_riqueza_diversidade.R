# Pacotes ----

library(data.table)

library(tidyverse)

library(CoordinateCleaner)

library(geobr)

library(sf)

library(writexl)

library(terra)

library(raster) #pacote usado para definir e trabalhar com celulas-grid
library(sf) #pacote usado para visualizarmos os resulados
library(rnaturalearthhires) #pacote usado para visualizarmos os resultados
library(rnaturalearthdata) #pacote usado para visualizarmos os resultados
library(viridis) #pacote usado para visualizarmos os resultados
library(betapart) #pacote usado para fazermos diversidade-beta
library(dplyr) #pacote usado para visualizarmos os resultados
library(tidyr) #pacote usado para visualizarmos os resultados
library(rnaturalearth) #pacote usado para visualizarmos os resultados


# Dados ----

## Importando ----

gbif <- data.table::fread("SpiroBR.csv",
                          h = T,
                          encoding = 'UTF-8',
                          quote = "")  |>
  tibble::as_tibble()

## Visualizando ----

gbif

gbif |> dplyr::glimpse()

## Tratando ----

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

vetor <- br_grid_trat |>
  terra::vect() |>
  terra::ext() |>
  terra::rast(res = 0.5)

br_riqueza_rast <- br_grid_trat |>
  terra::vect() |>
  terra::rasterize(vetor, field = "Riqueza")

ggplot() +
  tidyterra::geom_spatraster(data = br_riqueza_rast) +
  scale_fill_viridis_c(na.value = "transparent")

# PASSO 6 - CALCULANDO DIVERSIDADE-BETA E VISUALIZANDO O RESULTADO

# Matriz comunidade: células × espécies (presença/ausência)
mat_pa <- occ_id %>%
  dplyr::select(id_cell, species) %>%
  distinct() %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = species, values_from = value, values_fill = 0) %>%
  as.data.frame()

# Guardar os ID das células
cell_ids <- mat_pa$id_cell
mat_pa <- mat_pa[ , -1]


# Calcular os dados através do trabalho de Baselga (2010)
beta_calc <- betapart.core(mat_pa)

beta_pair <- beta.pair(beta_calc)

# Matriz de faça a faça:
beta_sor  <- as.matrix(beta_pair$beta.sor)  # beta total
beta_sim  <- as.matrix(beta_pair$beta.sim)  # turnover
beta_sne  <- as.matrix(beta_pair$beta.sne)  # nestedness

# Criando valores médios por célula (para mapear), os mapas de matrizes completas são difíceis de se ver, assim se usa o valor médio de beta entre cada célula e todas as demais
beta_sor_mean <- rowMeans(beta_sor, na.rm = TRUE)
beta_sim_mean <- rowMeans(beta_sim, na.rm = TRUE)
beta_sne_mean <- rowMeans(beta_sne, na.rm = TRUE)

beta_df <- data.frame(
  id_cell = cell_ids,
  beta_total = beta_sor_mean,
  turnover = beta_sim_mean,
  nestedness = beta_sne_mean
)

#Convertendo o resultado para raster
r_beta_total <- world_id
r_beta_total[] <- beta_df$beta_total[match(world_id[], beta_df$id_cell)]

r_turnover <- world_id
r_turnover[] <- beta_df$turnover[match(world_id[], beta_df$id_cell)]

r_nestedness <- world_id
r_nestedness[] <- beta_df$nestedness[match(world_id[], beta_df$id_cell)]

# Plotando os resultados de diversidade seguindo mesmo padrão visto até aqui
par(mfrow=c(1,3))

plot(r_beta_total, col = viridis(100),
     main = "Beta total (Sørensen)")
plot(st_geometry(estados), add = TRUE, border = "black")

plot(r_turnover, col = viridis(100),
     main = "Turnover (Simpson)")
plot(st_geometry(estados), add = TRUE, border = "black")

plot(r_nestedness, col = viridis(100),
     main = "Aninhamento (Nestedness)")
plot(st_geometry(estados), add = TRUE, border = "black")

#Salvando o mapa
jpeg("beta_total_SpirostreptidaBR.jpeg", width = 2000, height = 1800, res = 300)
plot(r_beta_total, col = viridis(100), main = "Beta total (Sørensen)")
plot(st_geometry(estados), add = TRUE, border = "black")
dev.off()

jpeg("turnover_SpirostreptidaBR.jpeg", width = 2000, height = 1800, res = 300)
plot(r_turnover, col = viridis(100), main = "Turnover (Simpson)")
plot(st_geometry(estados), add = TRUE, border = "black")
dev.off()

jpeg("nestedness_SpirostreptidaBR.jpeg", width = 2000, height = 1800, res = 300)
plot(r_nestedness, col = viridis(100), main = "Aninhamento (Nestedness)")
plot(st_geometry(br_estados), add = TRUE, border = "black")
dev.off()

###################################################################################################

# PASSO 7 - VERIFICANDO OS VIÉSES AMOSTRAIS E FATORES QUE INFLUENCIAM A DISTRIBUIÇÃO (EXCETO BIOCLIMÁTICOS)
# aqui usaremos o pacote escrito por Zizka e colaboradores em 2017, publicado na Ecography

#pacotes para abrir
library(sampbias)

#definindo novamente o diretório para evitar quaisquer erros
setwd("C:/Users/Motion/Desktop/UFPE/Docência/2025.2/PPGBA/Biogeografia/Aulas/Aula 4 - DADOS/RESULTS")
dir()

#limpa-se todos os dados e chama o arquivo occ novamente
occ <- read.table("gbif_records_clean.txt" , h = T, encoding = "UTF-8", sep= "\t", as.is = T)
occ #conferindo

#definindo a pasta onde estão nossos fatores a serem analisados
setwd("C:/Users/Motion/Desktop/UFPE/Docência/2025.2/PPGBA/Biogeografia/Aulas/Aula 4 - DADOS/DRIVERS")
dir()

#carregar SHPs usando raster::shapefile() — retorna Spatial* diretamente
air   <- terra::vect("airports.shp")
lak      <- terra::vect("lakes.shp")
por      <- terra::vect("ports.shp")
urb  <- terra::vect("urbanareas.shp")


#definindo os Gazetteers
gazetteers <- list(
  air = air,
  lak = lak,
  urb = urb,
  por = por
)

#rodando a análise com resolução de 2.0 (~ 200km) e sem definir uma máscara aos dados
bias.out <- calculate_bias(
  x = occ,
  gaz = gazetteers,
  res = 2
)

#ver os dados gerados no output da análise
summary(bias.out)

# Gera o gráfico
plot(bias.out)

#salvando o output e os mapas no diretório dos resultados
setwd("C:/Users/Motion/Desktop/UFPE/Docência/2025.2/PPGBA/Biogeografia/Aulas/Aula 4 - DADOS/RESULTS")
capture.output(
  summary(bias.out),
  file = "bias_out_summary.txt"
)

jpeg("bias_out_plot.jpeg", width = 2000, height = 2000, res = 300)

plot(bias.out)   # ou a função específica, se houver: plot(bias.out$bias_estimate) etc.

dev.off()

#projetar os resultados
proj <- project_bias(bias.out)

#gerar os mapas de viéses
map_bias(proj, type = "log_sampling_rate")

#apresenta no console todos os resulados obtidos como SpatRaster e os nomes [air,    air+por, air+por+lak, air+p~k+urb, Total~ntage, occurrences]
proj
sapply(proj, class)

#criar rasters do mapa para cada variável, neste caso estamos usando em sequencia de resultados os vistos no comando anterior (prestar atenção nos nomes dos tiff e variável)
for(i in 1:terra::nlyr(proj)) {
  layer <- proj[[i]]
  name  <- names(proj)[i]

  terra::writeRaster(
    layer,
    filename = paste0("bias_", name, ".tif"),
    overwrite = TRUE,
    filetype = "GTiff",
    gdal = c("COMPRESS=DEFLATE")
  )
}




