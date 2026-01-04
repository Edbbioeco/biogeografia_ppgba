# Pacotes ----

library(tidyverse)

library(sf)

library(terra)

library(tidyterra)

library(phyloraster)

library(phangorn)

library(SESraster)

library(ggtree)

# Dados ----

## Importando ----

linhas <- readr::read_lines("tsab_a_2039796_sm0766.xyd")

## Visualizando ----

linhas

## Transformando em um dataframe ----

df_occ <- linhas |>
  tibble::tibble() |>
  dplyr::slice(-c(1:4)) |>
  tidyr::separate(linhas,
                  into = c("long", "lat"),
                  sep = ",") |>
  dplyr::mutate(Espécie = dplyr::case_when(lat |> is.na() ~ long,
                                           .default = NA)) |>
  tidyr::fill(Espécie) |>
  tidyr::drop_na() |>
  dplyr::mutate(long = long |> as.numeric(),
                lat = lat |> as.numeric())

df_occ

df_occ |> dplyr::glimpse()

# Grade -----

## Transformando as ocorrencias em um shapefile ----

df_sf <- df_occ |>
  sf::st_as_sf(coords = c("long", "lat"),
               crs = 4674)

ggplot() +
  geom_sf(data = df_sf)

## Criando a grade ----

grade <- df_sf |>
  sf::st_make_grid(cellsize = c(6.500, 6.980),
                          offset = c(-96.000, -48.000)) |>
  sf::st_as_sf() |>
  dplyr::mutate(id = dplyr::row_number())

grade

ggplot() +
  geom_sf(data = grade) +
  geom_sf(data = df_sf)

## Calculando a intersecção ----

grade_id <- df_sf |>
  sf::st_join(grade)

grade_id

# Endemismo ----

## Abrangência da espécie ----

abg_sps <- grade_id |>
  sf::st_drop_geometry() |>
  dplyr::summarise(celulas = dplyr::n_distinct(id),
                   .by = Espécie)

abg_sps

## Endemicidade ----

valores_we <- grade_id |>
  sf::st_drop_geometry() |>
  dplyr::left_join(abg_sps,
                   by = "Espécie") |>
  dplyr::summarise(WE = sum(1 / celulas),
                   .by = id)

valores_we

## Mapa ----

### Unindo os valores ----

grade_id_we <- grade |>
  dplyr::left_join(valores_we,
                   by = "id") |>
  dplyr::mutate(Area = dplyr::case_when(WE >= 500 ~ "Endemismo",
                                        WE < 500 ~ "ground",
                                        .default = NA))

grade_id_we

### Mapa ----

ggplot() +
  geom_sf(data = grade_id_we, aes(color = WE, fill = WE)) +
  scale_color_viridis_c(na.value = "transparent") +
  scale_fill_viridis_c(na.value = "transparent")

ggplot() +
  geom_sf(data = grade_id_we, aes(color = Area, fill = Area)) +
  scale_color_viridis_d(na.value = "transparent", direction = -1) +
  scale_fill_viridis_d(na.value = "transparent", direction = -1)

# Endemismo com o pacote phyloraster ----

## Gerando multiplos rasters

multiplos_rasters <- function(especie){

  raster_inicial <- grade |>
    dplyr::left_join(grade_id |>
                       sf::st_drop_geometry(), by = "id") |>
    dplyr::mutate(presenca = 1) |>
    tidyr::drop_na() |>
    tidyr::pivot_wider(names_from = Espécie,
                       values_from = presenca,
                       values_fill = 0,
                       values_fn = ~max(.)) |>
    tidyr::pivot_longer(cols = 3:146,
                        names_to = "Espécie",
                        values_to = "presenca") |>
    dplyr::filter(Espécie == especie) |>
    terra::vect() |>
    terra::ext() |>
    terra::rast(resolution = c(6.500, 6.980))


  raster_sps <- grade |>
    dplyr::left_join(grade_id |>
                     sf::st_drop_geometry(),
                   by = "id") |>
    dplyr::mutate(presenca = 1) |>
    tidyr::drop_na() |>
    tidyr::pivot_wider(names_from = Espécie,
                       values_from = presenca,
                       values_fill = 0,
                       values_fn = ~ max(.)) |>
    tidyr::pivot_longer(cols = 3:146,
                        names_to = "Espécie",
                        values_to = "presenca") |>
    dplyr::filter(Espécie == especie) |>
    terra::vect() |>
    terra::rasterize(raster_inicial, field = "presenca")

  raster_sps |> plot()

  assign(paste0("raster_occ_", especie),
         raster_sps,
         envir = globalenv())


}

especie <- df_occ$Espécie |> unique()

especie

purrr::walk(especie, multiplos_rasters)

## Unindo os rasters -----

occ_raster <- ls(pattern = "raster_occ_") |>
  mget(envir = globalenv()) |>
  terra::rast()

occ_raster

occ_raster |> plot()

## Exportando ----

occ_raster |>
  terra::writeRaster("occ_raster.tif")

occ_raster <- terra::rast("occ_raster.tif")

## Calculando EA através do pacote phyloraster ----

we_raster <- phyloraster::rast.we(occ_raster,
                                   occ_raster |> phyloraster::inv.range())

we_raster

we_raster |> plot()

ggplot() +
  tidyterra::geom_spatraster(data = we_raster) +
  scale_fill_viridis_c(na.value = "transparent")

## Calculando EA padronizado pela riqueza de espécies através do pacote phyloraster ----

we_raster_ses <- phyloraster::rast.we.ses(occ_raster,
                                          occ_raster |> phyloraster::inv.range())

we_raster_ses

we_raster_ses |> plot()

ggplot() +
  tidyterra::geom_spatraster(data = we_raster_ses) +
  scale_fill_viridis_c(na.value = "transparent") +
  facet_wrap(~lyr)

# Análise de Parcimônia de Endemicidade ----

## Construindo a Matriz -----

matriz_pae <- grade |>
  dplyr::left_join(grade_id |>
                     sf::st_drop_geometry(),
                   by = "id") |>
  dplyr::mutate(presenca = 1,
                id = id |> as.character()) |>
  tidyr::drop_na() |>
  sf::st_drop_geometry() |>
  tidyr::pivot_wider(names_from = Espécie,
                     values_from = presenca,
                     values_fill = 0,
                     values_fn = ~max(.))


outgroup <- c("Outgroup", rep(0, 144)) |>
  t() |>
  as.data.frame() |>
  dplyr::rename_all(~matriz_pae |> names()) |>
  dplyr::mutate(dplyr::across(.cols = 2:145, ~as.numeric(.)))

outgroup

outgroup |> glimpse()

matriz_pae <- matriz_pae |>
  dplyr::bind_rows(outgroup) |>
  tibble::column_to_rownames(var = "id") |>
  as.matrix()

matriz_pae

## Criando um objeto phylodat ----

pae_phydat <- matriz_pae |>
  phangorn::phyDat(type = "USER",
                   levels = c(0, 1))

pae_phydat

## Análise de parsimônia ----

parsi <- pae_phydat |>
  phangorn::pratchet()

parsi

## Gráfico ----

parsi |>
  ggtree::ggtree(layout = "circular",
                 color = "black",
                 linewidth = 0.75) +
  ggtree::geom_tiplab(size = 5)
