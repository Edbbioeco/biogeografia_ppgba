# Pacotes ----

library(readxl)

library(tidyverse)

library(parzer)

library(sf)

library(openxlsx)

# Dados ----

## Ocorrências ----

### Importando ----

oc_bib <- readxl::read_xlsx("anfibios_inventários.xlsx")

### Visualizanddo ----

oc_bib %>% dplyr::glimpse()

oc_bib

## Coordenadas ----

### Importando ----

coord_bib <- readxl::read_xlsx("anfibios_inventários.xlsx",
                               sheet = 2)

### Visualizanddo ----

coord_bib %>% dplyr::glimpse()

coord_bib

### Tratando ----

coord_bib_trat <- coord_bib %>%
  dplyr::select(Área:Latitude) %>%
  dplyr::mutate(Longitude = Longitude %>% parzer::parse_lon(),
                Latitude = Latitude %>% parzer::parse_lat())

coord_bib_trat

## Tratando as ocorrencias ----

oc_bib_trat <- oc_bib %>%
  dplyr::left_join(coord_bib_trat,
                   by = "Área")

oc_bib_trat

## Grade ----

### Importando ----

grade_cep <- sf::st_read("grade_cep.shp")

### Visualizando ----

grade_cep

grade_cep %>%
  ggplot() +
  geom_sf(color = "black", fill = "green4")

# Comunidades ----

## Criando um shapefile das ocorrências ----

oc_bib_shp <- oc_bib_trat %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = grade_cep %>% sf::st_crs())

oc_bib_shp

ggplot() +
  geom_sf(data = grade_cep) +
  geom_sf(data = oc_bib_shp)

## Espécies por grade ----

### Intersecção ----

oc_bib_inter <- grade_cep %>% sf::st_join(oc_bib_shp, join = st_intersects) %>%
  dplyr::filter(!is.na(Espécie)) %>%
  tibble::as_tibble() %>%
  dplyr::select(FID:Família, Espécie) %>%
  dplyr::rename("family" = Família,
                "species" = Espécie,
                "presence" = Presença) %>%
  dplyr::select(-Área) %>%
  dplyr::relocate(species:presence, .after = family)

oc_bib_inter

### Matriz ----

oc_bib_inter %>%
  tidyr::pivot_wider(names_from = species,
                     values_from = presence,
                     values_fn = function(x) 1,
                     values_fill = 0)

### Exportando ----

oc_bib_inter %>%
  openxlsx::write.xlsx("registros_bib.xslx")
