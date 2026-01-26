# Pacotes ----

library(sf)

library(tidyverse)

library(readxl)

library(parzer)

library(ggrepel)

library(ggspatial)

# Dados ----

## Brasil ----

### Importando ----

br <- sf::st_read("estados.shp")

### Visualizando ----

br

br %>%
  ggplot() +
  geom_sf()

## Nordeste ----

### Importando ----

ne <- br %>%
  dplyr::filter(cod_rgn == 2)

### Visualizando ----

ne

ne %>%
  ggplot() +
  geom_sf()

## Mata Atlântica ----

### Importando ----

ma <- sf::st_read("biomas.shp") %>%
  dplyr::filter(name_biome == "Mata Atlântica")

### Visualizando ----

ma

ma %>%
  ggplot() +
  geom_sf()

## Centro de Endemismo Pernambuco ----

estados <- sf::st_read("estados.shp") %>%
  dplyr::filter(abbrv_s %in% c("AL", "PE", "PB", "RN"))

#### Visualizando ----

estados

estados %>%
  ggplot() +
  geom_sf()

#### Tratando ----

cep <- estados %>%
  sf::st_intersection(ma) %>%
  sf::st_buffer(0) %>%
  sf::st_union() %>%
  sf::st_sf()

cep

cep %>%
  ggplot() +
  geom_sf()

## Coordenadas ----

### Importando ----

coords <- readxl::read_xlsx("anfibios_inventários.xlsx",
                            sheet = 2)

### Visualizando ----

coords

coords %>% dplyr::glimpse()

### Tratando ----

dados_sps <- readxl::read_xlsx("anfibios_inventários.xlsx")

coords_trat <- coords %>%
  dplyr::mutate(Longitude = Longitude %>% parzer::parse_lon(),
                Latitude = Latitude %>% parzer::parse_lat(),
                área = dados_sps$Área %>% unique())

coords_trat

# Mapa ----

## Inset_map ----

inset_map <- ggplot() +
  geom_sf(data = br, color = "black", aes(fill = "Brasil"), linewidth = 0.5) +
  geom_sf(data = ne, color = "black", aes(fill = "Nordeste"), linewidth = 0.5) +
  geom_sf(data = estados, color = "black", aes(fill = "Estados do CEP"), linewidth = 0.5) +
  geom_sf(data = cep, color = "#c1e548", aes(fill = "CEP"), linewidth = 0.75) +
  geom_sf(data = ma, color = "#19530a", aes(fill = "Mata Atlântica"), linewidth = 0.5) +
  geom_sf(data = br, color = "black", fill = "transparent", linewidth = 0.5) +
  scale_fill_manual(values = c("Brasil" = "gray60",
                               "Nordeste" = "#f9ffaf",
                               "Estados do CEP" = "white",
                               "Mata Atlântica" = "transparent",
                               "CEP" = "#c1e548")) +
  geom_rect(aes(xmin = -41.5, xmax = -34.25, ymin = -10.8, ymax = -3.75),
            color = "red",
            fill = "red",
            alpha = 0.5) +
  theme_blank() +
  theme(axis.text = element_blank(),
        axis.line.x  = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none")


inset_map

## Mapa princiapal ----

mapa_principal <- ggplot() +
  geom_sf(data = br, color = "black", aes(fill = "Brasil"), linewidth = 0.75) +
  geom_sf(data = ne, color = "black", aes(fill = "Nordeste"), linewidth = 0.75) +
  geom_sf(data = estados, color = "black", aes(fill = "Estados do CEP"), linewidth = 0.75) +
  geom_sf(data = cep, color = "#c1e548", aes(fill = "CEP"), linewidth = 0.75) +
  geom_sf(data = ma, color = "#19530a", aes(fill = "Mata Atlântica"), linewidth = 0.75) +
  geom_point(data = coords_trat, aes(Longitude, Latitude, fill = "Locais de amostragem"),
             shape = 21,
             size = 3.5,
             color = "black") +
  ggrepel::geom_label_repel(data = coords_trat,
                            aes(Longitude, Latitude, label = área),
                            color = "black",
                            fill = "transparent",
                            direction = "y",
                            nudge_x = -1) +
  geom_sf(data = br, color = "black", fill = "transparent", linewidth = 0.75) +
  coord_sf(xlim = c(-41.5, -34.25),
           ylim = c(-10.8, -3.75),
           label_graticule = "NWSE") +
  scale_fill_manual(values = c("Brasil" = "gray60",
                               "Nordeste" = "#f9ffaf",
                               "Estados do CEP" = "white",
                               "Mata Atlântica" = "transparent",
                               "CEP" = "#c1e548",
                               "Locais de amostragem" = "cyan4"),
                    breaks = c("Brasil",
                               "Mata Atlântica",
                               "Nordeste",
                               "CEP",
                               "Estados do CEP",
                               "Locais de amostragem")) +
  scale_x_continuous(breaks = seq(-41.5, -34.25, 2)) +
  scale_y_continuous(breaks = seq(-10.8, -3.75, 2)) +
  labs(fill = NULL,
       x = NULL,
       y = NULL) +
  ggspatial::annotation_scale(location = "br",
                              height = unit(0.4, "cm"),
                              bar_cols = c("black", "gold"),
                              text_face = "bold",
                              text_col = "black",
                              text_cex = 1) +
  ggspatial::annotation_north_arrow(location = "tr",
                                    height = unit(3, "cm"),
                                    width = unit(3, "cm"),
                                    style = north_arrow_nautical(fill = c("black", "gold"))) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.position = "bottom")

mapa_principal

## Mapa foinal ----

mapa_principal +
  annotation_custom(ggplotGrob(inset_map),
                    ymax = -3.2,
                    ymin = -7,
                    xmin = -42.4,
                    xmax = -37.6)

ggsave("mapa_cep.png", height = 10, width = 12)
