# Pacotes ----

library(geobr)

library(tidyverse)

library(sf)

library(cowplot)

# Dados ----

## Brasil ----

### Importando ----

br <- geobr::read_country(year = 2019)

### Visualizando ----

br

ggplot() +
  geom_sf(data = br, color = "black")

## Nordeste ----

### Importando ----

ne <- geobr::read_state(year = 2019) |>
  dplyr::filter(abbrev_state %in% c("AL", "PE", "PB", "RN"))

### Visualizando ----

ne

ggplot() +
  geom_sf(data = ne, color = "black")

## Malha de grade ----

### Importando ----

grade_cep <- sf::st_read("grade_cep.shp")

### Visualizando ----

grade_cep

ggplot() +
  geom_sf(data = grade_cep, color = "black")

# Mapa ----

## Mapa principal ----

mapa_princ <- ggplot() +
  geom_sf(data = br, color = "black", aes(fill = "Brasil"), linewidth = 0.5) +
  geom_sf(data = ne, color = "black", aes(fill = "Estados do CEP"),
          linewidth = 0.5) +
  geom_sf(data = grade_cep, fill = "transparent",
          aes(color = "Malha de Grade"),
          linewidth = 0.5) +
  coord_sf(xlim = c(-41.3585, -32.3778),
           ylim = c(-10.501, -3.805),
           label_graticule = "NSEW") +
  scale_fill_manual(values = c("Brasil" = "gray40",
                               "Estados do CEP" = "white")) +
  scale_color_manual(values = c("Brasil" = "gray40",
                               "Malha de Grade" = "darkgreen")) +
  labs(fill = NULL,
       color = NULL) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 17.55, face = "bold"),
        legend.text = element_text(color = "black", size = 15, face = "bold"),
        legend.title = element_text(color = "black", size = 15),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", linewidth = 1))

mapa_princ

## Insert map ----

insert_map <- ggplot() +
  geom_sf(data = br, color = "black", fill = "gray40", linewidth = 0.5) +
  geom_sf(data = ne, color = "black", fill = "white",
          linewidth = 0.5) +
  geom_sf(data = grade_cep, fill = "transparent",
          color = "darkgreen") +
  coord_sf(label_graticule = "NSEW") +
  labs(fill = NULL,
       color = NULL) +
  geom_rect(aes(xmin = -41.3585,
                xmax = -32.3778,
                ymin = -10.501,
                ymax = -3.805),
            color = "darkred",
            fill = "red",
            alpha = 0.5) +
  theme_void()

insert_map

## Combinando os mapas ----

mapa_princ |>
  cowplot::ggdraw() +
  cowplot::draw_plot(insert_map,
                     x = 0.6,
                     y = 0.55,
                     width = 0.35,
                     height = 0.35) +
  ggview::canvas(height = 10, width = 12)

ggsave("mapa_cep.png", height = 10, width = 12)
