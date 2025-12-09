# Pacotes ----

library(geobr)

library(tidyverse)

library(sf)

library(readxl)

library(ggsflabel)

library(fields)

library(dbscan)

# Dados ----

## Grade ----

### Importando ----

grade_cep <- sf::st_read("grade_cep.shp")

### Visualizando ----

grade_cep

grade_cep %>%
  ggplot() +
  geom_sf(color = "black", fill = "green4")

## Registros ----

### Importando ----

matriz_trat <- readxl::read_xlsx("matriz_trat.xlsx")

### Transformando em shapefile ----

matriz_shp <- matriz_trat %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = grade_cep %>% sf::st_crs())

### Visualizando ----

matriz_trat

matriz_shp

ggplot() +
  geom_sf(data = grade_cep, color = "black", fill = "green4") +
  geom_sf(data = matriz_shp)

### Excluindo as assembleias com menos de 5 espécies ----

assembleias <- matriz_trat %>%
  tidyr::pivot_longer(cols = 5:104,
                      names_to = "Species",
                      values_to = "Presence") %>%
  dplyr::filter(Presence == 1) %>%
  dplyr::summarise(`Número de Espécies` = Species %>% dplyr::n_distinct(),
                   .by = Assemblage) %>%
  dplyr::arrange(`Número de Espécies`) %>%
  dplyr::filter(`Número de Espécies` < 5) %>%
  dplyr::pull(Assemblage)

assembleias

matriz_shp_trat <- matriz_shp %>%
  dplyr::filter(!Assemblage %in% assembleias)

ggplot() +
  geom_sf(data = grade_cep, color = "black", fill = "green4") +
  ggsflabel::geom_sf_label_repel(data = matriz_shp_trat, aes(label = Assemblage))

matriz_trat <- matriz_trat %>%
  dplyr::filter(!Assemblage %in% assembleias)

matriz_trat

# Clusterizando por distância < 10km ----

## Matriz de distância geográfica ----

dist_matrix <- matriz_trat %>%
  dplyr::select(Longitude:Latitude) %>%
  as.data.frame %>%
  fields::rdist.earth(miles = FALSE) %>%
  as.dist()

dist_matrix

## Calculando os clusters ----

dbscan_result <- dbscan::dbscan(dist_matrix,
                                eps = 10,
                                minPts = 2)

dbscan_result

## Criar um data frame de identificação dos clusters ----

clusters <- tibble::tibble(Assemblage = matriz_trat$Assemblage,
                           Cluster = dplyr::if_else(dbscan_result$cluster == 0,
                                                    "Sem Cluster",
                                                    paste0("Cluster ", dbscan_result$cluster))
)

clusters

## Adicionando a coluna de Cluster à matriz original ----

matriz_trat_2 <- matriz_trat %>%
  dplyr::left_join(clusters,
                   by = "Assemblage") %>%
  dplyr::relocate(Cluster, .after = Source)

matriz_trat_2

## Checando cada Cluster ----

### Criando uma função ----

checando_Clusters <- function(x){

  message(stringr::str_glue("# Avaliação para o {x}"))

  message("## Matriz")

  matriz_trat_2 %>%
    dplyr::filter(Cluster == x) %>%
    dplyr::select(1, 5:105) %>%
    dplyr::select(dplyr::where(~ any(. != 0))) %>%
    as.data.frame() %>%
    print()

  message("## Número de espécies")

  matriz_trat_2 %>%
    dplyr::filter(Cluster == x) %>%
    dplyr::select(1, 5:105) %>%
    dplyr::select(dplyr::where(~ any(. != 0))) %>%
    dplyr::rename("rowname" = Assemblage) %>%
    tibble::column_to_rownames() %>%
    as.data.frame() %>%
    dplyr::select_if(is.numeric) %>%
    vegan::specnumber() %>%
    print()

  message("## Dissimilaridade")

  matriz_trat_2 %>%
    dplyr::filter(Cluster == x) %>%
    dplyr::select(1, 5:105) %>%
    dplyr::select(dplyr::where(~ any(. != 0))) %>%
    dplyr::rename("rowname" = Assemblage) %>%
    tibble::column_to_rownames() %>%
    as.data.frame() %>%
    dplyr::select_if(is.numeric) %>%
    vegan::vegdist(method = "jaccard") %>%
    print()

}

### Checando ----

purrr::walk(matriz_trat_2 %>%
              dplyr::filter(Cluster != "Sem Cluster") %>%
              dplyr::pull(Cluster) %>%
              unique(),
            checando_Clusters)

### Avaliação ----

# Cluster 1: manter a Assemblage 16 e remover Assemblage 8
# Cluster 2: manter a Assemblage 88 e remover Assemblage 78
# Cluster 3: manter a Assemblage 239 e 240 e remover a Assemblage 226
# Cluster 4: manter a Assemblage 247 e remover as Assemblage 234 e 235
# Cluster 5: manter a Assemblage 323, 347, 348, 338 e remover a Assemblage 324, 330, 331, 337, 340, 349
# Cluster 6: manter a Assemblage 417, 416 e remover a Assemblage 410, 411
# Cluster 7: manter a Assemblage 441 e remover a Assemblage 436
# Cluster 8: manter a Assemblage 473, 459 e remover a Assemblage 460, 464, 465, 469, 470, 474
# Cluster 9: manter a Assemblage 482 e remover a Assemblage 480

### Removendo as assembleias ----

matriz_finalizada <- matriz_trat_2 %>%
  dplyr::filter(!Assemblage %in% c(8,
                                   78,
                                   226,
                                   234,
                                   235,
                                   324,
                                   330,
                                   331,
                                   337,
                                   340,
                                   349,
                                   410,
                                   411,
                                   436,
                                   460,
                                   464,
                                   465,
                                   469,
                                   470,
                                   474,
                                   480))

matriz_finalizada

##espécies -----

matriz_finalizada %>%
  dplyr::select(6:105) %>%
  names()

### Exportando ----

matriz_finalizada %>%
  openxlsx::write.xlsx("registros_finais.xlsx")
