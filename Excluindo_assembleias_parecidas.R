# Pacotes ----

library(sf)

library(tidyverse)

library(readxl)

library(geobr)

library(ggsflabel)

library(fields)

library(dbscan)

library(crayon)

# Dados ----

## Grade ----

### Importando ----

grade_cep <- sf::st_read("grade_cep.shp")

### Visualizando ----

grade_cep

grade_cep |>
  ggplot() +
  geom_sf(color = "black", fill = "green4")

## Registros ----

### Importando ----

matriz_trat <- readxl::read_xlsx("matriz_trat.xlsx")

### Transformando em shapefile ----

matriz_shp <- matriz_trat |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = grade_cep |> sf::st_crs())

### Visualizando ----

matriz_trat

matriz_shp

ggplot() +
  geom_sf(data = grade_cep, color = "black", fill = "green4") +
  geom_sf(data = matriz_shp)

### Excluindo as assembleias com menos de 5 espécies ----

assembleias <- matriz_trat |>
  tidyr::pivot_longer(cols = !c(Assemblage, Latitude:Source),
                      names_to = "Species",
                      values_to = "Presence") |>
  dplyr::filter(Presence == 1) |>
  dplyr::summarise(`Número de Espécies` = Species |> dplyr::n_distinct(),
                   .by = Assemblage) |>
  dplyr::arrange(`Número de Espécies`) |>
  dplyr::filter(`Número de Espécies` < 5) |>
  dplyr::pull(Assemblage)

assembleias

matriz_shp_trat <- matriz_shp |>
  dplyr::filter(!Assemblage %in% assembleias)

ggplot() +
  geom_sf(data = grade_cep, color = "black", fill = "green4") +
  ggsflabel::geom_sf_label_repel(data = matriz_shp_trat, aes(label = Assemblage))

matriz_trat <- matriz_trat |>
  dplyr::filter(!Assemblage %in% assembleias)

matriz_trat

## Exportando ----

matriz_trat |>
  writexl::write_xlsx("registros_finais.xlsx")

# Clusterizando por distância < 20km ----

## Matriz de distância geográfica ----

dist_matrix <- matriz_trat |>
  dplyr::select(Longitude:Latitude) |>
  as.data.frame() |>
  fields::rdist.earth(miles = FALSE) |>
  as.dist()

dist_matrix

## Calculando os clusters ----

dbscan_result <- dbscan::dbscan(dist_matrix,
                                eps = 20,
                                minPts = 2)

dbscan_result

## Criar um data frame de identificação dos clusters ----

clusters <- tibble::tibble(Assemblage = matriz_trat$Assemblage,
                           Cluster = dplyr::if_else(dbscan_result$cluster == 0,
                                                    "Sem Cluster",
                                                    paste0("Cluster ",
                                                           dbscan_result$cluster)))

clusters

## Adicionando a coluna de Cluster à matriz original ----

matriz_trat_2 <- matriz_trat |>
  dplyr::left_join(clusters,
                   by = "Assemblage") |>
  dplyr::relocate(Cluster, .after = Source)

matriz_trat_2

## Checando cada Cluster ----

### Criando uma função ----

checando_Clusters <- function(x){

  message(stringr::str_glue("# Avaliação para o {x}")) |>
    crayon::green()

  message("## Matriz")

  matriz_trat_2 |>
    dplyr::filter(Cluster == x) |>
    dplyr::select(1, !c(Latitude:Source, Cluster)) |>
    dplyr::select(dplyr::where(~ any(. != 0))) |>
    as.data.frame() |>
    print()

  message("## Número de espécies")

  matriz_trat_2 |>
    dplyr::filter(Cluster == x) |>
    dplyr::select(1, !c(Latitude:Source, Cluster)) |>
    dplyr::select(dplyr::where(~ any(. != 0))) |>
    dplyr::rename("rowname" = Assemblage) |>
    tibble::column_to_rownames() |>
    as.data.frame() |>
    dplyr::select_if(is.numeric) |>
    vegan::specnumber() |>
    print()

  message("## Dissimilaridade")

  matriz_trat_2 |>
    dplyr::filter(Cluster == x) |>
    dplyr::select(1, !c(Latitude:Source)) |>
    dplyr::select(dplyr::where(~ any(. != 0))) |>
    dplyr::rename("rowname" = Assemblage) |>
    tibble::column_to_rownames() |>
    as.data.frame() |>
    dplyr::select_if(is.numeric) |>
    vegan::vegdist(method = "jaccard") |>
    print()

}

### Checando ----

purrr::walk(matriz_trat_2 |>
              dplyr::filter(Cluster != "Sem Cluster") |>
              dplyr::pull(Cluster) |>
              unique(),
            checando_Clusters)

### Avaliação ----

# Cluster 1: manter a Assemblage 1 e remover Assemblage 2
# Cluster 2: manter a Assemblage 5 e remover Assemblage 6
# Cluster 3: manter a Assemblage 128 e remover a Assemblage 129
# Cluster 4: manter a Assemblage 145 e remover as Assemblage 144 e 146
# Cluster 5: manter a Assemblage 230 e remover a Assemblage 229, 349
# Cluster 6: manter a Assemblage 234 e remover a Assemblage 233
# Cluster 7: manter a Assemblage 316 e remover a Assemblage 315
# Cluster 8: manter a Assemblage 324 e remover a Assemblage 325
# Cluster 9: manter a Assemblage 333 e remover a Assemblage 480

### Removendo as assembleias ----

matriz_finalizada <- matriz_trat_2 |>
  dplyr::filter(!Assemblage %in% c(2,
                                   6,
                                   129,
                                   144,
                                   146,
                                   229,
                                   233,
                                   315,
                                   325))

matriz_finalizada

##espécies -----

matriz_finalizada |>
  dplyr::select(6:105) |>
  names()

### Exportando ----

matriz_finalizada |>
  openxlsx::write.xlsx("registros_finais_filtrados.xlsx")
