# Pacotes ----

library(readxl)

library(tidyverse)

library(sf)

library(writexl)

# Dados ----

## GBIF ----

### Importando ----

registros_gbif <- readxl::read_xlsx("registros_gbif.xlsx")

### Visualizando ----

registros_gbif |> dplyr::glimpse()

registros_gbif

## speciesLink ----

### Importando ----

registros_specieslink <- readxl::read_xlsx("registros_specieslink.xlsx")

### Visualizando ----

registros_specieslink |> dplyr::glimpse()

registros_specieslink

## Sibbr ----

### Importando ----

registros_sibbr <- readxl::read_xlsx("registros_sibbr.xlsx")

### Visualizando ----

registros_sibbr |> dplyr::glimpse()

registros_sibbr

## Bibliográficos ----

### Importando ----

registros_bib <- readxl::read_xlsx("registros_bib.xlsx")

### Visualizando ----

registros_bib |> dplyr::glimpse()

registros_bib

## Grade ----

### Importando ----

grade_cep <- sf::st_read("grade_cep.shp")

### Visualizando ----

grade_cep

grade_cep |>
  ggplot() +
  geom_sf(color = "black", fill = "green4")

# Dados unidos ----

## Unindo os dados de registro ----

registros <- dplyr::bind_rows(registros_gbif,
                              registros_specieslink,
                              registros_sibbr,
                              registros_bib)

## Visualizando ----

registros

## Checando as espécies ----

### Lista de espécies ----

registros$species |> unique()

# species == "Rhinella margaritifera" ~ "Rhinella hoogmoedi"
# species %in% c("Rhinella jimi", "Rhinella marina", "Rhinella schneideri") ~ "Rhinella diptycha"
# species %in% c("Dendropsophus werneri", "Dendropsophus rubicundulus") ~ "Dendropsophus branneri"
# species == "Dendropsophus leucophyllatus" ~ "Dendropsophus elegans"
# species %in%  c("Leptodactylus labyrinthicus", "Leptodactylus pentadactylus", "Leptodactylus pentadactylus labyrinthicus")  ~ "Leptodactylus vastus"
# species == "Hypsiboas raniceps" ~ "Boana raniceps"
# species == "Phyllomedusa nordestina" ~ "Pithecopus gonzagai"
# species == "Hypsiboas albomarginatus" ~ "Boana albomarginata"
# species %in% c("Colostethus alagoanus", "Allobates alagoanus") ~ "Allobates olfersioides"
# species == "Hyla raniceps" ~ "Boana raniceps"
# species == "Hypsiboas semilineatus" ~ "Boana semilineata"
# species == "Hypsiboas atlanticus" ~ "Boana atlantica"
# species == "Hypsiboas exastis" ~ "Boana exastis"
# species == "Hypsiboas freicanecae" ~ "Boana freicanecae"
# species == "Rana paradoxa" ~ "Lithobates palmipes"
# species == "Leptodactylus ocellatus" ~ "Leptodactylus macrosternum"
# species %in% c("Bufo granulosus granulosus", "Rhinella mirandaribeiroi") ~ "Rhinella granulosa"
# species == "Ischnocnema ramagii" ~ "Pristimantis ramagii"
# species %in% c("Ololygon v-signata", "Scinax similis", "Osteopilus ocellatus") ~ "Scinax x-signatus"
# species %in% c("Phyllomedusa hypocondrialis", "Phyllomedusa hypochondrialis") ~ "Pithecopus gonzagai"
# species %in% c("Hypsiboas crepitans", "Boana pardalis") ~ "Boana crepitans"
# species == "Chiasmocleis alagoanus" ~ "Chiasmocleis alagoana"
# species == "Scinax skuki" ~ "Ololygon skuki"
# species == "Hypsiboas faber" ~ "Boana faber"
# species == "Scinax muriciensis" ~ "Ololygon muriciensis"
# species == "Scinax agilis" ~ "Ololygon agilis"
# species == "Elachistocleis ovalis" ~ "Elachistocleis cesarii"
# species == "Elachistocleis ovalis" ~ "Elachistocleis cesarii"
# species == "Dendropsophus decioiens" ~ "Dendropsophus decipiens"
# species == "Leptodactylus marmoratus"" ~ "Adenomera hylaedactyla"

### Corrigindo a taxonomia ----

registros <- registros |>
  dplyr::mutate(species = dplyr::case_when(species == "Rhinella margaritifera" ~ "Rhinella hoogmoedi",
                                           species %in% c("Rhinella jimi", "Rhinella marina", "Rhinella schneideri") ~ "Rhinella diptycha",
                                           species %in% c("Dendropsophus werneri", "Dendropsophus rubicundulus") ~ "Dendropsophus branneri",
    species == "Dendropsophus leucophyllatus" ~ "Dendropsophus elegans",
    species %in% c("Leptodactylus labyrinthicus", "Leptodactylus pentadactylus", "Leptodactylus pentadactylus labyrinthicus") ~ "Leptodactylus vastus",
    species == "Hypsiboas raniceps" ~ "Boana raniceps",
    species == "Phyllomedusa nordestina" ~ "Pithecopus gonzagai",
    species == "Hypsiboas albomarginatus" ~ "Boana albomarginata",
    species %in% c("Colostethus alagoanus", "Allobates alagoanus") ~ "Allobates olfersioides",
    species == "Hyla raniceps" ~ "Boana raniceps",
    species == "Hypsiboas semilineatus" ~ "Boana semilineata",
    species == "Hypsiboas atlanticus" ~ "Boana atlantica",
    species == "Hypsiboas exastis" ~ "Boana exastis",
    species %in% c("Hypsiboas freicanecae", "Boana freicanecaee") ~ "Boana freicanecae",
    species == "Rana paradoxa" ~ "Lithobates palmipes",
    species == "Leptodactylus ocellatus" ~ "Leptodactylus macrosternum",
    species %in% c("Bufo granulosus granulosus", "Rhinella mirandaribeiroi") ~ "Rhinella granulosa",
    species == "Ischnocnema ramagii" ~ "Pristimantis ramagii",
    species %in% c("Ololygon v-signata", "Scinax similis", "Osteopilus ocellatus") ~ "Scinax x-signatus",
    species %in% c("Phyllomedusa hypocondrialis", "Phyllomedusa hypochondrialis") ~ "Pithecopus gonzagai",
    species %in% c("Hypsiboas crepitans", "Boana pardalis") ~ "Boana crepitans",
    species == "Chiasmocleis alagoanus" ~ "Chiasmocleis alagoana",
    species == "Scinax skuki" ~ "Ololygon skuki",
    species == "Hypsiboas faber" ~ "Boana faber",
    species == "Scinax muriciensis" ~ "Ololygon muriciensis",
    species == "Scinax agilis" ~ "Ololygon agilis",
    species == "Elachistocleis ovalis" ~ "Elachistocleis cesarii",
    species == "Dendropsophus decioiens" ~ "Dendropsophus decipiens",
    species == "Leptodactylus marmoratus" ~ "Adenomera hylaedactyla",
    species == "Agalychnis granulosa" ~ "Hylomantis granulosa",
    species %in% c("Adelophrnne nordestina", "Adelophrynne nordestina") ~ "Adelophrynne nordestina",
    species == "Vitreorana balionma" ~ "Vitreorana baliomma",
    .default = species
  )) |>
  dplyr::filter(!species %in% c("Breviceps gibbosus", "Vitreorana baliomma"))

registros

## Criando uma matriz de composição ----

### Matriz com todas as comunidades ----

matriz <- registros |>
  dplyr::rename("Assemblage" = FID) |>
  dplyr::group_by(Assemblage, species) |>
  dplyr::summarise(presence = max(presence, na.rm = TRUE),
                   .groups = "drop") |>
  tidyr::pivot_wider(names_from = species,
                     values_from = presence,
                     values_fill = 0) |>
  dplyr::left_join(registros |>
                     dplyr::rename("Assemblage" = FID) |>
                     dplyr::select(1, 4:6),
                   by = "Assemblage") |>
  dplyr::relocate(Latitude:Source,
                  .before = `Dermatonotus muelleri`) |>
  dplyr::distinct(Assemblage, .keep_all = TRUE)

matriz

matriz |> dplyr::glimpse()

ggplot() +
  geom_sf(data = grade_cep, color = "black", fill = "green4") +
  geom_point(data = matriz, aes(Longitude, Latitude))

### Removendo as comunidades com menos de 5 espécies ----

comunidades <- matriz |>
  tibble::column_to_rownames(var = "Assemblage") |>
  dplyr::select(!Latitude:Source) |>
  vegan::specnumber() |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  dplyr::rename("Riqueza" = 2) |>
  dplyr::filter(Riqueza >= 5) |>
  dplyr::pull(rowname)

comunidades

matriz_trat <- matriz |>
  dplyr::filter(Assemblage %in% comunidades) |>
  tibble::as_tibble()

matriz_trat

matriz_trat |> dplyr::glimpse()

ggplot() +
  geom_sf(data = grade_cep, color = "black", fill = "green4") +
  geom_point(data = matriz_trat, aes(Longitude, Latitude))

### Removendo possíveies espécies sem registro ----

especies_retirar <- matriz_trat |>
  tidyr::pivot_longer(cols = !c(Assemblage, Latitude:Source),
                      names_to = "Espécie",
                      values_to = "Presença") |>
  dplyr::summarise(Abundancia = Presença |> sum(),
                   .by = Espécie) |>
  dplyr::filter(Abundancia == 0) |>
  dplyr::pull(Espécie)

especies_retirar

matriz_trat <- matriz_trat |>
  dplyr::select(-especies_retirar)

matriz_trat

# Exportando ----

## Registros ----

registros |>
  dplyr::rename("Assemblage" = FID) |>
  openxlsx::write.xlsx("registros.xlsx")

## Matriz ----

matriz |>
  writexl::write_xlsx("matriz.xlsx")

matriz_trat |>
  writexl::write_xlsx("matriz_trat.xlsx")
