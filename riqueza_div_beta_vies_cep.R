# Pacotes ----

library(readxl)

library(tidyverse)

library(CoordinateCleaner)

library(geobr)

library(sf)

library(writexl)

library(terra)

library(tidyterra)

library(betapart)

library(sampbias)

# Dados ----

## Registros ----

### Importando ----

registros <- readxl::read_xlsx("registros.xlsx")

### Visualizando ----

registros

registros |> dplyr::glimpse()

## CEP ----

### Importando ----

### Visualizando ----

## Unidades de conservação ----

### Importando ----

### Visualizando ----

# Riqueza ----

# Diversidade beta ----

# Vi[es de amostragem ----
