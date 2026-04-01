# Pacote ----

library(gert)

# Selecionando os arquivos ----

gert::git_status() |>
  as.data.frame() |>
  dplyr::filter(file |> stringr::str_detect(".R$"))

# Selecionando o arquivo ----

gert::git_add(list.files(pattern = c("pratica_interpolação_extrapolacao.R"))) |>
  as.data.frame()

# Commitando ----

gert::git_commit("Script para a atividade de modelagfem de nicho ecológico")

# Pushando ----

gert::git_push(remote = "origin", force = TRUE)

# Pullando ----

gert::git_pull(remote = "origin")

# Resetando ----

gert::git_reset_mixed() |>
  as.data.frame()

gert::git_reset_soft("HEAD^")

# Removendo arquivos ----

## Selecionando os arquivos para remover ----

gert::git_rm(list.files(pattern = ".tif$|.png$|.jpeg$|.shp$|.dbf$|.shx$|.prj$|.txt$|DRIVERS|.csv$")) |>
  as.data.frame()

## Commitando ----

gert::git_commit("Remover")

## Pushando ----

gert::git_push(remote = "origin", force = TRUE)

## Pullando ----

gert::git_pull(remote = "origin")
