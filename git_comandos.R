# Pacote ----

library(gert)

# Selecionando os arquivos ----

gert::git_status() |>
  as.data.frame() |>
  dplyr::filter(file |> stringr::str_detect(".R$"))

# Selecionando o arquivo ----

gert::git_add(list.files(pattern = c("settando_github.R"))) |> as.data.frame()

# Commitando ----

gert::git_commit("Script para settar repositório do Github")

# Pushando ----

gert::git_push(remote = "origin", force = TRUE)

# Pullando ----

gert::git_pull(remote = "origin")

# Resetando ----

gert::git_reset_mixed()

gert::git_reset_soft("HEAD^")

# Removendo arquivos ----

## Selecionando os arquivos para remover ----

gert::git_rm(list.files())

## Commitando ----

## Pushando ----

## Pullando ----
