# Pacote ----

library(gert)

# Selecionando o arquivo ----

gert::git_add(list.files(pattern = "github")) |> as.data.frame()

# Commitando ----

gert::git_commit("Comandos para enviar arquivos ao github")

# Pushando ----

gert::git_push(remote = "origin", force = TRUE)

# Pullando ----

gert::git_pull()

# Mergendo ----

gert::git_merge()
