# Pacote ----

library(gert)

# Selecionando o arquivo ----

gert::git_add("github_comandos.R")

# Commitando ----

gert::git_commit("Script de comandos de git")

# Pushando ----

gert::git_push(remote = "origin", force = TRUE)

# Pullando ----

gert::git_pull()

# Mergendo ----

gert::git_merge()
