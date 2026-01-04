# Pacote ----

library(gert)

# Selecionando o arquivo ----

gert::git_add("configurar_github.R")

# Commitando ----

gert::git_commit("Script de comandos de git")

# Pushando ----

gert::git_push()

# Pullando ----

gert::git_pull()

# Mergendo ----

gert::git_merge()
