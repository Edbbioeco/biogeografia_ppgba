# Pacotes ----

library(usethis)

library(gert)

# Inicializando ----

usethis::use_git()

usethis::use_github_links()

# Dados ----

usethis::use_git_config(user.name = "Edbbioeco",
                        user.email = "edsonbbiologia@gmail.com")

# Configurando token ----

usethis::create_github_token()

# Setando o repositório ----

usethis::use_git_remote(name = "origin",
                        url = "https://github.com/Edbbioeco/biogeografia_ppgba",
                        overwrite = TRUE)

# Sincronizando os arquivos ----

usethis::git_default_branch_rename(from = "master", to = "main")

# Commit ----

gert::git_add("configurar_github.R")

gert::git_commit("Commit para alterar o README")

gert::git_push()
