# Pacotes ----

library(readxl)

library(tidyverse)

library(performance)

library(reshape2)

library(sjPlot)

# Dados ----

## Riqueza ----

### Importando ----

riqueza <- readxl::read_xlsx("valores_riqueza.xlsx") |>
  dplyr::rename()

### Visualizando ----

riqueza |> dplyr::glimpse()

riqueza

## Composição ----

### Importando ----

registros <- readxl::read_xlsx("registros_finais.xlsx")

### Visualizando ----### Visualizandolist() ----

registros |> dplyr::glimpse()

registros

# Modelos linear Riqueza ----

## Multicolinearidade ----

riqueza_cor <- riqueza |>
  dplyr::mutate(Altitude = Altitude |> as.numeric()) |>
  dplyr::select(2:6) |>
  cor(method = "spearman", use = "complete.obs")

riqueza_cor

riqueza_cor[upper.tri(riqueza_cor)] <- NA

riqueza_cor |>
  reshape2::melt() |>
  tidyr::drop_na() |>
  dplyr::mutate(igual = dplyr::case_when(Var1 == Var2 ~ "sim",
                                         .default = "Não"),
                value = value |> round(2)) |>
  dplyr::filter(igual == "Não") |>
  dplyr::select(-igual) |>
  ggplot(aes(Var1, Var2, fill = value, label = value)) +
  geom_tile(color = "black", linewidth = 1) +
  geom_text() +
  coord_equal() +
  scale_fill_gradientn(colours = c(viridis::viridis(n = 10) |> rev(), viridis::viridis(n = 10)),
                       limits = c(-1, 1),
                       breaks = seq(-1, 1, 0.2)) +
  labs(x = NULL,
       y = NULL) +
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = 0.5,
                                barwidth = 25,
                                barheight = 1.5,
                                frame.colour = "black",
                                ticks.colour = "black",
                                ticks.linewidth = 0.5)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position = "bottom")

## Modelo para altitude e precipitação ----

### Criando o modelo ----

modelo_glm_poisson_altprec <- glm(Riqueza ~ Altitude +
                                    Precipitação,
                                  data = riqueza,
                                  family = poisson(link = "log"))
### Avaliando o modelo ----

modelo_glm_poisson_altprec |> performance::check_model()

modelo_glm_poisson_altprec |> summary()

modelo_glm_poisson_altprec |> rsq::rsq(adj = TRUE)

### Gráfico ----

riqueza |>
  tidyr::pivot_longer(cols = c(Altitude:Precipitação),
                      names_to = "Preditor",
                      values_to = "Valor Preditor") |>
  dplyr::mutate(Preditor = Preditor |> forcats::fct_relevel(names(riqueza[, c(2:3)]))) |>
  ggplot(aes(`Valor Preditor`, Riqueza, color = Preditor, fill = Preditor)) +
  geom_point(size = 2.5, shape = 21, color = "black") +
  geom_smooth(method = "lm") +
  facet_wrap(~Preditor, scales = "free_x") +
  scale_fill_manual(values = c("gold", "cyan3")) +
  scale_color_manual(values = c("gold4", "cyan4")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        strip.text = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position = "none")

ggsave(filename = "modelo_riqueza_altprec.png", height = 10, width = 12)

### Sumário ----

sumario <- modelo_glm_poisson_altprec |> summary() |>
  .$coefficients |>
  tibble::as_tibble()

sumario

### Tabela das estátísticas ----

#### Criando ----

riqueza_tabela <- tibble::tibble(Preditor = names(riqueza[, c(2:3)]),
               `Coeficiente estimado ± Erro Padrão` = paste0(sumario$Estimate[-1] |> round(3),
                                                             " ± ",
                                                             sumario$`Std. Error`[-1] |> round(3)),
               `Graus de Liberdade` = 40,
               Z = sumario$`z value`[-1] |> round(3),
               p = sumario$`Pr(>|z|)`[-1] |> round(3)) |>
  dplyr::mutate(p = dplyr::case_when(p < 0.01 ~ "< 0.01",
                                     .default = p |> as.character()) ) |>
  flextable::flextable() |>
  flextable::width(width = 1.5) |>
  flextable::align(align = "center", part = "all") |>
  flextable::bold(part = "header")

riqueza_tabela

#### Exportando ----

riqueza_tabela |>
  flextable::save_as_docx(path = "tabela_riqueza_altprec.docx")

## Modelo para a paisagem ----

### Criando os modelos ----

modelo_glm_poisson_paisagem <- glm(Riqueza ~ `% de corpos d'água` +
                                    `% de vegetação nativa` +
                                    `Diversidade da paisagem`,
                                  data = riqueza,
                                  family = poisson(link = "log"))

### Avaliando o modelo ----

modelo_glm_poisson_paisagem |> performance::check_model()

modelo_glm_poisson_paisagem |> summary()

modelo_glm_poisson_paisagem |> rsq::rsq(adj = TRUE)

### Gráfico ----

riqueza |>
  tidyr::pivot_longer(cols = c(`% de corpos d'água`:`Diversidade da paisagem`),
                      names_to = "Preditor",
                      values_to = "Valor Preditor") |>
  dplyr::mutate(Preditor = Preditor |> forcats::fct_relevel(names(riqueza[, c(2, 4:8)]))) |>
  ggplot(aes(`Valor Preditor`, Riqueza, color = Preditor, fill = Preditor)) +
  geom_point(size = 2.5, shape = 21, color = "black") +
  geom_smooth(method = "lm") +
  facet_wrap(~Preditor, scales = "free_x") +
  scale_fill_manual(values = c("cyan4", "green2", "violet")) +
  scale_color_manual(values = "darkgreen") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        strip.text = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position = "none")

ggsave(filename = "modelo_riqueza_paisagem.png", height = 10, width = 12)

### Sumário ----

sumario <- modelo_glm_poisson_paisagem |>
  summary() |>
  .$coefficients |>
  tibble::as_tibble()

sumario

### Tabela das estátísticas ----

#### Criando ----

riqueza_tabela_paisagem <- tibble::tibble(Preditor = names(riqueza[, c(6:8)]),
                                 `Coeficiente estimado ± Erro Padrão` = paste0(sumario$Estimate[-1] |> round(3),
                                                                               " ± ",
                                                                               sumario$`Std. Error`[-1] |> round(3)),
                                 `Graus de Liberdade` = 40,
                                 Z = sumario$`z value`[-1] |> round(3),
                                 p = sumario$`Pr(>|z|)`[-1] |> round(3)) |>
  dplyr::mutate(p = dplyr::case_when(p < 0.01 ~ "< 0.01",
                                     .default = p |> as.character()) ) |>
  flextable::flextable() |>
  flextable::width(width = 1.5) |>
  flextable::align(align = "center", part = "all") |>
  flextable::bold(part = "header")

riqueza_tabela_paisagem

#### Exportando ----

riqueza_tabela |>
  flextable::save_as_docx(path = "tabela_riqueza_paisagen.docx")

# Modelos linear Composição ----

## Distância na composição ----

dist <- registros |>
  dplyr::filter(Assemblage != 341) |>
  dplyr::select(6:105) |>
  vegan::vegdist(method = "jaccard")

## Para altitude e precipitação ----

### Criando o modelo ----

dbrda_altprec <- vegan::capscale(dist ~ Altitude +
                                   Precipitação,
                                 data = riqueza,
                                 permutations = 1000)

### Avaliando ----

dbrda_altprec |>
  vegan::anova.cca(by = "term")

dbrda_altprec

vegan::RsquareAdj(dbrda_altprec)

ggord::ggord(dbrda_altprec, ptslab = TRUE, size = 1, addsize = 3, repel = TRUE)


### Gráfico ----

variaveis_altprec <- dbrda_altprec$CCA$biplot |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  dplyr::mutate(rowname = rowname |> stringr::str_remove_all("`"))

variaveis_altprec

coords_altprec <- dbrda_altprec$Ybar |>
  as.data.frame() |>
  dplyr::mutate(Assemblage = registros$Assemblage[registros$Assemblage != 341])

coords_altprec

ggplot() +
  geom_label(data = coords_altprec, aes(Dim1, Dim2, label = Assemblage),
             size = 5, color = "black", fill = "gold",
             label.size = 1,
             fontface = "bold") +
  geom_text(data = variaveis_altprec, aes(CAP1, CAP2, label = rowname,
                                          fontface = "bold"),
            color = "black", size = 5) +
  geom_segment(data = variaveis_altprec, aes(x = 0, y = 0,
                                             xend = CAP1, yend = CAP2),
               color = "black",
               linewidth = 1) +
  scale_x_continuous(limits = c(-0.6, 0.95)) +
  labs(x = "CAP 1 (74,91%)",
       y = "CAP2 (25,09%)") +
  theme_bw() +
  theme(text = element_text(face = "bold"),
        axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        strip.text = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position = "none",
        panel.border = element_rect(color = "black", linewidth = 1))

ggsave(filename = "modelo_composicao_altprec.png", height = 10, width = 12)

### Tabela de Estatísticas ----

#### Criando ----

sumario_rdaalt_prec <- dbrda_altprec |>
  vegan::anova.cca(by = "term")

composicao_tabela_altprec <- sumario_rdaalt_prec |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  dplyr::rename("Preditor" = rowname,
                "Graus de liberdade" = Df,
                "p" = `Pr(>F)`) |>
  dplyr::select(-3) |>
  dplyr::filter(Preditor != "Residual") |>
  dplyr::mutate(`Graus de liberdade` = "1, 38",
                Preditor = Preditor |> stringr::str_remove_all("`"),
                `F` = `F` |> round(3),
                p = dplyr::case_when(p < 0.01 ~ "< 0.01",
                                     .default = p |> as.character())) |>
  flextable::flextable() |>
  flextable::width(width = 1.5) |>
  flextable::align(align = "center", part = "all") |>
  flextable::bold(part = "header")

composicao_tabela_altprec

#### Exportando ----

composicao_tabela_altprec |>
  flextable::save_as_docx(path = "tabela_composicao_altprec.docx")

## Para paisagem ----

### Criando o modelo ----

dbrda_paisagem <- vegan::capscale(dist ~ `% de corpos d'água` +
                                   `% de vegetação nativa` +
                                   `Diversidade da paisagem`,
                                 data = riqueza,
                                 permutations = 1000)

### Avaliando ----

dbrda_paisagem

dbrda_paisagem |>
  vegan::anova.cca(by = "term")

vegan::RsquareAdj(dbrda_paisagem)

### Gráfico ----

variaveis_paisagem <- dbrda_paisagem$CCA$biplot |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  dplyr::mutate(rowname = rowname |> stringr::str_remove_all("`"))

variaveis_paisagem

coords_paisagem <- dbrda_paisagem$Ybar |>
  as.data.frame() |>
  dplyr::mutate(Assemblage = registros$Assemblage[-29])

coords_paisagem

ggord::ggord(dbrda_paisagem, ptslab = TRUE, size = 1, addsize = 3, repel = TRUE)

ggplot() +
  geom_label(data = coords_paisagem, aes(Dim1, Dim2, label = Assemblage),
             size = 5, color = "black", fill = "gold",
             label.size = 1,
             fontface = "bold") +
  geom_text(data = variaveis_paisagem, aes(CAP1, CAP2, label = rowname, fontface = "bold"), color = "black", size = 5)+
  geom_segment(data = variaveis_paisagem, aes(x = 0, y = 0, xend = CAP1, yend = CAP2), color = "black", linewidth = 1) +
  scale_x_continuous(limits = c(-0.725, 0.86)) +
  labs(x = "CAP 1 (46,76%)",
       y = "CAP2 (31,83%)") +
  theme_bw() +
  theme(text = element_text(face = "bold"),
        axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        strip.text = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position = "none",
        panel.border = element_rect(color = "black", linewidth = 1))

ggsave(filename = "modelo_composicao_paisagem.png", height = 10, width = 12)

### Tabela de Estatísticas ----

#### Criando ----

sumario_rdaalt_paisagem <- dbrda_paisagem |>
  vegan::anova.cca(by = "term")

composicao_tabela_paisagem <- sumario_rdaalt_paisagem |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  dplyr::rename("Preditor" = rowname,
                "Graus de liberdade" = Df,
                "p" = `Pr(>F)`) |>
  dplyr::select(-3) |>
  dplyr::filter(Preditor != "Residual") |>
  dplyr::mutate(`Graus de liberdade` = "1, 38",
                Preditor = Preditor |> stringr::str_remove_all("`"),
                `F` = `F` |> round(3),
                p = dplyr::case_when(p < 0.01 ~ "< 0.01",
                                     .default = p |> as.character())) |>
  flextable::flextable() |>
  flextable::width(width = 1.5) |>
  flextable::align(align = "center", part = "all") |>
  flextable::bold(part = "header")

composicao_tabela_paisagem

#### Exportando ----

composicao_tabela_altprec |>
  flextable::save_as_docx(path = "tabela_composicao_paisagem.docx")
