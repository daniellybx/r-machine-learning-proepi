# ==============================================================================
# AULA 8 - MACHINE LEARNING NAO SUPERVISIONADO - BRASIL
# Curso: Analise de Dados e Machine Learning com R - ProEpi 2026
# ------------------------------------------------------------------------------
# Objetivo geral da aula:
# - Aplicar clusterizacao hierarquica e K-Means em dados de dengue (SINAN).
# - Aplicar K-Prototypes em dados mistos de nascidos vivos (SINASC).
# - Avaliar rigorosamente qualidade dos agrupamentos e utilidade pratica.
# ==============================================================================

# ------------------------------------------------------------------------------
# SESSAO 1 - PREPARACAO DO AMBIENTE E DIRETORIO DE RESULTADOS
# Objetivo da sessao:
# - Carregar pacotes necessarios para clusterizacao e avaliacao.
# - Criar pasta de saida para salvar summaries (.md) e figuras (ex: dendrogramas).
# - Definir funcao auxiliar para exportacao padronizada de graficos.
# ------------------------------------------------------------------------------
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("cluster", quietly = TRUE)) install.packages("cluster")
if (!requireNamespace("factoextra", quietly = TRUE)) install.packages("factoextra")
if (!requireNamespace("clustMixType", quietly = TRUE)) install.packages("clustMixType")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")

library(dplyr)        # Manipulacao de dados e sumarizacoes por cluster.
library(lubridate)    # Extracao de componentes temporais para engenharia de features.
library(ggplot2)      # Graficos didaticos para avaliacao de clusterizacao.
library(cluster)      # Distancias, silhouette e metricas de coesao/separacao.
library(factoextra)   # Visualizacoes de cluster, dendrograma e escolha do K.
library(clustMixType) # K-Prototypes para dados mistos (numericos + categoricos).
library(tidyr)        # Transformacao longa para perfis de cluster.

# Cria diretorio de saida dos resultados da aula.
pasta_saida_modelos <- file.path("modelos_machine_learning", "08_ml_nao_supervisionado")
dir.create(pasta_saida_modelos, recursive = TRUE, showWarnings = FALSE)

# Funcao auxiliar para salvar graficos com padrao unico.
salvar_grafico <- function(grafico, nome_arquivo, largura = 10, altura = 6) {
  ggplot2::ggsave(
    filename = file.path(pasta_saida_modelos, nome_arquivo),
    plot = grafico,
    width = largura,
    height = altura,
    dpi = 300
  )
}

# ------------------------------------------------------------------------------
# SESSAO 2 - MODELOS 1 E 2: HIERARQUICO E K-MEANS (SINAN DENGUE)
# Objetivo da sessao:
# - Preparar variaveis numericas de data e idade para clusterizacao.
# - Ajustar Hierarquico Aglomerativo e K-Means com escolha de K por silhueta.
# - Avaliar qualidade por variancia explicada, silhueta e concordancia entre modelos.
#
# ABORDAGENS DE CLUSTERIZACAO UTILIZADAS:
# -> K-Means: 
#    * Ideal para datasets grandes (alta velocidade e baixo uso de memoria).
#    * Exige a definicao previa do numero de clusters (k).
#    * Melhor aplicado quando se espera clusters esfericos/similares em tamanho.
#
# -> Aglomerativo Hierarquico:
#    * Ideal para datasets pequenos a medios (alto custo computacional).
#    * Nao exige a definicao de 'k' a priori (definido via corte no dendrograma).
#    * Excelente para visualizar a taxonomia/hierarquia dos dados e lidar 
#      com formatos de clusters irregulares.
# ------------------------------------------------------------------------------

# Importa base tratada do SINAN.
raiz_dados_processados <- "dados_processados_datasus"
load(file.path(raiz_dados_processados, "sinan_modelagem.RData"))

dim(df_sinan_modelagem)
dplyr::glimpse(df_sinan_modelagem)

# Seleciona variaveis solicitadas e cria metrica numerica de data.
# Aqui usamos "dia do ano" para representar sazonalidade de notificacao.
dados_sinan_cluster <- df_sinan_modelagem %>%
  dplyr::select(DT_NOTIFIC, IDADEanos) %>%
  mutate(
    DT_NOTIFIC = as.Date(DT_NOTIFIC),
    DIA_DO_ANO = yday(DT_NOTIFIC)
  ) %>%
  dplyr::select(DIA_DO_ANO, IDADEanos) %>%
  tidyr::drop_na()

# Observacao didatica:
# A base do SINAN e grande; para Hierarquico e Silhueta (custosos em memoria),
# usamos amostra reprodutivel para viabilidade computacional sem perder didatica.
set.seed(2026)
tamanho_amostra_sinan <- min(5000, nrow(dados_sinan_cluster))
dados_sinan_amostra <- dados_sinan_cluster %>%
  dplyr::slice_sample(n = tamanho_amostra_sinan)

# Padronizacao com scale() para que variaveis em escalas diferentes
# (idade e dia do ano) tenham peso comparavel no calculo de distancia.
dados_sinan_scaled <- scale(dados_sinan_amostra)

# Modelo Hierarquico Aglomerativo (Ward.D2).
matriz_distancias_sinan <- dist(dados_sinan_scaled, method = "euclidean")
modelo_hclust <- hclust(matriz_distancias_sinan, method = "ward.D2")

# Dendrograma (visualizacao didatica do processo de fusao dos grupos).
grafico_dendrograma_hclust <- factoextra::fviz_dend(
  x = modelo_hclust,
  k = 4,              # valor didatico para destacar blocos visuais iniciais.
  rect = TRUE,
  show_labels = FALSE,
  cex = 0.5,
  main = "SINAN - Dendrograma Hierarquico (Ward.D2)"
)
print(grafico_dendrograma_hclust)
salvar_grafico(grafico_dendrograma_hclust, "01_dendrograma_hclust_sinan.png", largura = 11, altura = 7)

# Escolha de K pelo metodo da Silhueta com fviz_nbclust.
set.seed(2026)
grafico_k_silhueta <- factoextra::fviz_nbclust(
  x = as.data.frame(dados_sinan_scaled),
  FUNcluster = kmeans,
  method = "silhouette",
  k.max = 8,
  nstart = 25
) +
  ggplot2::labs(
    title = "SINAN - Escolha de K pelo metodo da Silhueta (K-Means)"
  )
print(grafico_k_silhueta)
salvar_grafico(grafico_k_silhueta, "02_escolha_k_silhueta_sinan.png")

# Extrai K ideal diretamente do objeto de plot (quando disponivel).
# Fallback: se nao houver estrutura esperada, usa K = 3 como default didatico.
k_otimo <- 3
if (!is.null(grafico_k_silhueta$data) &&
    all(c("clusters", "y") %in% names(grafico_k_silhueta$data))) {
  k_otimo <- as.integer(grafico_k_silhueta$data$clusters[which.max(grafico_k_silhueta$data$y)])
}
cat("\nK ideal (criterio silhueta):", k_otimo, "\n")

# Ajusta K-Means com K escolhido.
set.seed(2026)
modelo_kmeans <- kmeans(
  x = dados_sinan_scaled,
  centers = k_otimo,
  nstart = 50,
  iter.max = 200
)

# Avaliacao rigorosa 1: variancia explicada (BSS/TSS).
# Calcula a porcentagem da dispersao total dos dados (TSS) que 
# foi "explicada" pela divisao dos grupos (BSS). Em termos práticos, mede a qualidade 
# do agrupamento: um valor mais proximo de 1 (100%) indica que os clusters estao 
# muito bem separados e definidos uns dos outros.
percentual_variancia_explicada <- modelo_kmeans$betweenss / modelo_kmeans$totss
cat("\nSINAN - Variancia explicada (BSS/TSS):", round(percentual_variancia_explicada, 4), "\n")
cat("Interpretacao: quanto maior o BSS/TSS, maior a separacao global entre clusters.\n")

# Avaliacao rigorosa 2: Silhouette do K-Means.
# Avalia a qualidade do agrupamento verificando se cada ponto 
# foi colocado no cluster "certo". Ele mede o quão próximo um ponto está dos 
# outros do seu próprio grupo (coesão) em comparação com o grupo vizinho mais 
# próximo (separação). A pontuação vai de -1 a 1:
# -> Próximo a 1: O ponto está muito bem agrupado.
# -> Próximo a 0: O ponto está na fronteira entre dois clusters (sobreposição).
# -> Valores negativos: O ponto provavelmente foi colocado no cluster errado.
obj_silhueta_kmeans <- cluster::silhouette(modelo_kmeans$cluster, matriz_distancias_sinan)
grafico_silhueta_kmeans <- factoextra::fviz_silhouette(obj_silhueta_kmeans) +
  ggplot2::labs(
    title = "SINAN - Analise de Silhueta (K-Means)"
  )
print(grafico_silhueta_kmeans)
salvar_grafico(grafico_silhueta_kmeans, "03_silhueta_kmeans_sinan.png")

largura_media_silhueta <- mean(obj_silhueta_kmeans[, "sil_width"], na.rm = TRUE)
cat("\nSINAN - Largura media da silhueta:", round(largura_media_silhueta, 4), "\n")
cat("Interpretacao: valores proximos de 1 indicam melhor coesao/separacao; proximos de 0 indicam sobreposicao.\n")

# Avaliacao rigorosa 3: comparacao Hierarquico vs K-Means.
# Cria uma tabela de contingencia cruzando os resultados dos 
# dois metodos de agrupamento. O objetivo e verificar a "estabilidade" ou robustez
# dos clusters. Se os dois algoritmos agruparam os mesmos registros nos mesmos 
# grupos (concentrando os valores em poucas celulas especificas da tabela), 
# isso indica que os agrupamentos encontrados sao consistentes e tem forte presenca
# nos dados, nao sendo apenas um artefato de um algoritmo especifico. Uma tabela 
# muito "espalhada" indica que os modelos discordam sobre como agrupar os dados.

clusters_hclust <- cutree(modelo_hclust, k = k_otimo)
clusters_kmeans <- modelo_kmeans$cluster
tabela_concordancia <- table(Hclust = clusters_hclust, Kmeans = clusters_kmeans)
print(tabela_concordancia)

# Visualizacao dos clusters K-Means na amostra padronizada.
grafico_clusters_kmeans <- factoextra::fviz_cluster(
  object = modelo_kmeans,
  data = as.data.frame(dados_sinan_scaled),
  geom = "point",
  ellipse.type = "norm",
  ggtheme = ggplot2::theme_minimal()
) +
  ggplot2::labs(
    title = "SINAN - Visualizacao dos clusters K-Means (amostra padronizada)"
  )
print(grafico_clusters_kmeans)
salvar_grafico(grafico_clusters_kmeans, "04_visualizacao_clusters_kmeans_sinan.png")

# ------------------------------------------------------------------------------
# SESSAO 3 - MODELO 3: K-PROTOTYPES PARA DADOS MISTOS (SINASC)
# Objetivo da sessao:
# - Preparar dados mistos (numericos + categoricos) para K-Prototypes.
# - Ajustar K-Prototypes com K empirico e avaliar custo de agrupamento.
# - Construir perfis dos clusters para interpretacao clinico-administrativa.
# ------------------------------------------------------------------------------

# Importa base tratada do SINASC.
load(file.path(raiz_dados_processados, "sinasc_modelagem.RData"))

dim(df_sinasc_modelagem)
dplyr::glimpse(df_sinasc_modelagem)

# Seleciona variaveis solicitadas e exclui explicitamente o ID CODMUNNASC.
dados_sinasc_cluster <- df_sinasc_modelagem %>%
  dplyr::select(
    IDADEMAE, PARTO, ESCMAE, QTDFILVIVO, CONSULTAS,
    RACACORMAE, ESTCIVMAE, APGAR1, APGAR5
  ) %>%
  tidyr::drop_na()

# Garante tipagem correta para dados mistos.
dados_sinasc_cluster <- dados_sinasc_cluster %>%
  mutate(
    IDADEMAE = as.numeric(IDADEMAE),
    QTDFILVIVO = as.numeric(QTDFILVIVO),
    PARTO = as.factor(PARTO),
    ESCMAE = as.factor(ESCMAE),
    CONSULTAS = as.factor(CONSULTAS),
    RACACORMAE = as.factor(RACACORMAE),
    ESTCIVMAE = as.factor(ESTCIVMAE),
    APGAR1 = as.factor(APGAR1),
    APGAR5 = as.factor(APGAR5)
  )

dim(dados_sinasc_cluster)
dplyr::glimpse(dados_sinasc_cluster)

# K-Prototypes com K empirico para aula (K = 3).
set.seed(2026)
k_kproto <- 3
modelo_kproto <- clustMixType::kproto(
  x = dados_sinasc_cluster,
  k = k_kproto
)

# Funcao de custo (objetivo do algoritmo).
# Extrai a medida de erro ou "custo" total do modelo K-Prototypes. 
# Como esse algoritmo agrupa dados mistos (numéricos e categóricos), o custo 
# representa a soma total da heterogeneidade (distâncias e diferenças) dentro 
# de todos os clusters. O objetivo matemático do algoritmo é minimizar esse valor: 
# quanto menor o custo final, mais coesos e semelhantes entre si são os registros 
# que ficaram dentro do mesmo grupo.
custo_kproto <- if (!is.null(modelo_kproto$tot.withinss)) {
  modelo_kproto$tot.withinss
} else if (!is.null(modelo_kproto$tot.within)) {
  modelo_kproto$tot.within
} else {
  NA_real_
}
cat("\nSINASC - Funcao de custo do K-Prototypes:", custo_kproto, "\n")

# Metrica complementar de facil interpretacao (Silhueta).
# Como o valor do custo acima e um numero absoluto dificil de avaliar isoladamente,
# calculamos a Silhueta usando a Distancia de Gower (ideal para dados mistos).
# Ela traduz a qualidade do agrupamento em uma escala de -1 a 1:
# Valores proximos de 1 indicam boa separacao; proximos de 0 indicam sobreposicao.
distancias_gower_sinasc <- cluster::daisy(dados_sinasc_cluster, metric = "gower")
obj_silhueta_kproto <- cluster::silhouette(modelo_kproto$cluster, distancias_gower_sinasc)
largura_media_silhueta_kproto <- mean(obj_silhueta_kproto[, "sil_width"], na.rm = TRUE)
cat("SINASC - Silhueta Media (K-Prototypes / Gower):", round(largura_media_silhueta_kproto, 4), "\n")

# Anexa clusters para perfilamento.
dados_sinasc_cluster <- dados_sinasc_cluster %>%
  mutate(cluster_kproto = factor(modelo_kproto$cluster))

# Perfil 1: tamanho dos clusters.
tabela_tamanho_clusters <- dados_sinasc_cluster %>%
  count(cluster_kproto, name = "n")
print(tabela_tamanho_clusters)

grafico_tamanho_clusters <- ggplot(tabela_tamanho_clusters, aes(x = cluster_kproto, y = n, fill = cluster_kproto)) +
  geom_col(alpha = 0.85) +
  theme_minimal(base_size = 11) +
  labs(
    title = "SINASC - Tamanho dos clusters (K-Prototypes)",
    x = "Cluster",
    y = "Numero de registros",
    fill = "Cluster"
  )
print(grafico_tamanho_clusters)
salvar_grafico(grafico_tamanho_clusters, "05_tamanho_clusters_kproto_sinasc.png")

# Perfil 2: variaveis numericas por cluster.
tabela_perfil_numerico <- dados_sinasc_cluster %>%
  group_by(cluster_kproto) %>%
  summarise(
    media_idademae = mean(IDADEMAE, na.rm = TRUE),
    media_qtdfilvivo = mean(QTDFILVIVO, na.rm = TRUE),
    .groups = "drop"
  )
print(tabela_perfil_numerico)

grafico_perfil_numerico <- dados_sinasc_cluster %>%
  pivot_longer(cols = c(IDADEMAE, QTDFILVIVO), names_to = "variavel", values_to = "valor") %>%
  ggplot(aes(x = cluster_kproto, y = valor, fill = cluster_kproto)) +
  geom_boxplot(alpha = 0.75) +
  facet_wrap(~variavel, scales = "free_y") +
  theme_minimal(base_size = 11) +
  labs(
    title = "SINASC - Perfil numerico por cluster",
    x = "Cluster",
    y = "Valor",
    fill = "Cluster"
  )
print(grafico_perfil_numerico)
salvar_grafico(grafico_perfil_numerico, "06_perfil_numerico_clusters_kproto.png")

# Perfil 3: variaveis categoricas por cluster.
variaveis_categoricas_sinasc <- c("PARTO", "ESCMAE", "CONSULTAS", "RACACORMAE", "ESTCIVMAE", "APGAR1", "APGAR5")

tabela_perfil_categorico <- dados_sinasc_cluster %>%
  # Padroniza para character para evitar conflito entre factor e ordered em pivot_longer.
  mutate(across(all_of(variaveis_categoricas_sinasc), as.character)) %>%
  pivot_longer(cols = all_of(variaveis_categoricas_sinasc), names_to = "variavel", values_to = "categoria") %>%
  group_by(cluster_kproto, variavel, categoria) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percentual = 100 * n / sum(n)) %>%
  ungroup()
print(tabela_perfil_categorico)

grafico_perfil_categorico <- tabela_perfil_categorico %>%
  ggplot(aes(x = cluster_kproto, y = percentual, fill = categoria)) +
  geom_col(position = "stack") +
  facet_wrap(~variavel, scales = "free_y") +
  theme_minimal(base_size = 10) +
  labs(
    title = "SINASC - Perfil categorico por cluster (proporcoes)",
    x = "Cluster",
    y = "Percentual (%)",
    fill = "Categoria"
  )
print(grafico_perfil_categorico)
salvar_grafico(grafico_perfil_categorico, "07_perfil_categorico_clusters_kproto.png", largura = 12, altura = 8)

# Interpretacao clinico/negocio (didatica):
# - Cluster com menor media de IDADEMAE tende a representar maes mais jovens.
# - Se esse cluster tambem concentrar ESCMAE mais baixa e menos CONSULTAS,
#   pode indicar maior vulnerabilidade social e necessidade de cuidado focal.
# - Cluster com maiores APGAR1/APGAR5 e maior numero de consultas pode sugerir
#   melhor acompanhamento pre-natal e melhores desfechos neonatais iniciais.
# - A validacao final em modelos nao supervisionados depende da utilidade pratica:
#   os agrupamentos precisam gerar perfis acionaveis para vigilancia e gestao.

# ------------------------------------------------------------------------------
# SESSAO 4 - RELATORIO FINAL E SALVAMENTO DOS SUMMARIES
# Objetivo da sessao:
# - Consolidar metodos e metricas de avaliacao em markdown.
# - Facilitar reproducao didatica com rastreabilidade dos resultados.
# - Salvar apenas summaries em .md e graficos em imagem (sem .rds).
# ------------------------------------------------------------------------------
sumario_hclust_md <- capture.output(summary(modelo_hclust))
sumario_kmeans_md <- capture.output(modelo_kmeans)
sumario_kproto_md <- capture.output(modelo_kproto)

conteudo_md <- c(
  "# Aula 8 - Summary dos Modelos Nao Supervisionados",
  "",
  "## SINAN - K-Means e Hierarquico",
  "",
  paste0("- K ideal (silhueta): ", k_otimo),
  paste0("- BSS/TSS K-Means: ", round(percentual_variancia_explicada, 4)),
  paste0("- Largura media da silhueta: ", round(largura_media_silhueta, 4)),
  "",
  "### Tabela de concordancia (Hclust vs K-Means)",
  "```r",
  capture.output(tabela_concordancia),
  "```",
  "",
  "### Summary hclust",
  "```r",
  sumario_hclust_md,
  "```",
  "",
  "### Objeto K-Means",
  "```r",
  sumario_kmeans_md,
  "```",
  "",
  "## SINASC - K-Prototypes",
  "",
  paste0("- K fixado para aula: ", k_kproto),
  paste0("- Custo final do K-Prototypes: ", custo_kproto),
  "",
  "### Perfil numerico por cluster",
  "```r",
  capture.output(tabela_perfil_numerico),
  "```",
  "",
  "### Summary K-Prototypes",
  "```r",
  sumario_kproto_md,
  "```"
)
writeLines(conteudo_md, file.path(pasta_saida_modelos, "summary_modelos_nao_supervisionados.md"))