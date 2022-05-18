setwd('C:/Users/Danilo/OneDrive/FIA/Pós Data Science/Aulas/Analytics/05 - mar22/20220316')
getwd()
######################################################
####### CASE: LIVRARIA
######################################################

# Alterar diretorio

setwd("...")

# Pacotes necessarios

install.packages("readxl")
install.packages("partykit")
install.packages("HH")
install.packages("Information")
install.packages("InformationValue")
install.packages("dplyr")
install.packages("gtools")
install.packages("CHAID", repos = "http://R-Forge.R-project.org", type = "source")
install.packages("cluster")
install.packages("factoextra")
install.packages("gridExtra")

library(readxl)
library(dplyr)
library(HH)
library(Information)
library(InformationValue)
library(gtools)
library(partykit)
library(CHAID)
library(cluster)    
library(factoextra) 
library(gridExtra)

# Leitura das bases de dados

livraria <- read_excel("Livraria.xlsx", sheet = "Base 1")
livraria_cluster <- read_excel("Livraria.xlsx", sheet = "Base 2")


######################################################
####### ANALISE EXPLORATORIA
######################################################

# Variaveis quantitativas

summary(livraria$qtde_paginas)
summary(livraria$preco)

par(mfrow = c(1,2))
hist(livraria$qtde_paginas, col = "darkturquoise", main = "Qtde. de paginas")
hist(livraria$preco, col = "darkturquoise", main = "Preço")

# Variaveis categoricas

table(livraria$edicao)
round(prop.table(table(livraria$edicao)), 2)

table(livraria$tipo_capa)
round(prop.table(table(livraria$tipo_capa)), 2)

table(livraria$flag_universitario)
round(prop.table(table(livraria$flag_universitario)), 2)

table(livraria$densidade_papel)
round(prop.table(table(livraria$densidade_papel)), 2)

table(livraria$cor)
round(prop.table(table(livraria$cor)), 2)

table(livraria$ano)
round(prop.table(table(livraria$ano)), 2)


######################################################
####### ANALISE BIVARIADA
######################################################

# Graficos de todas as variaveis versus preco

par(mfrow = c(2,4))
plot(livraria$preco ~ livraria$qtde_paginas,          col = "darkturquoise", main = "Qtde de paginas", xlab = "")
boxplot(livraria$preco ~ livraria$edicao,             col = "darkturquoise", main = "Edicao", xlab = "")
boxplot(livraria$preco ~ livraria$tipo_capa,          col = "darkturquoise", main = "Tipo de capa", xlab = "")
boxplot(livraria$preco ~ livraria$flag_universitario, col = "darkturquoise", main = "Universitario", xlab = "")
boxplot(livraria$preco ~ livraria$densidade_papel,    col = "darkturquoise", main = "Densidade do papel", xlab = "")
boxplot(livraria$preco ~ livraria$cor,                col = "darkturquoise", main = "Cor", xlab = "")
boxplot(livraria$preco ~ livraria$ano,                col = "darkturquoise", main = "Ano", xlab = "")

# Correlacoes lineares de Pearson, para as variaveis quantitativas

cor(livraria$preco, livraria$qtde_paginas)


######################################################
####### REGRESSAO LINEAR
######################################################

# Modelo de regressao linear inicial

options(scipen = 999)

regressao <- lm(preco ~
                  qtde_paginas +
                  edicao +
                  tipo_capa +
                  flag_universitario +
                  densidade_papel +
                  cor +
                  ano,
                data = livraria)

summary(regressao)


regressao1 <- lm(preco ~
                  qtde_paginas +
                  edicao +
                  tipo_capa +
                  flag_universitario +
                  cor +
                  ano,
                data = livraria)

summary(regressao1)
# Avaliando multicolinearidade

vif(regressao1)

# Analise dos residuos

residuos <- residuals(regressao)
valores_preditos <- fitted.values(regressao)

par(mfrow = c(1,3))
hist(residuos, breaks = 25, col = "darkturquoise", main = "Histograma dos residuos")
qqnorm(residuos, pch = 1, col = "darkturquoise", frame = FALSE)
qqline(residuos, col = "steelblue", lwd = 2)
plot(valores_preditos, residuos, main = 'Valores preditos x Residuos', ylab = 'Residuos', col = "darkturquoise")
abline(h = 0)


######################################################
####### ARVORE DE DECISAO
######################################################

# Calculo da variavel resposta

livraria$preco_por_pagina <- as.factor(ifelse (livraria$preco/livraria$qtde_paginas > 0.2, 1, 0))

# Categorizando as variaveis quantitativas

livraria$qtde_paginas_cat <- as.factor(quantcut(livraria$qtde_paginas, q = 4))

# Transformando as variaveis em fatores

livraria$edicao <- as.factor(livraria$edicao)
livraria$tipo_capa <- as.factor(livraria$tipo_capa)
livraria$flag_universitario <- as.factor(livraria$flag_universitario)
livraria$densidade_papel <- as.factor(livraria$densidade_papel)
livraria$cor <- as.factor(livraria$cor)
livraria$ano <- as.factor(livraria$ano)

# Arvore de decisao de 3 niveis, de acordo com o algoritmo CHAID

controle <- chaid_control(maxheight = 3)

arvore_3niveis <- chaid(preco_por_pagina ~
                          edicao +
                          tipo_capa +
                          flag_universitario +
                          densidade_papel +
                          cor +
                          ano,
                        data = livraria,
                        control = controle)

plot(arvore_3niveis, gp = gpar(cex = 0.8))
print(arvore_3niveis)

# Marcando os perfis na base e avaliando frequencia absoluta/relativa

livraria$no_arvore <- predict(arvore_3niveis, livraria, type = "node")
table(livraria$no_arvore)
round(prop.table(table(livraria$no_arvore)), 3)

# Obtendo probabilidade predita e no da arvore

prob_geral <- mean(as.numeric(livraria$preco_por_pagina)) - 1
livraria$prob_arvore <- predict(arvore_3niveis, livraria, type = "p")[,2]
livraria$predito_arvore <- ifelse(livraria$prob_arvore >= prob_geral, 1, 0)

# Funcao para calculo de medidas de desempenho do modelo

desempenho <- function (base, real, predito, prob) {
  
  tabela <- table(real, predito)
  
  acuracia <- (sum(real == 0 & predito == 0) + sum(real == 1 & predito == 1)) / nrow(base)
  sensibilidade <- sum(real == 1 & predito == 1) / (sum(real == 1 & predito == 0) + sum(real == 1 & predito == 1))
  especificidade <- sum(real == 0 & predito == 0) / (sum(real == 0 & predito == 0) + sum(real == 0 & predito == 1))
  ks <- ks_stat(actuals = real,
                predictedScores = prob)

  print(paste0("Acuracia = ", round(acuracia, 3)))
  print(paste0("Sensibilidade = ", round(sensibilidade, 3)))
  print(paste0("Especificidade = ", round(especificidade, 3)))
  print(paste0("KS = ", round(ks, 3)))
  plotROC(actuals = real,
          predictedScores = prob)
  
}

# Desempenho geral do modelo

desempenho(livraria,
           livraria$preco_por_pagina,
           livraria$predito_arvore,
           livraria$prob_arvore)


# Desempenho do modelo: por qtde de paginas

livraria_filtro <- livraria %>% filter(qtde_paginas_cat == '[72,345]')

desempenho(livraria_filtro,
           livraria_filtro$preco_por_pagina,
           livraria_filtro$predito_arvore,
           livraria_filtro$prob_arvore)

livraria_filtro <- livraria %>% filter(qtde_paginas_cat == '(345,467]')

desempenho(livraria_filtro,
           livraria_filtro$preco_por_pagina,
           livraria_filtro$predito_arvore,
           livraria_filtro$prob_arvore)

livraria_filtro <- livraria %>% filter(qtde_paginas_cat == '(467,622]')

desempenho(livraria_filtro,
           livraria_filtro$preco_por_pagina,
           livraria_filtro$predito_arvore,
           livraria_filtro$prob_arvore)

livraria_filtro <- livraria %>% filter(qtde_paginas_cat == '(622,1.61e+03]')

desempenho(livraria_filtro,
           livraria_filtro$preco_por_pagina,
           livraria_filtro$predito_arvore,
           livraria_filtro$prob_arvore)

# Desempenho do modelo: por flag universitario

livraria_filtro <- livraria %>% filter(flag_universitario == 0)

desempenho(livraria_filtro,
           livraria_filtro$preco_por_pagina,
           livraria_filtro$predito_arvore,
           livraria_filtro$prob_arvore)

livraria_filtro <- livraria %>% filter(flag_universitario == 1)

desempenho(livraria_filtro,
           livraria_filtro$preco_por_pagina,
           livraria_filtro$predito_arvore,
           livraria_filtro$prob_arvore)

# Desempenho do modelo: por densidade do papel

livraria_filtro <- livraria %>% filter(densidade_papel == 'Alta')

desempenho(livraria_filtro,
           livraria_filtro$preco_por_pagina,
           livraria_filtro$predito_arvore,
           livraria_filtro$prob_arvore)

livraria_filtro <- livraria %>% filter(densidade_papel == 'Media')

desempenho(livraria_filtro,
           livraria_filtro$preco_por_pagina,
           livraria_filtro$predito_arvore,
           livraria_filtro$prob_arvore)

livraria_filtro <- livraria %>% filter(densidade_papel == 'Baixa')

desempenho(livraria_filtro,
           livraria_filtro$preco_por_pagina,
           livraria_filtro$predito_arvore,
           livraria_filtro$prob_arvore)


######################################################
####### REGRESSAO LOGISTICA
######################################################

options(scipen = 999)

# Calculo da variavel resposta

livraria$preco_por_pagina <- as.numeric(ifelse (livraria$preco/livraria$qtde_paginas > 0.2, 1, 0))

# Information value (IV) das variaveis

IV <- create_infotables(data = livraria[,c(1:7,9)],
                        y = "preco_por_pagina")
IV$Summary

# Modelo de regressao logistica inicial

regressao_log <- glm(preco_por_pagina ~
                       edicao +
                       tipo_capa +
                       flag_universitario +
                       densidade_papel +
                       cor +
                       ano,
                     data = livraria,
                     family = binomial(link = "logit"))

summary(regressao_log)

regressao_log1 <- glm(preco_por_pagina ~
                       edicao +
                       tipo_capa +
                       flag_universitario +
                       cor +
                       ano,
                     data = livraria,
                     family = binomial(link = "logit"))

summary(regressao_log1)

# Avaliando multicolinearidade

vif(regressao_log1)

# Obtendo probabilidade predita e resposta predita

prob_geral <- mean(as.numeric(livraria$preco_por_pagina))
livraria$prob_regressao <- predict(regressao_log, livraria, type = "response")
livraria$predito_regressao <- ifelse(livraria$prob_regressao >= prob_geral, 1, 0)

# Desempenho geral do modelo

desempenho(livraria,
           livraria$preco_por_pagina,
           livraria$predito_regressao,
           livraria$prob_regressao)

# Desempenho do modelo: por qtde de paginas

livraria_filtro <- livraria %>% filter(qtde_paginas_cat == '[72,345]')

desempenho(livraria_filtro,
           livraria_filtro$preco_por_pagina,
           livraria_filtro$predito_regressao,
           livraria_filtro$prob_regressao)

livraria_filtro <- livraria %>% filter(qtde_paginas_cat == '(345,467]')

desempenho(livraria_filtro,
           livraria_filtro$preco_por_pagina,
           livraria_filtro$predito_regressao,
           livraria_filtro$prob_regressao)

livraria_filtro <- livraria %>% filter(qtde_paginas_cat == '(467,622]')

desempenho(livraria_filtro,
           livraria_filtro$preco_por_pagina,
           livraria_filtro$predito_regressao,
           livraria_filtro$prob_regressao)

livraria_filtro <- livraria %>% filter(qtde_paginas_cat == '(622,1.61e+03]')

desempenho(livraria_filtro,
           livraria_filtro$preco_por_pagina,
           livraria_filtro$predito_regressao,
           livraria_filtro$prob_regressao)

# Desempenho do modelo: por flag universitario

livraria_filtro <- livraria %>% filter(flag_universitario == 0)

desempenho(livraria_filtro,
           livraria_filtro$preco_por_pagina,
           livraria_filtro$predito_regressao,
           livraria_filtro$prob_regressao)

livraria_filtro <- livraria %>% filter(flag_universitario == 1)

desempenho(livraria_filtro,
           livraria_filtro$preco_por_pagina,
           livraria_filtro$predito_regressao,
           livraria_filtro$prob_regressao)

# Desempenho do modelo: por densidade do papel

livraria_filtro <- livraria %>% filter(densidade_papel == 'Alta')

desempenho(livraria_filtro,
           livraria_filtro$preco_por_pagina,
           livraria_filtro$predito_regressao,
           livraria_filtro$prob_regressao)

livraria_filtro <- livraria %>% filter(densidade_papel == 'Media')

desempenho(livraria_filtro,
           livraria_filtro$preco_por_pagina,
           livraria_filtro$predito_regressao,
           livraria_filtro$prob_regressao)

livraria_filtro = livraria %>% filter(densidade_papel == 'Baixa')

desempenho(livraria_filtro,
           livraria_filtro$preco_por_pagina,
           livraria_filtro$predito_regressao,
           livraria_filtro$prob_regressao)

# Modelo de regressao logistica segmentado (se necessario)

livraria_segmento <- livraria %>% filter(...) # alterar esta linha e rodar os codigos a seguir para cada um dos segmentos

regressao_seg <- glm(preco_por_pagina ~
                       ..., # completar com as variaveis explicativas (excluindo a de segmentacao)
                     data = livraria_segmento,
                     family = binomial(link = "logit"))

summary(regressao_seg)

# Avaliando multicolinearidade

vif(regressao_seg)

# Obtendo probabilidade predita e resposta predita

prob_geral <- mean(as.numeric(livraria_segmento$preco_por_pagina))
livraria_segmento$prob_regressao <- predict(regressao_seg, livraria_segmento, type = "response")
livraria_segmento$predito_regressao <- ifelse(livraria_segmento$prob_regressao >= prob_geral, 1, 0)

# Desempenho do modelo

desempenho(livraria_segmento,
           livraria_segmento$preco_por_pagina,
           livraria_segmento$predito_regressao,
           livraria_segmento$prob_regressao)


######################################################
####### ANALISE DE CLUSTER: METODO HIERARQUICO
######################################################

# Padronizacao das variaveis

livraria_z <- scale(livraria_cluster[,1:3])

# Matriz de distancias euclidianas

distancia <- dist(livraria_z, method = "euclidean")

# Agrupamento hierarquico (metodos single e complete)

par(mfrow = c(1,2))

clust_single <- hclust(distancia, method = "single") 
plot(clust_single, main = "Metodo Single", hang = -1, labels = FALSE)

clust_complete <- hclust(distancia, method = "complete")
plot(clust_complete, main = "Metodo Complete", hang = -1, labels = FALSE)

# Marcando os grupos na base amostral, a partir do metodo desejado e de um valor de k

livraria_cluster$cluster <- as.factor(cutree(clust_complete, k = 4)) # completar o metodo e o valor de k

# Tamanho dos clusters

table(livraria_cluster$cluster)
prop.table(table(livraria_cluster$cluster))

# Boxplots das variaveis por cluster

par(mfrow = c(1,3))

boxplot(livraria_cluster$preco ~ livraria_cluster$cluster,
        col = "darkturquoise", main = "Preco")
abline(h = median(livraria_cluster$preco), col = "red")

boxplot(livraria_cluster$preco_por_pagina ~ livraria_cluster$cluster,
        col = "darkturquoise", main = "Preco por pagina")
abline(h = median(livraria_cluster$preco_por_pagina), col = "red")

boxplot(livraria_cluster$qtde_vendas_6m ~ livraria_cluster$cluster,
        col = "darkturquoise", main = "Qtde. de vendas")
abline(h = median(livraria_cluster$qtde_vendas_6m), col = "red")


######################################################
####### ANALISE DE CLUSTER: METODO K-MEDIAS
######################################################

# Definicao da semente

set.seed(12345)

# Padronizacao das variaveis

livraria_z <- scale(livraria_cluster[,1:3])

# Agrupamento com k-medias

modelo.k3 <- kmeans(livraria_z, centers = 3, nstart = 25, iter.max = 100)
modelo.k4 <- kmeans(livraria_z, centers = 4, nstart = 25, iter.max = 100)
modelo.k5 <- kmeans(livraria_z, centers = 5, nstart = 25, iter.max = 100)

# Graficos
G3 <- fviz_cluster(modelo.k3, geom = "point", data = livraria_z) + ggtitle("k = 3")
G4 <- fviz_cluster(modelo.k4, geom = "point", data = livraria_z) + ggtitle("k = 4")
G5 <- fviz_cluster(modelo.k5, geom = "point", data = livraria_z) + ggtitle("k = 5")
grid.arrange(G3, G4, G5, nrow = 1)

# Marcando os grupos na base completa, considerando algum valor de k

livraria_cluster$cluster_km <- modelo.k4$cluster # completar com o modelo desejado (modelo.k3, modelo.k4 ou modelo.k5)

# Tamanho dos clusters

table(livraria_cluster$cluster_km)
prop.table(table(livraria_cluster$cluster_km))

# Boxplots das variaveis por cluster

par(mfrow = c(1,3))

boxplot(livraria_cluster$preco ~ livraria_cluster$cluster_km,
        col = "darkturquoise", main = "Preco")
abline(h = median(livraria_cluster$preco), col = "red")

boxplot(livraria_cluster$preco_por_pagina ~ livraria_cluster$cluster_km,
        col = "darkturquoise", main = "Preco por pagina")
abline(h = median(livraria_cluster$preco_por_pagina), col = "red")

boxplot(livraria_cluster$qtde_vendas_6m ~ livraria_cluster$cluster_km,
        col = "darkturquoise", main = "Qtde. de vendas")
abline(h = median(livraria_cluster$qtde_vendas_6m), col = "red")
