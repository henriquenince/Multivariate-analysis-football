########## CLUSTERIZACAO ########## 

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(stringi)
library(writexl)
library(psych)
library(caret)
library(factoextra)
library(cluster)
library(NbClust)
library(FSA)

setwd("D:/Henrique Nince/OneDrive/Área de Trabalho/UnB/TCC/Análises/6ª Tentativa/4. Tratamento final, Análise Fatorial e Scores")

scores_vol <- read_excel("scores_volantes.xlsx", sheet = 1)

# Selecionando o numero de clusters

  ## Elbow Method

set.seed(123)

fviz_nbclust(scores_vol["Score_Final"], kmeans, method = "wss", k.max = 9, nstart = 25) +
  ggtitle("Método do Cotovelo") +
  theme_minimal()

  ## Silhouette Method

set.seed(123)

fviz_nbclust(scores_vol["Score_Final"], kmeans, method = "silhouette", k.max = 9, nstart = 25) +
  ggtitle("Método da Silhueta") +
  theme_minimal()

  ## NbClust

set.seed(123)

nb_result <- NbClust(data = scores_vol["Score_Final"], distance = "euclidean", 
                     min.nc = 2, max.nc = 9, method = "kmeans", index = "all")             

table(nb_result$Best.nc[1, ]) # 3 clusters

k_ideal <- 3

# K-Means

set.seed(123)

kmeans_result <- kmeans(scores_vol["Score_Final"], centers = k_ideal, nstart = 25)
scores_vol$Cluster <- as.factor(kmeans_result$cluster)

# Criando tabela de medias por cluster

cluster_media <- scores_vol %>% group_by(Cluster) %>%
  summarise(media_score = mean(Score_Final)) %>%
  arrange(desc(media_score)) %>% mutate(novo_cluster = row_number()) 

cluster_media

# Juntar para atualizar a numeracao dos clusters

scores_vol <- scores_vol %>% left_join(cluster_media, by = "Cluster") %>%
  mutate(Cluster_ordenado = as.factor(novo_cluster)) %>%
  select(-Cluster, -media_score, -novo_cluster) %>%
  rename(Cluster = Cluster_ordenado)

# Validacao

scores_vol %>% group_by(Cluster) %>%
  summarise(Média = mean(Score_Final), Mediana = median(Score_Final), Jogadores = n()) %>% arrange(Cluster)

# Metodo de Silhuette para avaliacao final

silhouette_result <- silhouette(as.numeric(scores_vol$Cluster), dist(scores_vol["Score_Final"]))

# Gerar o Grafico de Silhueta

fviz_silhouette(silhouette_result, label = FALSE,  
                ggtheme = theme_minimal()) + labs(title = paste0("Gráfico de Silhueta para k = ", k_ideal))

# Exibir a largura media da silhueta geral

summary(silhouette_result)

# Testes

#   ## Normalidade dos clusters
# 
# scores_vol %>% group_by(Cluster) %>% summarise(p_valor_shapiro = shapiro.test(Score_Final)$p.value) 

  ## Kruskal-Wallis

kruskal_result <- kruskal.test(Score_Final ~ Cluster, data = scores_vol)
kruskal_result

  ## Dunn

dunn_result <- dunnTest(Score_Final ~ Cluster, data = scores_vol, method = "bonferroni")
dunn_result

# Grafico

## Boxplot do Score Final por cluster

ggplot(scores_vol, aes(x = Cluster, y = Score_Final, fill = Cluster)) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA) +
  geom_jitter(aes(color = Cluster), size = 2, width = 0.15, alpha = 0.7) +
  theme_minimal(base_size = 12) + labs(title = "Distribuição do Score Final por Cluster",
                                       x = "Cluster", y = "Score Final (0–100)",
                                       color = "Cluster", fill = "Cluster") +
  scale_color_brewer(palette = "Set2") + scale_fill_brewer(palette = "Set2")

# Summary dos scores por cluster

cluster1 <- scores_vol %>% filter(Cluster == 1)
summary(cluster1$Score_MR1)
summary(cluster1$Score_MR4)
summary(cluster1$Score_MR2)
summary(cluster1$Score_MR5)
summary(cluster1$Score_MR3)
summary(cluster1$Score_MR6)

cluster2 <- scores_vol %>% filter(Cluster == 2)
summary(cluster2$Score_MR1)
summary(cluster2$Score_MR4)
summary(cluster2$Score_MR2)
summary(cluster2$Score_MR5)
summary(cluster2$Score_MR3)
summary(cluster2$Score_MR6)

cluster3 <- scores_vol %>% filter(Cluster == 3)
summary(cluster3$Score_MR1)
summary(cluster3$Score_MR4)
summary(cluster3$Score_MR2)
summary(cluster3$Score_MR5)
summary(cluster3$Score_MR3)
summary(cluster3$Score_MR6)

# objetos_para_manter <- c("scores_gol", "scores_zag", "scores_lat", "scores_vol")
# 
# rm(list = setdiff(ls(), objetos_para_manter))


