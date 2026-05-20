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

scores_gol <- read_excel("scores_goleiros.xlsx", sheet = 1)

# Selecionando o numero de clusters

  ## Elbow Method

set.seed(123)

fviz_nbclust(scores_gol["Score_Final"], kmeans, method = "wss", k.max = 9, nstart = 25) +
  ggtitle("Método do Cotovelo") +
  theme_minimal()

  ## Silhouette Method

set.seed(123)

fviz_nbclust(scores_gol["Score_Final"], kmeans, method = "silhouette", k.max = 9, nstart = 25) +
  ggtitle("Método da Silhueta") +
  theme_minimal()

  ## NbClust

set.seed(123)

nb_result <- NbClust(data = scores_gol["Score_Final"], distance = "euclidean", 
                     min.nc = 2, max.nc = 9, method = "kmeans", index = "all")             

table(nb_result$Best.nc[1, ]) # 2 clusters

k_ideal <- 2

# K-Means

set.seed(123)

kmeans_result <- kmeans(scores_gol["Score_Final"], centers = k_ideal, nstart = 25)
scores_gol$Cluster <- as.factor(kmeans_result$cluster)

# Criando tabela de medias por cluster

cluster_media <- scores_gol %>% group_by(Cluster) %>%
  summarise(media_score = mean(Score_Final)) %>%
  arrange(desc(media_score)) %>% mutate(novo_cluster = row_number()) 

cluster_media

# Juntar para atualizar a numeracao dos clusters

scores_gol <- scores_gol %>% left_join(cluster_media, by = "Cluster") %>%
  mutate(Cluster_ordenado = as.factor(novo_cluster)) %>%
  select(-Cluster, -media_score, -novo_cluster) %>%
  rename(Cluster = Cluster_ordenado)

# Validacao

scores_gol %>% group_by(Cluster) %>%
  summarise(Média = mean(Score_Final), Mediana = median(Score_Final), Jogadores = n()) %>% arrange(Cluster)

# Metodo de Silhuette para avaliacao final

silhouette_result <- silhouette(as.numeric(scores_gol$Cluster), dist(scores_gol["Score_Final"]))

# Gerar o Grafico de Silhueta

fviz_silhouette(silhouette_result, label = FALSE,  
                ggtheme = theme_minimal()) + labs(title = paste0("Gráfico de Silhueta para k = ", k_ideal))

# Exibir a largura media da silhueta geral

summary(silhouette_result)

# Testes

#   ## Normalidade dos clusters
# 
# scores_gol %>% group_by(Cluster) %>% summarise(p_valor_shapiro = shapiro.test(Score_Final)$p.value) 

  ## Kruskal-Wallis

kruskal_result <- kruskal.test(Score_Final ~ Cluster, data = scores_gol)
kruskal_result

# Grafico

## Boxplot do Score Final por cluster

ggplot(scores_gol, aes(x = Cluster, y = Score_Final, fill = Cluster)) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA) +
  geom_jitter(aes(color = Cluster), size = 2, width = 0.15, alpha = 0.7) +
  theme_minimal(base_size = 12) + labs(title = "Distribuição do Score Final por Cluster",
                                       x = "Cluster", y = "Score Final (0–100)",
                                       color = "Cluster", fill = "Cluster") +
  scale_color_brewer(palette = "Set2") + scale_fill_brewer(palette = "Set2")

# Summary dos scores por cluster

cluster1 <- scores_gol %>% filter(Cluster == 1)
summary(cluster1$Score_Geral)

cluster2 <- scores_gol %>% filter(Cluster == 2)
summary(cluster2$Score_Geral)

# objetos_para_manter <- c("scores_gol")
# 
# rm(list = setdiff(ls(), objetos_para_manter))


