########## CORRELACOES ########## 

library(corrplot)
library(tidyverse)
library(openxlsx)
library(ggcorrplot)

# ------------ Goleiros ------------ #

# Selecionar so as variaveis numericas (a partir da 8a coluna)

dados_num_gol <- banco_goleiros[, c(7:40, 42)]

# Matriz de correlacao

corr <- cor(dados_num_gol)

ggcorrplot(corr, hc.order = TRUE, type = "full",
           lab = FALSE, show.legend = TRUE,
           colors = c("#B2182B", "white", "#2166AC")) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

# Vendo as maiores correlacoes

cor_long <- corr %>%
  as.data.frame() %>% rownames_to_column("var1") %>%
  pivot_longer(cols = -var1, names_to = "var2", values_to = "cor")

cor_long_unico <- cor_long %>% filter(var1 < var2)

cor_altas <- cor_long_unico %>% filter(abs(cor) >= 0.9) %>% arrange(desc(abs(cor)))

summary(cor_long_unico$cor)

# ------------ Geral ------------ #

# Selecionar so as variaveis numericas (a partir da 8a coluna)

dados_num <- banco_geral[, 8:ncol(banco_geral)]

dados_num <- dados_num %>% dplyr::select(-"(Expected) Ast")

# Matriz de correlacao

corr <- cor(dados_num)

ggcorrplot(corr, hc.order = TRUE, type = "full",
  lab = FALSE, show.legend = TRUE,
  colors = c("#B2182B", "white", "#2166AC")) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

# Vendo as maiores correlacoes

cor_long <- corr %>%
  as.data.frame() %>% rownames_to_column("var1") %>%
  pivot_longer(cols = -var1, names_to = "var2", values_to = "cor")

cor_long_unico <- cor_long %>% filter(var1 < var2)

cor_altas <- cor_long_unico %>% filter(abs(cor) >= 0.9) %>% arrange(desc(abs(cor)))

summary(cor_long_unico$cor)



