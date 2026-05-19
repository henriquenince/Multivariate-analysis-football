########## ANALISE FATORIAL ########## 

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(stringi)
library(writexl)
library(psych)
library(caret)
library(MVN)

dados_pad <- dados_pad_zag

# # Verificando normalidade (os dados nao seguem distribuicao normal)
# 
# apply(dados_pad, 2, shapiro.test)
# 
# hist(dados_pad[,1])
# qqnorm(dados_pad[,1])
# qqline(dados_pad[,1])

# Calcular autovalores da matriz de correlacao

R <- cor(dados_pad)
autovalores <- eigen(R)$values

# Criterio de Kaiser: quantos > 1

n_fatores_kaiser <- sum(autovalores > 1)
cat("Número de fatores sugerido pelo critério de Kaiser:", n_fatores_kaiser, "\n")

# Scree Plot

plot(autovalores, type="b", pch=19,
     main="Scree Plot - Autovalores",
     xlab="Componente", ylab="Autovalor")
abline(h=1, col="red", lty=2) 

# Variancia acumulada

prop_var <- cumsum(autovalores) / sum(autovalores)
print(prop_var)

# Analise paralela

fa.parallel(dados_pad, fa = "fa", fm = "minres", main = "Análise Paralela")

# Definir o numero de fatores a ser usado

n_fatores <- 4 # testei com 3, 4 e 5 fatores (4 foi o melhor)

# Analise Fatorial 

resultado <- fa(dados_pad, nfactors = n_fatores, fm = "minres", rotate = "oblimin", scores = "regression")
print(resultado, digits = 3)
fa.diagram(resultado)

# Filtrando variaveis com comunalidade >= 0.5

comunalidades <- resultado$communality
variaveis_boas <- names(comunalidades[comunalidades >= 0.5])

dados_pad <- dados_pad[, variaveis_boas]

kmo_res_filtrado <- KMO(dados_pad)
print(paste("KMO Global após filtro:", round(kmo_res_filtrado$MSA, 3)))

# Refazendo Analise Fatorial 

resultado <- fa(dados_pad, nfactors = n_fatores, fm = "minres", rotate = "oblimin", scores = "regression")
print(resultado, digits = 3)
fa.diagram(resultado)

# Resultados principais

print(resultado$loadings)

print(sort(resultado$communality)) 

print(resultado$Vaccounted)

print(resultado$uniquenesses)

print(resultado$e.values)

print(resultado$score.cor)

########## SCORES ########## 

# Extraindo scores

scores_fatoriais <- as.data.frame(resultado$scores)
colnames(scores_fatoriais) <- c("Score_MR1", "Score_MR2", "Score_MR3", "Score_MR4")

# Normalizando 0–100 (para evitar qualquer distorcao numerica)

normalize_0_100 <- function(x) (x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE)) * 100

scores_fatoriais <- scores_fatoriais %>%
  mutate(Score_MR1 = normalize_0_100(scores_fatoriais$Score_MR1),
         Score_MR2 = normalize_0_100(scores_fatoriais$Score_MR2),
         Score_MR3 = normalize_0_100(scores_fatoriais$Score_MR3),
         Score_MR4 = normalize_0_100(scores_fatoriais$Score_MR4))

# Juntando bancos

banco_zagueiros <- cbind(banco_zagueiros[, c(1:7, 118)], as.data.frame(scores_fatoriais))

# Calculando score geral

  ## Pesos pela variancia explicada

pesos <- c(MR1 = 0.440, MR2 = 0.178, MR3 = 0.107, MR4 = 0.103)
pesos <- pesos / sum(pesos)  # normalizar para somar 1

banco_zagueiros <- banco_zagueiros %>%
  mutate(Score_Geral_Var = (Score_MR1 * pesos["MR1"]) +
           (Score_MR2 * pesos["MR2"]) +
           (Score_MR3 * pesos["MR3"]) +
           (Score_MR4 * pesos["MR4"]))

summary(banco_zagueiros$Score_Geral_Var)

# Correlacao dos scores com gols sofridos

cor(banco_zagueiros$Score_Geral_Var, banco_zagueiros$`(Team Success) onGA`, use = "complete.obs")

  ## Ajustando pesos pelos gols sofridos

# Calculando gols sofridos por 90 minutos jogados

  ## Retirando jogadores com 0.0 "(Playing Time) 90s"

banco_zagueiros <- banco_zagueiros %>% filter(`(Playing Time) 90s` > 0)

banco_zagueiros <- banco_zagueiros %>% mutate(Gols_90 = `(Team Success) onGA` / `(Playing Time) 90s`)

# Criando peso inversamente proporcional aos gols sofridos por 90

peso_gols_90 <- 1 / (1 + banco_zagueiros$Gols_90)

# Recalculando o score ajustado

banco_zagueiros <- banco_zagueiros %>%
  mutate(Score_Final = Score_Geral_Var * peso_gols_90)

# # Normalizando novamente 0-100
# 
# normalize_0_100 <- function(x) (x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE)) * 100
# 
# banco_zagueiros <- banco_zagueiros %>%
#   mutate(Score_Final = normalize_0_100(Score_Final))

#   ## Ajustando pesos pela quantidade de partidas
# 
# banco_zagueiros <- banco_zagueiros %>%
#   mutate(peso_partidas = log1p(`(Playing Time) 90s`) / log1p(max(`(Playing Time) 90s`, na.rm = TRUE)),
#          Score_Final = Score_Ajustado * peso_partidas)

# Salvando banco com scores

banco_zagueiros <- banco_zagueiros[, c(1, 3, 4, 9, 10, 11, 12, 13, 15)]

write_xlsx(banco_zagueiros, "scores_zagueiros.xlsx")








