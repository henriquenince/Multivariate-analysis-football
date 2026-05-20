########## SELECAO DAS VARIAVEIS ########## 

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(stringi)
library(writexl)
library(psych)
library(caret)

banco_geral <- read_excel("banco_sem_transf.xlsx", sheet = 1)
banco_goleiros <- read_excel("banco_goleiros_sem_transf.xlsx", sheet = 1)

transf_geral <- read_excel("banco_transf.xlsx", sheet = 1)
transf_goleiros <- read_excel("banco_goleiros_transf.xlsx", sheet = 1)

# ------------ Banco para geral ------------ #

  ## Retirando algumas colunas

names(banco_geral)
banco_geral <- banco_geral[, c(-26, -27, -28, -29, -30, -31, -32, -33, -34, -35, -39, -40, -84, -92, -146, -151)]

  ## Corrigindo formato das variaveis

str(banco_geral)

  ## Converter todas as colunas character para numericas, exceto as 7 primeiras

cols_to_convert <- names(banco_geral)[7:ncol(banco_geral)]
char_cols <- cols_to_convert[sapply(banco_geral[cols_to_convert], is.character)]

  ## Aplicar a conversao apenas nessas colunas

banco_geral[char_cols] <- lapply(banco_geral[char_cols], function(x) {
  as.numeric(gsub(",", ".", gsub("[^0-9\\.-]", "", x)))})

banco_geral <- banco_geral[, c(-7, -8, -9, -121, -122, -123, -124, -125, -126, -127, -128)]

# ------------ Banco para transferidos ------------ #

  ## Retirando algumas colunas

transf_geral <- transf_geral[, c(-26, -27, -28, -29, -30, -31, -32, -33, -34, -35, -39, -40, -84, -92, -146, -151)]

  ## Corrigindo formato das variaveis

str(transf_geral)

  ## Converter todas as colunas character para numericas, exceto as 5 primeiras

cols_to_convert <- names(transf_geral)[7:ncol(transf_geral)]
char_cols <- cols_to_convert[sapply(transf_geral[cols_to_convert], is.character)]

  ## Aplicar a conversao apenas nessas colunas

transf_geral[char_cols] <- lapply(transf_geral[char_cols], function(x) {
  as.numeric(gsub(",", ".", gsub("[^0-9\\.-]", "", x)))})

transf_geral <- transf_geral[, c(-7, -8, -9, -121, -122, -123, -124, -125, -126, -127, -128)]

  ## Unificando jogadores

cols_fixas <- c("Nation", "Position", "Squad", "Age", "Born")

transf_geral <- transf_geral %>%
  group_by(Player) %>%
  summarise(across(all_of(cols_fixas), ~ first(.x), .names = "{.col}"),
    across(where(is.numeric) & !all_of(cols_fixas), ~ sum(.x, na.rm = TRUE), .names = "{.col}")) %>% ungroup()

transf_geral <- transf_geral %>%
  mutate(
    `(Standard) SoT%` = round((`(Standard) SoT` / `(Standard) Sh`) * 100, 1),
    `(Total) Cmp%` = round((`(Total) Cmp` / `(Total) Att`) * 100, 1),
    `(Short) Cmp%` = round((`(Short) Cmp` / `(Short) Att`) * 100, 1),
    `(Medium) Cmp%` = round((`(Medium) Cmp` / `(Medium) Att`) * 100, 1),
    `(Long) Cmp%` = round((`(Long) Cmp` / `(Long) Att`) * 100, 1),
    `(Challenges) Tkl%` = round((`(Challenges) Tkl` / `(Challenges) Att`) * 100, 1),
    `(Take-Ons) Succ%` = round((`(Take-Ons) Succ` / `(Take-Ons) Att`) * 100, 1),
    `(Take-Ons) Tkld%` = round((`(Take-Ons) Tkld` / `(Take-Ons) Att`) * 100, 1),
    `(Aerial Duels) Won%` = round((`(Aerial Duels) Won` /
                                     (`(Aerial Duels) Won` + `(Aerial Duels) Lost`)) * 100, 1))

  ## Juntando bancos

banco_geral <- rbind(banco_geral, transf_geral)

banco_geral <- banco_geral %>% mutate_all(~ ifelse(is.na(.), 0.0, .))

# Retirando colunas de Team Success

  ## Utilizando gols sofridos para zagueiros

banco_zagueiros <- banco_geral %>% filter(Position == "Centre-Back")
banco_zagueiros <- banco_zagueiros[, c(-118, -120, -121, -122, -123, -124, -125)]

banco_geral <- banco_geral[, c(-118, -119, -120, -121, -122, -123, -124, -125)]

# ------------ Banco para goleiros ------------ #

  ## Retirando algumas colunas

names(banco_goleiros)
banco_goleiros <- banco_goleiros[, c(-11, -31, -46)]

  ## Corrigindo formato das variaveis

str(banco_goleiros)

  ## Converter todas as colunas character para numericas, exceto as 5 primeiras

cols_to_convert <- names(banco_goleiros)[6:ncol(banco_goleiros)]
char_cols <- cols_to_convert[sapply(banco_goleiros[cols_to_convert], is.character)]

  ## Aplicar a conversao apenas nessas colunas

banco_goleiros[char_cols] <- lapply(banco_goleiros[char_cols], function(x) {
  as.numeric(gsub(",", ".", gsub("[^0-9\\.-]", "", x)))})

banco_goleiros <- banco_goleiros[, c(-6, -7, -8, -17, -18, -19)]

# ------------ Banco para goleiros transferidos ------------ #

  ## Retirando algumas colunas

transf_goleiros <- transf_goleiros[, c(-11, -31, -46)]

  ## Corrigindo formato das variaveis

str(transf_goleiros)

  ## Converter todas as colunas character para numericas, exceto as 5 primeiras

cols_to_convert <- names(transf_goleiros)[6:ncol(transf_goleiros)]
char_cols <- cols_to_convert[sapply(transf_goleiros[cols_to_convert], is.character)]

  ## Aplicar a conversao apenas nessas colunas

transf_goleiros[char_cols] <- lapply(transf_goleiros[char_cols], function(x) {
  as.numeric(gsub(",", ".", gsub("[^0-9\\.-]", "", x)))})

transf_goleiros <- transf_goleiros[, c(-6, -7, -8, -17, -18, -19)]

  ## Unificando jogadores

cols_fixas <- c("Nation", "Pos", "Squad", "Age")

transf_goleiros <- transf_goleiros %>%
  group_by(Player) %>%
  summarise(across(all_of(cols_fixas), ~ first(.x), .names = "{.col}"),
            across(where(is.numeric) & !all_of(cols_fixas), ~ sum(.x, na.rm = TRUE), .names = "{.col}")) %>% ungroup()

transf_goleiros <- transf_goleiros %>%
  mutate(
    `(Performance) Save%` = round((`(Performance) Saves` / `(Performance) SoTA`) * 100, 1),
    `(Performance) CS%` = round((`(Performance) CS` / `(Playing Time) 90s`) * 100, 1),
    `(Penalty Kicks) Save%` = round((`(Penalty Kicks) PKsv` / `(Penalty Kicks) PKatt`) * 100, 1),
    `(Launched) Cmp%` = round((`(Launched) Cmp` / `(Launched) Att`) * 100, 1),
    `(Passes) Launch%` = round((`(Passes) Thr` / `(Passes) Att (GK)`) * 100, 1),
    `(Goal Kicks) Launch%` = round(((36.8 * 3) + (15.1 * 21)) / (3 + 21), 1),
    `(Crosses) Stp%` = round((`(Crosses) Stp` / `(Crosses) Opp`) * 100, 1))

  ## Juntando bancos

banco_goleiros <- rbind(banco_goleiros, transf_goleiros)

banco_goleiros <- banco_goleiros %>% mutate_all(~ ifelse(is.na(.), 0.0, .))

# Retirando colunas de Team Success

banco_goleiros <- banco_goleiros[, c(-42)]

objetos_para_manter <- c("banco_geral", "banco_goleiros", "banco_zagueiros")

rm(list = setdiff(ls(), objetos_para_manter))

# ------------ Banco para goleiros ------------ #

table(banco_goleiros$Nation)
summary(banco_goleiros$Age)

  ## Selecionar so as variaveis numericas (a partir da 8a coluna)

dados_num_gol <- banco_goleiros[, c(7:40, 42)]

  ## Retirando manualmente combinacoes lineares

dados_num_gol <- dados_num_gol %>%
  dplyr::select(-"(Performance) Save%", -"(Performance) CS%",  
                -"(Penalty Kicks) PKA", -"(Penalty Kicks) PKsv", -"(Penalty Kicks) PKm", 
                -"(Expected) PSxG/SoT", -"(Expected) PSxG+/-",
                -"(Launched) Cmp%", -"(Crosses) Stp%")

  ## Remover variaveis com desvio-padrao zero ou NA

variaveis_validas_gol <- apply(dados_num_gol, 2, sd, na.rm = TRUE) > 0
dados_num_gol <- dados_num_gol[, variaveis_validas_gol]

  ## Detectar combinacoes lineares nos dados

combos <- findLinearCombos(dados_num_gol)

if(!is.null(combos$remove)) {
  message("Variáveis linearmente dependentes detectadas e removidas:")
  print(colnames(dados_num_gol)[combos$remove])
  dados_num_gol <- dados_num_gol[, -combos$remove]}

#   ## Remover variaveis altamente correlacionadas (com base na matriz original)
# 
# cor_mat_gol <- cor(dados_num_gol, use = "pairwise.complete.obs")
# highCorr_gol <- findCorrelation(cor_mat_gol, cutoff = 0.99)
# 
# if(length(highCorr_gol) > 0){dados_num_gol <- dados_num_gol[, -highCorr_gol]}

  ## Padronizar (scale)

dados_pad_gol <- scale(dados_num_gol)

  ## Calcular KMO global

kmo_res_gol <- KMO(dados_pad_gol)
print(paste("KMO Global:", round(kmo_res_gol$MSA, 3)))

  ## Resultado por variavel

kmo_por_var_gol <- kmo_res_gol$MSAi
print(kmo_por_var_gol)
sort(kmo_por_var_gol)

  ## Retirando variaveis com KMO abaixo de 0.5

variaveis_boas_gol <- names(kmo_res_gol$MSAi[kmo_res_gol$MSAi >= 0.5])
dados_pad_gol <- dados_pad_gol[, variaveis_boas_gol]

  ## Recalculando KMO global

kmo_res_filtrado_gol <- KMO(dados_pad_gol)
print(paste("KMO Global após filtro:", round(kmo_res_filtrado_gol$MSA, 3)))

objetos_para_manter <- c("banco_geral", "banco_goleiros", "banco_zagueiros")

rm(list = setdiff(ls(), objetos_para_manter))

# ------------ Banco para zagueiros ------------ #

table(banco_zagueiros$Nation)
summary(banco_zagueiros$Age)

  ## Selecionar so as variaveis numericas (a partir da 8a coluna)

dados_num_zag <- banco_zagueiros[, c(8:117, 119:129)]

  ## Retirando manualmente combinacoes lineares

dados_num_zag <- dados_num_zag %>%
  dplyr::select(-"(Performance) G+A", -"(Performance) PK", -"(Expected) npxG+xAG", -"(Standard) SoT%", -"(Standard) G/Sh", 
                -"(Standard) G/SoT",-"(Expected) npxG/Sh", -"(Expected) G-xG", -"(Expected) np:G-xG", -"(Total) Cmp%", 
                -"(Short) Cmp%", -"(Medium) Cmp%", -"(Long) Cmp%", -"(Expected) Ast", -"(Expected) A-xAG", -"(SCA) SCA",
                -"(GCA) GCA", -"(Tackles) Tkl", -"(Challenges) Tkl%", -"(Challenges) Lost", -"(Blocks) Blocks",
                -"(Blocks) Tkl+Int", -"(Touches) Live", -"(Take-Ons) Succ%", -"(Take-Ons) Tkld%", -"(Aerial Duels) Won%")

  ## Remover variaveis com desvio-padrao zero ou NA

variaveis_validas_zag <- apply(dados_num_zag, 2, sd, na.rm = TRUE) > 0
dados_num_zag <- dados_num_zag[, variaveis_validas_zag]

  ## Detectar combinacoes lineares nos dados

combos <- findLinearCombos(dados_num_zag)

if(!is.null(combos$remove)) {
  message("Variáveis linearmente dependentes detectadas e removidas:")
  print(colnames(dados_num_zag)[combos$remove])
  dados_num_zag <- dados_num_zag[, -combos$remove]}

#   ## Remover variaveis altamente correlacionadas (com base na matriz original)
# 
# cor_mat_zag <- cor(dados_num_zag, use = "pairwise.complete.obs")
# highCorr_zag <- findCorrelation(cor_mat_zag, cutoff = 0.98)
# 
# if(length(highCorr_zag) > 0){dados_num_zag <- dados_num_zag[, -highCorr_zag]}

  ## Padronizar (scale)

dados_pad_zag <- scale(dados_num_zag)

  ## Calcular KMO global

kmo_res_zag <- KMO(dados_pad_zag)
print(paste("KMO Global:", round(kmo_res_zag$MSA, 3)))

  ## Resultado por variavel

kmo_por_var_zag <- kmo_res_zag$MSAi
print(kmo_por_var_zag)
sort(kmo_por_var_zag)

  ## Retirando variaveis com KMO abaixo de 0.5

variaveis_boas_zag <- names(kmo_res_zag$MSAi[kmo_res_zag$MSAi >= 0.5])
dados_pad_zag <- dados_pad_zag[, variaveis_boas_zag]

  ## Recalculando KMO global

kmo_res_filtrado_zag <- KMO(dados_pad_zag)
print(paste("KMO Global após filtro:", round(kmo_res_filtrado_zag$MSA, 3)))

objetos_para_manter <- c("banco_geral", "banco_goleiros", "banco_zagueiros")

rm(list = setdiff(ls(), objetos_para_manter))

# ------------ Banco para laterais ------------ #

banco_laterais <- banco_geral %>% filter(Position %in% c("Left-Back", "Right-Back"))

table(banco_laterais$Nation)
summary(banco_laterais$Age)

  ## Selecionar so as variaveis numericas (a partir da 8a coluna)

dados_num_lat <- banco_laterais[, 8:ncol(banco_laterais)]

  ## Retirando manualmente combinacoes lineares

dados_num_lat <- dados_num_lat %>%
  dplyr::select(-"(Performance) G+A", -"(Performance) PK", -"(Expected) npxG+xAG", -"(Standard) SoT%", -"(Standard) G/Sh", 
                -"(Standard) G/SoT",-"(Expected) npxG/Sh", -"(Expected) G-xG", -"(Expected) np:G-xG", -"(Total) Cmp%", 
                -"(Short) Cmp%", -"(Medium) Cmp%", -"(Long) Cmp%", -"(Expected) Ast", -"(Expected) A-xAG", -"(SCA) SCA",
                -"(GCA) GCA", -"(Tackles) Tkl", -"(Challenges) Tkl%", -"(Challenges) Lost", -"(Blocks) Blocks",
                -"(Blocks) Tkl+Int", -"(Touches) Live", -"(Take-Ons) Succ%", -"(Take-Ons) Tkld%", -"(Aerial Duels) Won%")

  ## Remover variaveis com desvio-padrao zero ou NA

variaveis_validas_lat <- apply(dados_num_lat, 2, sd, na.rm = TRUE) > 0
dados_num_lat <- dados_num_lat[, variaveis_validas_lat]

  ## Detectar combinacoes lineares nos dados

combos <- findLinearCombos(dados_num_lat)

if(!is.null(combos$remove)) {
  message("Variáveis linearmente dependentes detectadas e removidas:")
  print(colnames(dados_num_lat)[combos$remove])
  dados_num_lat <- dados_num_lat[, -combos$remove]}

#   ## Remover variaveis altamente correlacionadas (com base na matriz original)
# 
# cor_mat_lat <- cor(dados_num_lat, use = "pairwise.complete.obs")
# highCorr_lat <- findCorrelation(cor_mat_lat, cutoff = 0.99)
# 
# if(length(highCorr_lat) > 0){dados_num_lat <- dados_num_lat[, -highCorr_lat]}

  ## Padronizar (scale)

dados_pad_lat <- scale(dados_num_lat)

  ## Calcular KMO global

kmo_res_lat <- KMO(dados_pad_lat)
print(paste("KMO Global:", round(kmo_res_lat$MSA, 3)))

  ## Resultado por variavel

kmo_por_var_lat <- kmo_res_lat$MSAi
print(kmo_por_var_lat)
sort(kmo_por_var_lat)

  ## Retirando variaveis com KMO abaixo de 0.5

variaveis_boas_lat <- names(kmo_res_lat$MSAi[kmo_res_lat$MSAi >= 0.5])
dados_pad_lat <- dados_pad_lat[, variaveis_boas_lat]

  ## Recalculando KMO global

kmo_res_filtrado_lat <- KMO(dados_pad_lat)
print(paste("KMO Global após filtro:", round(kmo_res_filtrado_lat$MSA, 3)))

objetos_para_manter <- c("banco_geral", "banco_goleiros", "banco_zagueiros", "banco_laterais")

rm(list = setdiff(ls(), objetos_para_manter))

# ------------ Banco para volantes ------------ #

banco_volantes <- banco_geral %>% filter(Position %in% c("Defensive Midfield", "Central Midfield"))

table(banco_volantes$Nation)
summary(banco_volantes$Age)

  ## Selecionar so as variaveis numericas (a partir da 8a coluna)

dados_num_vol <- banco_volantes[, 8:ncol(banco_volantes)]

  ## Retirando manualmente combinacoes lineares

dados_num_vol <- dados_num_vol %>%
  dplyr::select(-"(Performance) G+A", -"(Performance) PK", -"(Expected) npxG+xAG", -"(Standard) SoT%", -"(Standard) G/Sh", 
                -"(Standard) G/SoT",-"(Expected) npxG/Sh", -"(Expected) G-xG", -"(Expected) np:G-xG", -"(Total) Cmp%", 
                -"(Short) Cmp%", -"(Medium) Cmp%", -"(Long) Cmp%", -"(Expected) Ast", -"(Expected) A-xAG", -"(SCA) SCA",
                -"(GCA) GCA", -"(Tackles) Tkl", -"(Challenges) Tkl%", -"(Challenges) Lost", -"(Blocks) Blocks",
                -"(Blocks) Tkl+Int", -"(Touches) Live", -"(Take-Ons) Succ%", -"(Take-Ons) Tkld%", -"(Aerial Duels) Won%")

  ## Remover variaveis com desvio-padrao zero ou NA

variaveis_validas_vol <- apply(dados_num_vol, 2, sd, na.rm = TRUE) > 0
dados_num_vol <- dados_num_vol[, variaveis_validas_vol]

  ## Detectar combinacoes lineares nos dados

combos <- findLinearCombos(dados_num_vol)

if(!is.null(combos$remove)) {
  message("Variáveis linearmente dependentes detectadas e removidas:")
  print(colnames(dados_num_vol)[combos$remove])
  dados_num_vol <- dados_num_vol[, -combos$remove]}

#   ## Remover variaveis altamente correlacionadas (com base na matriz original)
# 
# cor_mat_vol <- cor(dados_num_vol, use = "pairwise.complete.obs")
# highCorr_vol <- findCorrelation(cor_mat_vol, cutoff = 0.99)
# 
# if(length(highCorr_vol) > 0){dados_num_vol <- dados_num_vol[, -highCorr_vol]}

  ## Padronizar (scale)

dados_pad_vol <- scale(dados_num_vol)

  ## Calcular KMO global

kmo_res_vol <- KMO(dados_pad_vol)
print(paste("KMO Global:", round(kmo_res_vol$MSA, 3)))

  ## Resultado por variavel

kmo_por_var_vol <- kmo_res_vol$MSAi
print(kmo_por_var_vol)
sort(kmo_por_var_vol)

  ## Retirando variaveis com KMO abaixo de 0.5

variaveis_boas_vol <- names(kmo_res_vol$MSAi[kmo_res_vol$MSAi >= 0.5])
dados_pad_vol <- dados_pad_vol[, variaveis_boas_vol]

  ## Recalculando KMO global

kmo_res_filtrado_vol <- KMO(dados_pad_vol)
print(paste("KMO Global após filtro:", round(kmo_res_filtrado_vol$MSA, 3)))

objetos_para_manter <- c("banco_geral", "banco_goleiros", "banco_zagueiros", "banco_laterais", "banco_volantes")

rm(list = setdiff(ls(), objetos_para_manter))

# ------------ Banco para meias ------------ #

banco_meias <- banco_geral %>% filter(Position %in% c("Attacking Midfield", "Left Midfield"))

table(banco_meias$Nation)
summary(banco_meias$Age)

  ## Selecionar so as variaveis numericas (a partir da 8a coluna)

dados_num_mei <- banco_meias[, 8:ncol(banco_meias)]

  ## Retirando manualmente combinacoes lineares

dados_num_mei <- dados_num_mei %>%
  dplyr::select(-"(Performance) G+A", -"(Performance) PK", -"(Expected) npxG+xAG", -"(Standard) SoT%", -"(Standard) G/Sh", 
                -"(Standard) G/SoT",-"(Expected) npxG/Sh", -"(Expected) G-xG", -"(Expected) np:G-xG", -"(Total) Cmp%", 
                -"(Short) Cmp%", -"(Medium) Cmp%", -"(Long) Cmp%", -"(Expected) Ast", -"(Expected) A-xAG", -"(SCA) SCA",
                -"(GCA) GCA", -"(Tackles) Tkl", -"(Challenges) Tkl%", -"(Challenges) Lost", -"(Blocks) Blocks",
                -"(Blocks) Tkl+Int", -"(Touches) Live", -"(Take-Ons) Succ%", -"(Take-Ons) Tkld%", -"(Aerial Duels) Won%")

  ## Remover variaveis com desvio-padrao zero ou NA

variaveis_validas_mei <- apply(dados_num_mei, 2, sd, na.rm = TRUE) > 0
dados_num_mei <- dados_num_mei[, variaveis_validas_mei]

  ## Detectar combinacoes lineares nos dados

combos <- findLinearCombos(dados_num_mei)

if(!is.null(combos$remove)) {
  message("Variáveis linearmente dependentes detectadas e removidas:")
  print(colnames(dados_num_mei)[combos$remove])
  dados_num_mei <- dados_num_mei[, -combos$remove]}

  ## Remover variaveis altamente correlacionadas (com base na matriz original)

cor_mat_mei <- cor(dados_num_mei, use = "pairwise.complete.obs")
highCorr_mei <- findCorrelation(cor_mat_mei, cutoff = 0.99)

if(length(highCorr_mei) > 0){dados_num_mei <- dados_num_mei[, -highCorr_mei]}

  ## Padronizar (scale)

dados_pad_mei <- scale(dados_num_mei)

  ## Calcular KMO global

kmo_res_mei <- KMO(dados_pad_mei)
print(paste("KMO Global:", round(kmo_res_mei$MSA, 3)))

  ## Resultado por variavel

kmo_por_var_mei <- kmo_res_mei$MSAi
print(kmo_por_var_mei)
sort(kmo_por_var_mei)

  ## Retirando variaveis com KMO abaixo de 0.5

variaveis_boas_mei <- names(kmo_res_mei$MSAi[kmo_res_mei$MSAi >= 0.5])
dados_pad_mei <- dados_pad_mei[, variaveis_boas_mei]

  ## Recalculando KMO global

kmo_res_filtrado_mei <- KMO(dados_pad_mei)
print(paste("KMO Global após filtro:", round(kmo_res_filtrado_mei$MSA, 3)))

objetos_para_manter <- c("banco_geral", "banco_goleiros", "banco_zagueiros", "banco_laterais", "banco_volantes", "banco_meias")

rm(list = setdiff(ls(), objetos_para_manter))

# ------------ Banco para pontas ------------ #

banco_pontas <- banco_geral %>% filter(Position %in% c("Left Winger", "Right Winger"))

table(banco_pontas$Nation)
summary(banco_pontas$Age)

  ## Selecionar so as variaveis numericas (a partir da 8a coluna)

dados_num_pon <- banco_pontas[, 8:ncol(banco_pontas)]

  ## Retirando manualmente combinacoes lineares

dados_num_pon <- dados_num_pon %>%
  dplyr::select(-"(Performance) G+A", -"(Performance) PK", -"(Expected) npxG+xAG", -"(Standard) SoT%", -"(Standard) G/Sh", 
                -"(Standard) G/SoT",-"(Expected) npxG/Sh", -"(Expected) G-xG", -"(Expected) np:G-xG", -"(Total) Cmp%", 
                -"(Short) Cmp%", -"(Medium) Cmp%", -"(Long) Cmp%", -"(Expected) Ast", -"(Expected) A-xAG", -"(SCA) SCA",
                -"(GCA) GCA", -"(Tackles) Tkl", -"(Challenges) Tkl%", -"(Challenges) Lost", -"(Blocks) Blocks",
                -"(Blocks) Tkl+Int", -"(Touches) Live", -"(Take-Ons) Succ%", -"(Take-Ons) Tkld%", -"(Aerial Duels) Won%")

  ## Remover variaveis com desvio-padrao zero ou NA

variaveis_validas_pon <- apply(dados_num_pon, 2, sd, na.rm = TRUE) > 0
dados_num_pon <- dados_num_pon[, variaveis_validas_pon]

  ## Detectar combinacoes lineares nos dados

combos <- findLinearCombos(dados_num_pon)

if(!is.null(combos$remove)) {
  message("Variáveis linearmente dependentes detectadas e removidas:")
  print(colnames(dados_num_pon)[combos$remove])
  dados_num_pon <- dados_num_pon[, -combos$remove]}

#   ## Remover variaveis altamente correlacionadas (com base na matriz original)
# 
# cor_mat_pon <- cor(dados_num_pon, use = "pairwise.complete.obs")
# highCorr_pon <- findCorrelation(cor_mat_pon, cutoff = 0.99)
# 
# if(length(highCorr_pon) > 0){dados_num_pon <- dados_num_pon[, -highCorr_pon]}

  ## Padronizar (scale)

dados_pad_pon <- scale(dados_num_pon)

  ## Calcular KMO global

kmo_res_pon <- KMO(dados_pad_pon)
print(paste("KMO Global:", round(kmo_res_pon$MSA, 3)))

  ## Resultado por variavel

kmo_por_var_pon <- kmo_res_pon$MSAi
print(kmo_por_var_pon)
sort(kmo_por_var_pon)

  ## Retirando variaveis com KMO abaixo de 0.5

variaveis_boas_pon <- names(kmo_res_pon$MSAi[kmo_res_pon$MSAi >= 0.5])
dados_pad_pon <- dados_pad_pon[, variaveis_boas_pon]

  ## Recalculando KMO global

kmo_res_filtrado_pon <- KMO(dados_pad_pon)
print(paste("KMO Global após filtro:", round(kmo_res_filtrado_pon$MSA, 3)))

objetos_para_manter <- c("banco_geral", "banco_goleiros", "banco_zagueiros", "banco_laterais", "banco_volantes", 
                         "banco_meias", "banco_pontas")

rm(list = setdiff(ls(), objetos_para_manter))

# ------------ Banco para atacantes ------------ #

banco_atacantes <- banco_geral %>% filter(Position %in% c("Centre-Forward", "Striker", "Second Striker"))

table(banco_atacantes$Nation)
summary(banco_atacantes$Age)

  ## Selecionar so as variaveis numericas (a partir da 8a coluna)

dados_num_ata <- banco_atacantes[, 8:ncol(banco_atacantes)]

  ## Retirando manualmente combinacoes lineares

dados_num_ata <- dados_num_ata %>%
  dplyr::select(-"(Performance) G+A", -"(Performance) PK", -"(Expected) npxG+xAG", -"(Standard) SoT%", -"(Standard) G/Sh", 
                -"(Standard) G/SoT",-"(Expected) npxG/Sh", -"(Expected) G-xG", -"(Expected) np:G-xG", -"(Total) Cmp%", 
                -"(Short) Cmp%", -"(Medium) Cmp%", -"(Long) Cmp%", -"(Expected) Ast", -"(Expected) A-xAG", -"(SCA) SCA",
                -"(GCA) GCA", -"(Tackles) Tkl", -"(Challenges) Tkl%", -"(Challenges) Lost", -"(Blocks) Blocks",
                -"(Blocks) Tkl+Int", -"(Touches) Live", -"(Take-Ons) Succ%", -"(Take-Ons) Tkld%", -"(Aerial Duels) Won%")

  ## Remover variaveis com desvio-padrao zero ou NA

variaveis_validas_ata <- apply(dados_num_ata, 2, sd, na.rm = TRUE) > 0
dados_num_ata <- dados_num_ata[, variaveis_validas_ata]

  ## Detectar combinacoes lineares nos dados

combos <- findLinearCombos(dados_num_ata)

if(!is.null(combos$remove)) {
  message("Variáveis linearmente dependentes detectadas e removidas:")
  print(colnames(dados_num_ata)[combos$remove])
  dados_num_ata <- dados_num_ata[, -combos$remove]}

  ## Remover variaveis altamente correlacionadas (com base na matriz original)

cor_mat_ata <- cor(dados_num_ata, use = "pairwise.complete.obs")
highCorr_ata <- findCorrelation(cor_mat_ata, cutoff = 0.99)

if(length(highCorr_ata) > 0){dados_num_ata <- dados_num_ata[, -highCorr_ata]}

  ## Padronizar (scale)

dados_pad_ata <- scale(dados_num_ata)

  ## Calcular KMO global

kmo_res_ata <- KMO(dados_pad_ata)
print(paste("KMO Global:", round(kmo_res_ata$MSA, 3)))

  ## Resultado por variavel

kmo_por_var_ata <- kmo_res_ata$MSAi
print(kmo_por_var_ata)
sort(kmo_por_var_ata)

  ## Retirando variaveis com KMO abaixo de 0.5

variaveis_boas_ata <- names(kmo_res_ata$MSAi[kmo_res_ata$MSAi >= 0.5])
dados_pad_ata <- dados_pad_ata[, variaveis_boas_ata]

  ## Recalculando KMO global

kmo_res_filtrado_ata <- KMO(dados_pad_ata)
print(paste("KMO Global após filtro:", round(kmo_res_filtrado_ata$MSA, 3)))

objetos_para_manter <- c("banco_geral", "banco_goleiros", "banco_zagueiros", "banco_laterais", "banco_volantes", 
                         "banco_meias", "banco_pontas", "banco_atacantes")

rm(list = setdiff(ls(), objetos_para_manter))

