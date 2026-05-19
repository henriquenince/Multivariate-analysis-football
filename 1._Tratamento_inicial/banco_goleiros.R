########## ORGANIZANDO BANCO DE DADOS ########## 

library(readxl)
library(dplyr)
library(writexl)
library(writexl)

# Goleiro

goleiro <- read_excel("FBREF_BR_2024.xlsx", sheet = 2)
colnames(goleiro) <- c(
  "Rk", "Player", "Nation", "Pos", "Squad", "Age", "Born",
  "(Playing Time) MP", "(Playing Time) Starts", "(Playing Time) Min", "(Playing Time) 90s", 
  "(Performance) GA", "(Performance) GA90", "(Performance) SoTA", "(Performance) Saves", "(Performance) Save%",
  "(Performance) W", "(Performance) D", "(Performance) L", "(Performance) CS", "(Performance) CS%", 
  "(Penalty Kicks) PKatt", "(Penalty Kicks) PKA", "(Penalty Kicks) PKsv", "(Penalty Kicks) PKm", "(Penalty Kicks) Save%", 
  "Partidas")

# Goleiro avançado

gol_avancado <- read_excel("FBREF_BR_2024.xlsx", sheet = 3)
colnames(gol_avancado) <- c(
  "Rk", "Player", "Nation", "Pos", "Squad", "Age", "Born", "90s", 
  "(Goals) GA", "(Goals) PKA", "(Goals) FK", "(Goals) CK", "(Goals) OG", 
  "(Expected) PSxG", "(Expected) PSxG/SoT", "(Expected) PSxG+/-", "(Expected) PSxG+/-/90",
  "(Launched) Cmp", "(Launched) Att", "(Launched) Cmp%", 
  "(Passes) Att (GK)", "(Passes) Thr", "(Passes) Launch%", "(Passes) AvgLen", 
  "(Goal Kicks) Att", "(Goal Kicks) Launch%", "(Goal Kicks) AvgLen", 
  "(Crosses) Opp", "(Crosses) Stp", "(Crosses) Stp%",
  "(Sweeper) #OPA", "(Sweeper) #OPA/90", "(Sweeper) AvdDist", 
  "Partidas")

######## Juntando os bancos ######## 

banco_goleiros <- full_join(goleiro, gol_avancado, by = c("Rk", "Player", "Nation", "Pos", "Squad", "Age", "Born"))

# Selecionando as variaveis, renomeando e reordenando

variaveis_desejadas <- c(
  "Player", "Nation", "Pos", "Squad", "Age",
  "(Playing Time) MP", "(Playing Time) Starts", "(Playing Time) Min", "(Playing Time) 90s", 
  "(Performance) GA", "(Performance) GA90", "(Performance) SoTA", "(Performance) Saves", "(Performance) Save%",
  "(Performance) W", "(Performance) D", "(Performance) L", "(Performance) CS", "(Performance) CS%",
  "(Penalty Kicks) PKatt", "(Penalty Kicks) PKA", "(Penalty Kicks) PKsv", "(Penalty Kicks) PKm", "(Penalty Kicks) Save%", 
  "(Goals) FK", "(Goals) CK", "(Goals) OG", 
  "(Expected) PSxG", "(Expected) PSxG/SoT", "(Expected) PSxG+/-", "(Expected) PSxG+/-/90",
  "(Launched) Cmp", "(Launched) Att", "(Launched) Cmp%", 
  "(Passes) Att (GK)", "(Passes) Thr", "(Passes) Launch%", "(Passes) AvgLen", 
  "(Goal Kicks) Att", "(Goal Kicks) Launch%", "(Goal Kicks) AvgLen", 
  "(Crosses) Opp", "(Crosses) Stp", "(Crosses) Stp%",
  "(Sweeper) #OPA", "(Sweeper) #OPA/90", "(Sweeper) AvdDist")

banco_goleiros <- banco_goleiros %>% select(all_of(variaveis_desejadas)) %>%
  rename("(Performance) FK" = "(Goals) FK",
    "(Performance) CK" = "(Goals) CK",
    "(Performance) OG" = "(Goals) OG") %>%
  relocate(`(Performance) FK`, `(Performance) CK`, `(Performance) OG`,
    .after = `(Performance) Save%`)

write_xlsx(banco_goleiros, "banco_goleiros.xlsx")


















