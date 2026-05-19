########## TRATAMENTO DE TRANSFERENCIAS ########## 

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(stringi)
library(writexl)

# Bancos

#D:/Henrique Nince/OneDrive/Área de Trabalho/UnB/TCC/Análises/Tratamento/2. Tratamento da posição/
  
banco_geral <- read_excel("banco_tratado_pos.xlsx", sheet = 1)

banco_goleiros <- read_excel("banco_goleiros.xlsx", sheet = 1)
banco_goleiros <- banco_goleiros %>% mutate(Player = toupper(stri_trans_general(Player, "Latin-ASCII")))

resto_goleiros <- read_excel("resto_goleiros.xlsx", sheet = 1)
resto_goleiros <- resto_goleiros %>% mutate(Player = toupper(stri_trans_general(Player, "Latin-ASCII")))

# Para geral

cols_orig <- c(
  "Player",
  "Nation",
  "Position",
  "Squad",
  "Age",
  "Born",
  "(Playing Time) MP",
  "(Playing Time) Starts",
  "(Playing Time) Min",
  "(Playing Time) 90s",
  "(Performance) Gls",
  "(Performance) Ast",
  "(Performance) G+A",
  "(Performance) G-PK",
  "(Performance) PK",
  "(Performance) PKatt",
  "(Performance) CrdY",
  "(Performance) CrdR",
  "(Expected) xG",
  "(Expected) npxG",
  "(Expected) xAG",
  "(Expected) npxG+xAG",
  "(Progression) PrgC",
  "(Progression) PrgP",
  "(Progression) PrgR",
  "(Per 90 Minutes) Gls",
  "(Per 90 Minutes) Ast",
  "(Per 90 Minutes) G+A",
  "(Per 90 Minutes) G-PK",
  "(Per 90 Minutes) G+A-PK",
  "(Per 90 Minutes) xG",
  "(Per 90 Minutes) xAG",
  "(Per 90 Minutes) xG+xAG",
  "(Per 90 Minutes) npxG",
  "(Per 90 Minutes) npxG+xAG",
  "(Standard) Sh",
  "(Standard) SoT",
  "(Standard) SoT%",
  "(Standard) Sh/90",
  "(Standard) SoT/90",
  "(Standard) G/Sh",
  "(Standard) G/SoT",
  "(Standard) Dist",
  "(Standard) FK",
  "(Expected) npxG/Sh",
  "(Expected) G-xG",
  "(Expected) np:G-xG",
  "(Total) Cmp",
  "(Total) Att",
  "(Total) Cmp%",
  "(Total) TotDist",
  "(Total) PrgDist",
  "(Short) Cmp",
  "(Short) Att",
  "(Short) Cmp%",
  "(Medium) Cmp",
  "(Medium) Att",
  "(Medium) Cmp%",
  "(Long) Cmp",
  "(Long) Att",
  "(Long) Cmp%",
  "(Expected) Ast",
  "(Expected) xA",
  "(Expected) A-xAG",
  "(Expected) KP",
  "(Expected) 1/3",
  "(Expected) PPA",
  "(Expected) CrsPA",
  "(Pass Types) Live",
  "(Pass Types) Dead",
  "(Pass Types) FK",
  "(Pass Types) TB",
  "(Pass Types) Sw",
  "(Pass Types) Crs",
  "(Pass Types) TI",
  "(Pass Types) CK",
  "(Corner Kicks) In",
  "(Corner Kicks) Out",
  "(Corner Kicks) Str",
  "(Outcomes) Cmp",
  "(Outcomes) Off",
  "(Outcomes) Blocks",
  "(SCA) SCA",
  "(SCA) SCA90",
  "(SCA Types) PassLive",
  "(SCA Types) PassDead",
  "(SCA Types) TO",
  "(SCA Types) Sh",
  "(SCA Types) Fld",
  "(SCA Types) Def",
  "(GCA) GCA",
  "(GCA) GCA90",
  "(GCA Types) PassLive",
  "(GCA Types) PassDead",
  "(GCA Types) TO",
  "(GCA Types) Sh",
  "(GCA Types) Fld",
  "(GCA Types) Def",
  "(Tackles) Tkl",
  "(Tackles) TklW",
  "(Tackles) Def 3rd",
  "(Tackles) Mid 3rd",
  "(Tackles) Att 3rd",
  "(Challenges) Tkl",
  "(Challenges) Att",
  "(Challenges) Tkl%",
  "(Challenges) Lost",
  "(Blocks) Blocks",
  "(Blocks) Sh",
  "(Blocks) Pass",
  "(Blocks) Int",
  "(Blocks) Tkl+Int",
  "(Blocks) Clr",
  "(Blocks) Err",
  "(Touches) Touches",
  "(Touches) Def Pen",
  "(Touches) Def 3rd",
  "(Touches) Mid 3rd",
  "(Touches) Att 3rd",
  "(Touches) Att Pen",
  "(Touches) Live",
  "(Take-Ons) Att",
  "(Take-Ons) Succ",
  "(Take-Ons) Succ%",
  "(Take-Ons) Tkld",
  "(Take-Ons) Tkld%",
  "(Carries) Carries",
  "(Carries) TotDist",
  "(Carries) PrgDist",
  "(Carries) 1/3",
  "(Carries) CPA",
  "(Carries) Mis",
  "(Carries) Dis",
  "(Receiving) Rec",
  "(Playing Time) Mn/MP",
  "(Playing Time) Min%",
  "(Starts) Mn/Start",
  "(Starts) Compl",
  "(Subs) Subs",
  "(Subs) Mn/Sub",
  "(Subs) unSub",
  "(Team Success) PPM",
  "(Team Success) onG",
  "(Team Success) onGA",
  "(Team Success) +/-",
  "(Team Success) +/-90",
  "(Team Success) On-Off",
  "(Team Success xG) onxG",
  "(Team Success xG) onxGA",
  "(Team Success xG) xG+/-",
  "(Team Success xG) xG+/-90",
  "(Team Success xG) On-Off",
  "(Performance) 2CrdY",
  "(Performance) Fls",
  "(Performance) Fld",
  "(Performance) Off",
  "(Performance) PKwon",
  "(Performance) PKcon",
  "(Performance) OG",
  "(Performance) Recov",
  "(Aerial Duels) Won",
  "(Aerial Duels) Lost",
  "(Aerial Duels) Won%"
)

keys_duplicadas <- banco_geral %>%
  count(Player, Nation, Age) %>%
  filter(n > 1) %>%
  select(-n)

banco_geral_duplos <- banco_geral %>%
  inner_join(keys_duplicadas, by = c("Player", "Nation", "Age")) %>%
  select(any_of(cols_orig)) %>%
  arrange(Player, Age, Nation, Squad)

## Retirando jogadores que de fato são diferentes

banco_geral_duplos <- banco_geral_duplos[c(-11, -12, -19, -20, -21, -22, -41, -42, -43, -44, -45, -46), ]

## Separando os bancos

banco_geral <- anti_join(banco_geral, banco_geral_duplos)

write_xlsx(banco_geral, "banco_sem_transf.xlsx")
write_xlsx(banco_geral_duplos, "banco_transf.xlsx")

# Para goleiros

  ## Juntando as duas bases

variaveis_goleiros <- c("Player", "Nation", "Pos", "Squad", "Age", 
                         "(Performance) CrdY", "(Performance) CrdR",
                         "(Team Success) onGA", "(Team Success) On-Off",
                         "(Performance) 2CrdY")

resto_goleiros <- resto_goleiros %>% select(Player, all_of(variaveis_goleiros))

banco_goleiros <- full_join(banco_goleiros, resto_goleiros, by = c("Player", "Nation", "Pos", "Squad", "Age"))

cols_orig <- c(
  "Player","Nation","Pos","Squad","Age","(Playing Time) MP","(Playing Time) Starts","(Playing Time) Min",
  "(Playing Time) 90s","(Performance) GA","(Performance) GA90","(Performance) SoTA","(Performance) Saves",
  "(Performance) Save%","(Performance) FK","(Performance) CK","(Performance) OG","(Performance) W","(Performance) D",
  "(Performance) L","(Performance) CS","(Performance) CS%","(Penalty Kicks) PKatt","(Penalty Kicks) PKA",
  "(Penalty Kicks) PKsv","(Penalty Kicks) PKm","(Penalty Kicks) Save%","(Expected) PSxG","(Expected) PSxG/SoT",
  "(Expected) PSxG+/-","(Expected) PSxG+/-/90","(Launched) Cmp","(Launched) Att","(Launched) Cmp%",
  "(Passes) Att (GK)","(Passes) Thr","(Passes) Launch%","(Passes) AvgLen","(Goal Kicks) Att",
  "(Goal Kicks) Launch%","(Goal Kicks) AvgLen","(Crosses) Opp","(Crosses) Stp","(Crosses) Stp%",
  "(Sweeper) #OPA","(Sweeper) #OPA/90","(Sweeper) AvdDist","(Performance) CrdY","(Performance) CrdR",
  "(Team Success) onGA","(Team Success) On-Off","(Performance) 2CrdY")

keys_duplicadas <- banco_goleiros %>%
  count(Player, Nation, Age) %>%
  filter(n > 1) %>%
  select(-n)

banco_goleiros_duplos <- banco_goleiros %>%
  inner_join(keys_duplicadas, by = c("Player", "Nation", "Age")) %>%
  select(any_of(cols_orig)) %>%
  arrange(Player, Age, Nation, Squad)

## Separando os bancos

banco_goleiros <- anti_join(banco_goleiros, banco_goleiros_duplos)

write_xlsx(banco_goleiros, "banco_goleiros_sem_transf.xlsx")
write_xlsx(banco_goleiros_duplos, "banco_goleiros_transf.xlsx")









