########## ORGANIZANDO BANCO DE DADOS ########## 

library(readxl)
library(dplyr)
library(stringr)
library(writexl)

# Estatisticas padrao

est_padrao <- read_excel("FBREF_BR_2024.xlsx", sheet = 1)
colnames(est_padrao) <- c(
  "Rk", "Player", "Nation", "Pos", "Squad", "Age", "Born",
  "(Playing Time) MP", "(Playing Time) Starts", "(Playing Time) Min", "(Playing Time) 90s", 
  "(Performance) Gls", "(Performance) Ast", "(Performance) G+A", "(Performance) G-PK", 
  "(Performance) PK", "(Performance) PKatt", "(Performance) CrdY", "(Performance) CrdR", 
  "(Expected) xG", "(Expected) npxG", "(Expected) xAG", "(Expected) npxG+xAG", 
  "(Progression) PrgC", "(Progression) PrgP", "(Progression) PrgR",
  "(Per 90 Minutes) Gls", "(Per 90 Minutes) Ast", "(Per 90 Minutes) G+A", "(Per 90 Minutes) G-PK", "(Per 90 Minutes) G+A-PK", 
  "(Per 90 Minutes) xG", "(Per 90 Minutes) xAG", "(Per 90 Minutes) xG+xAG", "(Per 90 Minutes) npxG", "(Per 90 Minutes) npxG+xAG", 
  "Partidas")

# Chutes

chutes <- read_excel("FBREF_BR_2024.xlsx", sheet = 4)
colnames(chutes) <- c(
  "Rk", "Player", "Nation", "Pos", "Squad", "Age", "Born", "90s", 
  "(Standard) Gls", "(Standard) Sh", "(Standard) SoT", "(Standard) SoT%", 
  "(Standard) Sh/90", "(Standard) SoT/90", "(Standard) G/Sh", "(Standard) G/SoT",
  "(Standard) Dist", "(Standard) FK", "(Standard) PK", "(Standard) PKatt", 
  "(Expected) xG", "(Expected) npxG", "(Expected) npxG/Sh", "(Expected) G-xG", "(Expected) np:G-xG", 
  "Partidas")

# Passes

passes <- read_excel("FBREF_BR_2024.xlsx", sheet = 5)
colnames(passes) <- c(
  "Rk", "Player", "Nation", "Pos", "Squad", "Age", "Born", "90s", 
  "(Total) Cmp", "(Total) Att", "(Total) Cmp%", "(Total) TotDist", "(Total) PrgDist",
  "(Short) Cmp", "(Short) Att", "(Short) Cmp%", 
  "(Medium) Cmp", "(Medium) Att", "(Medium) Cmp%", 
  "(Long) Cmp", "(Long) Att", "(Long) Cmp%",
  "(Expected) Ast", "(Expected) xAG", "(Expected) xA", "(Expected) A-xAG", "(Expected) KP", 
  "(Expected) 1/3", "(Expected) PPA", "(Expected) CrsPA", "(Expected) PrgP",
  "Partidas")

# Tipos de passe

tipos_passe <- read_excel("FBREF_BR_2024.xlsx", sheet = 6)
colnames(tipos_passe) <- c(
  "Rk", "Player", "Nation", "Pos", "Squad", "Age", "Born", "90s", 
  "(Pass Types) Att", "(Pass Types) Live", "(Pass Types) Dead", "(Pass Types) FK", 
  "(Pass Types) TB", "(Pass Types) Sw", "(Pass Types) Crs", "(Pass Types) TI", "(Pass Types) CK",
  "(Corner Kicks) In", "(Corner Kicks) Out", "(Corner Kicks) Str", 
  "(Outcomes) Cmp", "(Outcomes) Off", "(Outcomes) Blocks", 
  "Partidas")

# Gols e criacao de chutes

criacao <- read_excel("FBREF_BR_2024.xlsx", sheet = 7)
colnames(criacao) <- c(
  "Rk", "Player", "Nation", "Pos", "Squad", "Age", "Born", "90s", 
  "(SCA) SCA", "(SCA) SCA90", 
  "(SCA Types) PassLive", "(SCA Types) PassDead", "(SCA Types) TO", 
  "(SCA Types) Sh", "(SCA Types) Fld", "(SCA Types) Def", 
  "(GCA) GCA", "(GCA) GCA90", 
  "(GCA Types) PassLive", "(GCA Types) PassDead", "(GCA Types) TO", 
  "(GCA Types) Sh", "(GCA Types) Fld", "(GCA Types) Def", 
  "Partidas")

# Acoes defensivas 

defesa <- read_excel("FBREF_BR_2024.xlsx", sheet = 8)
colnames(defesa) <- c(
  "Rk", "Player", "Nation", "Pos", "Squad", "Age", "Born", "90s", 
  "(Tackles) Tkl", "(Tackles) TklW", "(Tackles) Def 3rd", "(Tackles) Mid 3rd", "(Tackles) Att 3rd",
  "(Challenges) Tkl", "(Challenges) Att", "(Challenges) Tkl%", "(Challenges) Lost", 
  "(Blocks) Blocks", "(Blocks) Sh", "(Blocks) Pass", 
  "(Blocks) Int", "(Blocks) Tkl+Int", "(Blocks) Clr", "(Blocks) Err", 
  "Partidas")

# Posse

posse <- read_excel("FBREF_BR_2024.xlsx", sheet = 9)
colnames(posse) <- c(
  "Rk", "Player", "Nation", "Pos", "Squad", "Age", "Born", "90s", 
  "(Touches) Touches", "(Touches) Def Pen", "(Touches) Def 3rd", "(Touches) Mid 3rd", 
  "(Touches) Att 3rd", "(Touches) Att Pen", "(Touches) Live", 
  "(Take-Ons) Att", "(Take-Ons) Succ", "(Take-Ons) Succ%", "(Take-Ons) Tkld", "(Take-Ons) Tkld%", 
  "(Carries) Carries", "(Carries) TotDist", "(Carries) PrgDist", "(Carries) PrgC", 
  "(Carries) 1/3", "(Carries) CPA", "(Carries) Mis", "(Carries) Dis",
  "(Receiving) Rec", "(Receiving) PrgR", 
  "Partidas")

# Tempo de jogo

tempo <- read_excel("FBREF_BR_2024.xlsx", sheet = 10)
colnames(tempo) <- c(
  "Rk", "Player", "Nation", "Pos", "Squad", "Age", "Born",
  "(Playing Time) MP", "(Playing Time) Min", "(Playing Time) Mn/MP", "(Playing Time) Min%", "(Playing Time) 90s", 
  "(Starts) Starts", "(Starts) Mn/Start", "(Starts) Compl",
  "(Subs) Subs", "(Subs) Mn/Sub", "(Subs) unSub", 
  "(Team Success) PPM", "(Team Success) onG", "(Team Success) onGA", "(Team Success) +/-", "(Team Success) +/-90", "(Team Success) On-Off", 
  "(Team Success xG) onxG", "(Team Success xG) onxGA", "(Team Success xG) xG+/-", "(Team Success xG) xG+/-90", "(Team Success xG) On-Off",
  "Partidas")

  ## Filtrando para ter os mesmos 728 jogadores

tempo <- tempo %>% semi_join(est_padrao, by = c("Player", "Nation", "Squad", "Age"))
tempo <- tempo[, -1]

# Estatisticas variadas

est_variadas <- read_excel("FBREF_BR_2024.xlsx", sheet = 11)
colnames(est_variadas) <- c(
  "Rk", "Player", "Nation", "Pos", "Squad", "Age", "Born", "90s", 
  "(Performance) CrtsY", "(Performance) CrtR", "(Performance) 2CrdY", "(Performance) Fls", "(Performance) Fld", 
  "(Performance) Off", "(Performance) Crs", "(Performance) Int", "(Performance) TklW", 
  "(Performance) PKwon", "(Performance) PKcon", "(Performance) OG", "(Performance) Recov",
  "(Aerial Duels) Won", "(Aerial Duels) Lost", "(Aerial Duels) Won%", 
  "Partidas")

######## Juntando os bancos ######## 

# est_padrao com chutes -> banco1

banco1 <- full_join(est_padrao, chutes, by = c("Rk", "Player", "Nation", "Pos", "Squad", "Age", "Born"))
banco1 <- banco1[, c(-37, -38, -39, -49, -50, -51, -52, -56)]
banco1 <- banco1 %>% rename("(Expected) xG" = "(Expected) xG.x", "(Expected) npxG" = "(Expected) npxG.x")

# banco1 com passes -> banco2 

banco2 <- full_join(banco1, passes, by = c("Rk", "Player", "Nation", "Pos", "Squad", "Age", "Born"))
banco2 <- banco2[, c(-49, -65, -72, -73)]
banco2 <- banco2 %>% rename("(Expected) xAG" = "(Expected) xAG.x")

# banco2 com tipos_passe -> banco3

banco3 <- full_join(banco2, tipos_passe, by = c("Rk", "Player", "Nation", "Pos", "Squad", "Age", "Born"))
banco3 <- banco3[, c(-70, -71, -86)]

# banco3 com criacao -> banco4

banco4 <- full_join(banco3, criacao, by = c("Rk", "Player", "Nation", "Pos", "Squad", "Age", "Born"))
banco4 <- banco4[, c(-84, -101)]

# banco4 com defesa -> banco5

banco5 <- full_join(banco4, defesa, by = c("Rk", "Player", "Nation", "Pos", "Squad", "Age", "Born"))
banco5 <- banco5[, c(-100, -117)]

# banco5 com posse -> banco6

banco6 <- full_join(banco5, posse, by = c("Rk", "Player", "Nation", "Pos", "Squad", "Age", "Born"))
banco6 <- banco6[, c(-1, -116, -132, -138, -139)]

# banco6 com tempo -> banco7

banco7 <- full_join(banco6, tempo, by = c("Player", "Nation", "Pos", "Squad", "Age", "Born"))
banco7 <- banco7[, c(-135, -136, -139, -140, -157)]
banco7 <- banco7 %>% rename("(Playing Time) MP" = "(Playing Time) MP.x" , "(Playing Time) Min" = "(Playing Time) Min.x", 
                            "(Playing Time) 90s" = "(Playing Time) 90s.x")

# banco7 com est_variadas -> banco8 -> banco_geral

est_variadas <- est_variadas[, -1]
banco8 <- full_join(banco7, est_variadas, by = c("Player", "Nation", "Pos", "Squad", "Age", "Born"))
banco_geral <- banco8[, c(-153, -154, -155, -160, -161, -162, -170)]

rm(list = setdiff(ls(), "banco_geral"))

write_xlsx(banco_geral, "banco_geral.xlsx")



                            




