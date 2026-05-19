########## TRATAMENTO DA POSICAO ########## 

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(stringi)
library(writexl)

# Bancos
  
  ## Banco geral

#D:/Henrique Nince/OneDrive/Área de Trabalho/UnB/TCC/Análises/Tratamento/1. Tratamento inicial/

banco_geral <- read_excel("banco_geral.xlsx", sheet = 1)
banco_geral <- banco_geral %>% mutate(Player = toupper(stri_trans_general(Player, "Latin-ASCII")))

  ## Bancos do transfermarkt

club_id <- read_excel("club_id.xlsx", sheet = 1)
players_positions <- read_excel("players_positions.xlsx", sheet = 1)
players_positions <- players_positions %>% mutate(name = toupper(stri_trans_general(name, "Latin-ASCII")))
names(players_positions)[names(players_positions) == "name"] <- "Player"
names(players_positions)[names(players_positions) == "position"] <- "Position"

# Alterando posicao via transfermarkt

club_id$club_id <- as.character(club_id$club_id)
players_positions <- players_positions %>% left_join(club_id %>% select(club_id, Squad), by = "club_id")
rm(club_id)

  ## Retirando goleiros

resto_goleiros <- subset(banco_geral, Pos == "GK")
write_xlsx(resto_goleiros, "resto_goleiros.xlsx")

banco_geral <- subset(banco_geral, Pos != "GK")
players_positions <- subset(players_positions, Position != "Goalkeeper")

  ## Separando por time

clubes <- unique(banco_geral$Squad)
for (clube in clubes) {
  nome_obj <- paste0("banco_", gsub("[^A-Za-z0-9]", "_", tolower(clube)))
  assign(nome_obj, subset(banco_geral, Squad == clube))}

clubes <- unique(players_positions$Squad)
for (clube in clubes) {
  nome_obj <- paste0("players_", gsub("[^A-Za-z0-9]", "_", tolower(clube)))
  assign(nome_obj, subset(players_positions, Squad == clube))}

  ## Ath Paranaense

banco_ath_paranaense <- banco_ath_paranaense %>%
  mutate(Player = case_when(
    Player == "BRUNO PRAXEDES"        ~ "PRAXEDES",
    Player == "JOAO MACHADO CRUZ"     ~ "JOAO CRUZ",
    Player == "JOSE VITOR SILVA NEVES"~ "ZE VITOR",
    Player == "KAYKE SANTOS"          ~ "KAYKE AYRTON",
    TRUE ~ Player))

banco_ath_paranaense <- banco_ath_paranaense %>% left_join(players_ath_paranaense %>% select(Player, Position), by = "Player")
banco_ath_paranaense <- banco_ath_paranaense %>% relocate(Position, .after = 2)   
banco_ath_paranaense %>% filter(is.na(Position)) %>% select(Player)
table(players_ath_paranaense$Player)
rm(players_ath_paranaense)

  ## Atl Goianiense

banco_atl_goianiense <- banco_atl_goianiense %>%
  mutate(Player = case_when(
    Player == "GABRIEL BARALHAS"           ~ "BARALHAS",
    Player == "GUSTAVO CAMPANHARO"         ~ "CAMPANHARO",
    Player == "JAN CARLOS HURTADO"         ~ "JAN HURTADO",
    Player == "ELI"                        ~ "ELI JUNIOR",
    Player == "JEAN CARLOS ALVES FERREIRA" ~ "JEAN CARLOS",
    Player == "DE MARCAO"                  ~ "MARCAO",
    Player == "PEDRAO"                     ~ "PEDRO HENRIQUE",  
    Player == "BUDIGA RONI"                ~ "RONI",
    Player == "ALIX VINICIUS"              ~ "ALIX",
    TRUE ~ Player))

banco_atl_goianiense <- banco_atl_goianiense %>% left_join(players_atl_goianiense %>% select(Player, Position), by = "Player")
banco_atl_goianiense <- banco_atl_goianiense %>% relocate(Position, .after = 2)
banco_atl_goianiense %>% filter(is.na(Position)) %>% select(Player)
table(players_atl_goianiense$Player)
rm(players_atl_goianiense)

  ## Atl Mineiro

banco_atl_tico_mineiro <- banco_atl_tico_mineiro %>%
  mutate(Player = case_when(
    Player == "ROBERT CONCEICAO" ~ "ROBERT SANTOS",
    Player == "FEDERICO ZARACHO" ~ "MATIAS ZARACHO",
    TRUE ~ Player))

banco_atl_tico_mineiro <- banco_atl_tico_mineiro %>% left_join(players_atl_tico_mineiro %>% select(Player, Position), by = "Player")
banco_atl_tico_mineiro <- banco_atl_tico_mineiro %>% relocate(Position, .after = 2)
banco_atl_tico_mineiro %>% filter(is.na(Position)) %>% select(Player)
table(players_atl_tico_mineiro$Player)
rm(players_atl_tico_mineiro)

  ## Bahia

banco_bahia <- banco_bahia %>%
  mutate(Player = case_when(
    Player == "GABRIEL"              ~ "GABRIEL XAVIER",
    Player == "CAULY OLIVEIRA SOUZA" ~ "CAULY",
    Player == "ADEMIR SANTOS"        ~ "ADEMIR",
    Player == "EVERALDO STUM"        ~ "EVERALDO",
    Player == "YAGO"                 ~ "YAGO FELIPE",
    TRUE ~ Player))

banco_bahia <- banco_bahia %>% left_join(players_bahia %>% select(Player, Position), by = "Player")
banco_bahia <- banco_bahia %>% relocate(Position, .after = 2)
banco_bahia %>% filter(is.na(Position)) %>% select(Player)
table(players_bahia$Player)
rm(players_bahia)

  ## Botafogo (RJ)

banco_botafogo__rj_ <- banco_botafogo__rj_ %>%
  mutate(Player = case_when(
    Player == "YARLEN AUGUSTO" ~ "YARLEN",
    Player == "CARLOS EDUARDO" ~ "EDUARDO",
    Player == "FERNANDO MARCAL" ~ "MARCAL",
    TRUE ~ Player))

banco_botafogo__rj_ <- banco_botafogo__rj_ %>% left_join(players_botafogo__rj_ %>% select(Player, Position), by = "Player")
banco_botafogo__rj_ <- banco_botafogo__rj_ %>% relocate(Position, .after = 2)
banco_botafogo__rj_ %>% filter(is.na(Position)) %>% select(Player)
table(players_botafogo__rj_$Player)
rm(players_botafogo__rj_)

  ## Corinthians

banco_corinthians <- banco_corinthians %>%
  mutate(Player = case_when(
    Player == "JOSE ANDRES MARTINEZ" ~ "JOSE MARTINEZ",
    Player == "ARTHUR"               ~ "ARTHUR SOUSA",
    Player == "BRENO"                ~ "BRENO BIDON",
    Player == "RYAN GUSTAVO"         ~ "RYAN",
    Player == "HECTOR"               ~ "HECTOR HERNANDEZ",
    Player == "MEMPHIS"              ~ "MEMPHIS DEPAY",
    Player == "GUSTAVO MOSQUITO"     ~ "GUSTAVO SILVA",
    Player == "JOAO PEDRO"           ~ "TCHOCA",
    Player == "CHARLES RIGON MATOS"  ~ "CHARLES",
    Player == "FELIX TORRES CAICEDO" ~ "FELIX TORRES",
    TRUE ~ Player))

banco_corinthians <- banco_corinthians %>% left_join(players_corinthians %>% select(Player, Position), by = "Player")
banco_corinthians <- banco_corinthians %>% relocate(Position, .after = 2)
banco_corinthians %>% filter(is.na(Position)) %>% select(Player)
table(players_corinthians$Player)
rm(players_corinthians)

  ## Criciuma

banco_crici_ma <- banco_crici_ma %>%
  mutate(Player = case_when(
    Player == "ARTHUR"                          ~ "ARTHUR CAIKE",
    Player == "SERGIO ANTONIO DE LUIZ JUNIOR"   ~ "SERGINHO",
    Player == "RODRIGO FAGUNDES"                ~ "RODRIGO",
    Player == "RENATO KAYSER"                   ~ "RENATO KAYZER",
    Player == "HIGOR MERITAO"                   ~ "MERITAO",
    Player == "PEDRO ROCHA NEVES"               ~ "PEDRO ROCHA",
    Player == "LUIS EDUARDO MARQUES DOS SANTOS" ~ "DUDU",
    TRUE ~ Player))

banco_crici_ma <- banco_crici_ma %>% left_join(players_crici_ma %>% select(Player, Position), by = "Player")
banco_crici_ma <- banco_crici_ma %>% relocate(Position, .after = 2)
banco_crici_ma %>% filter(is.na(Position)) %>% select(Player)
table(players_crici_ma$Player)
rm(players_crici_ma)

  ## Cruzeiro

banco_cruzeiro <- banco_cruzeiro %>%
  mutate(Player = case_when(
    Player == "ARTHUR"               ~ "ARTHUR GOMES",
    Player == "RAMIRO BENETTI"       ~ "RAMIRO",
    Player == "TEVIS GABRIEL"        ~ "TEVIS",
    Player == "JUAN IGNACIO DINENNO" ~ "JUAN DINENNO",
    Player == "JONATHAN"             ~ "JONATHAN JESUS",
    Player == "KENJI"                ~ "KAIQUE KENJI",
    Player == "MACHADO"              ~ "FILIPE MACHADO",
    Player == "ARTHUR RODRIGUES"     ~ "ARTHUR VIANA",
    Player == "RAFAEL SILVA"         ~ "RAFA SILVA",
    Player == "JOAO WELLINGTON"      ~ "JAPA",
    TRUE ~ Player))

banco_cruzeiro <- banco_cruzeiro %>% left_join(players_cruzeiro %>% select(Player, Position), by = "Player")
banco_cruzeiro <- banco_cruzeiro %>% relocate(Position, .after = 2)
banco_cruzeiro %>% filter(is.na(Position)) %>% select(Player)
table(players_cruzeiro$Player)
rm(players_cruzeiro)

  ## Cuiaba

banco_cuiab_ <- banco_cuiab_ %>%
  mutate(Player = case_when(
    Player == "MARLLON BORGES"          ~ "MARLLON",
    Player == "ELIEL CHRYSTIAN"         ~ "ELIEL",
    Player == "DAVID"                   ~ "DAVID MIGUEL",
    Player == "BRUNO FABIANO ALVES"     ~ "BRUNO ALVES",
    Player == "KAUAN"                   ~ "KAUAN CRISTTYAN",
    Player == "GABRIEL KNESOWITSCH"     ~ "GABRIEL",
    Player == "MAX"                     ~ "MAX ALVES",
    Player == "JUAN PABLO"              ~ "JUAN TAVARES",
    Player == "ALLYSON AIRES DOS SANTOS"~ "ALLYSON",
    TRUE ~ Player))


banco_cuiab_ <- banco_cuiab_ %>% left_join(players_cuiab_ %>% select(Player, Position), by = "Player")
banco_cuiab_ <- banco_cuiab_ %>% relocate(Position, .after = 2)
banco_cuiab_ %>% filter(is.na(Position)) %>% select(Player)
table(players_cuiab_$Player)
rm(players_cuiab_)

  ## Flamengo

banco_flamengo <- banco_flamengo %>%
  mutate(Player = case_when(
    Player == "EVERTTON"       ~ "EVERTTON ARAUJO",
    Player == "GUILHERME"      ~ "GUILHERME GOMES",
    Player == "IGOR"           ~ "IGOR JESUS",
    Player == "EVERTON SOARES" ~ "EVERTON",
    Player == "WALLACE"        ~ "WALLACE YAN",
    TRUE ~ Player))

banco_flamengo <- banco_flamengo %>% left_join(players_flamengo %>% select(Player, Position), by = "Player")
banco_flamengo <- banco_flamengo %>% relocate(Position, .after = 2)
banco_flamengo %>% filter(is.na(Position)) %>% select(Player)
table(players_flamengo$Player)
rm(players_flamengo)

  ## Fluminense

banco_fluminense <- banco_fluminense %>%
  mutate(Player = case_when(
    Player == "LUCAS CALEGARI"      ~ "CALEGARI",
    Player == "THIAGO DOS SANTOS"   ~ "THIAGO SANTOS",
    Player == "GABRIEL"             ~ "GABRIEL PIRES",   # ou "GABRIEL FUENTES", ajustar conforme elenco
    Player == "JAN LUCUMI GONZALEZ" ~ "JAN LUCUMI",
    Player == "FELIPE VIEIRA"       ~ "FELIPE ANDRADE",
    TRUE ~ Player))

banco_fluminense <- banco_fluminense %>% left_join(players_fluminense %>% select(Player, Position), by = "Player")
banco_fluminense <- banco_fluminense %>% relocate(Position, .after = 2)
banco_fluminense %>% filter(is.na(Position)) %>% select(Player)
table(players_fluminense$Player)
rm(players_fluminense)

  ## Fortaleza

banco_fortaleza <- banco_fortaleza %>%
  mutate(Player = case_when(
    Player == "PEDRO BORGES"                   ~ "PEDRO AUGUSTO",
    Player == "BRENO"                          ~ "BRENO LOPES",
    Player == "LEANDRO EMMANUEL MARTINEZ"      ~ "EMMANUEL MARTINEZ",
    Player == "RENATO KAYSER"                  ~ "RENATO KAYZER",
    Player == "PEDRO ROCHA NEVES"              ~ "PEDRO ROCHA",
    Player == "LUIS EDUARDO MARQUES DOS SANTOS"~ "DUDU",
    Player == "GUILHERME TINGA"                ~ "TINGA",
    Player == "JOSE WELISON"                   ~ "ZE WELISON",
    TRUE ~ Player))

banco_fortaleza <- banco_fortaleza %>% left_join(players_fortaleza %>% select(Player, Position), by = "Player")
banco_fortaleza <- banco_fortaleza %>% relocate(Position, .after = 2)
banco_fortaleza %>% filter(is.na(Position)) %>% select(Player)
table(players_fortaleza$Player)
rm(players_fortaleza)

  ## Gremio

banco_gr_mio <- banco_gr_mio %>%
  mutate(Player = case_when(
    Player == "ALYSSON EDWARD"         ~ "ALYSSON",
    Player == "EVERTON"                ~ "EVERTON GALDINO",
    Player == "GUSTAVO MARTINS SANTOS" ~ "GUSTAVO MARTINS",
    Player == "JOAO PEDRO PEPE"        ~ "PEPE",
    Player == "JOAO PEDRO" & Age == 31 ~ "JP GALVAO",
    TRUE ~ Player))

players_gr_mio <- players_gr_mio %>% mutate(Player = if_else(Player == "JOAO PEDRO" & age == 30, "JP GALVAO", Player))
banco_gr_mio <- banco_gr_mio %>% left_join(players_gr_mio %>% select(Player, Position), by = "Player")
banco_gr_mio <- banco_gr_mio %>% relocate(Position, .after = 2)
banco_gr_mio %>% filter(is.na(Position)) %>% select(Player)
table(players_gr_mio$Player)
rm(players_gr_mio)

  ## Internacional

banco_internacional <- banco_internacional %>%
  mutate(Player = case_when(
    Player == "LUCCA HOLANDA" ~ "LUCCA",
    TRUE ~ Player))

banco_internacional <- banco_internacional %>% left_join(players_internacional %>% select(Player, Position), by = "Player")
banco_internacional <- banco_internacional %>% relocate(Position, .after = 2)
banco_internacional %>% filter(is.na(Position)) %>% select(Player)
table(players_internacional$Player)
rm(players_internacional)

  ## Juventude

banco_juventude <- banco_juventude %>%
  mutate(Player = case_when(
    Player == "JEAN CARLOS VICENTE"       ~ "JEAN CARLOS",
    Player == "DAVID"                     ~ "DAVID DA HORA",
    Player == "ERICK"                     ~ "ERICK FARIAS",
    Player == "EWERTON"                   ~ "EWERTHON",
    Player == "CAIQUE DE JESUS GONCALVES" ~ "CAIQUE GONCALVES",
    Player == "KLEITON"                   ~ "KLEITON PEGO",
    Player == "LUIS MANDACA"              ~ "MANDACA",
    Player == "PEIXOTO"                   ~ "DANIEL PEIXOTO",
    Player == "THIAGUINHO"                ~ "THIAGO CONTI",
    TRUE ~ Player))

banco_juventude <- banco_juventude %>% left_join(players_juventude %>% select(Player, Position), by = "Player")
banco_juventude <- banco_juventude %>% relocate(Position, .after = 2)
banco_juventude %>% filter(is.na(Position)) %>% select(Player)
table(players_juventude$Player)
rm(players_juventude)

  ## Palmeiras

banco_palmeiras <- banco_palmeiras %>%
  mutate(Player = case_when(
    Player == "MURILO CERQUEIRA" ~ "MURILO",
    Player == "LUAN GARCIA"      ~ "LUAN",
    Player == "JHONATAN JOWJOW"  ~ "JHON JHON",
    Player == "JOSE LOPEZ"       ~ "JOSE MANUEL LOPEZ",
    Player == "KAIKY NAVES"      ~ "NAVES",
    Player == "ESTEVAO WILLIAN"  ~ "ESTEVAO",
    TRUE ~ Player))

banco_palmeiras <- banco_palmeiras %>% left_join(players_palmeiras %>% select(Player, Position), by = "Player")
banco_palmeiras <- banco_palmeiras %>% relocate(Position, .after = 2)
banco_palmeiras %>% filter(is.na(Position)) %>% select(Player)
table(players_palmeiras$Player)
rm(players_palmeiras)

  ## RB Bragantino

banco_rb_bragantino <- banco_rb_bragantino %>%
  mutate(Player = case_when(
    Player == "VITINHO" & `(Performance) Gls` == 0 ~ "VICTOR HUGO",
    TRUE ~ Player))

players_rb_bragantino <- players_rb_bragantino %>%
  mutate(Player = case_when(
    Player == "VITINHO" & Position == "Attacking Midfield" ~ "VICTOR HUGO",
    TRUE ~ Player))

banco_rb_bragantino <- banco_rb_bragantino %>%
  mutate(Player = case_when(
    Player == "ARTHUR"            ~ "ARTHUR SOUSA",
    Player == "GUILHERME"         ~ "GUILHERME LOPES",
    Player == "GUSTAVINHO"        ~ "GUSTAVO NEVES",
    Player == "JOSE HURTADO"      ~ "JOSE ANDRES HURTADO",
    Player == "JHONATAN JOWJOW"   ~ "JHON JHON",
    Player == "JULIANO"           ~ "JULIANO VIANA",
    Player == "VINICIUS MENDONCA" ~ "VINICINHO",
    Player == "LEONARDO REALPE"   ~ "LEO REALPE",
    Player == "EDUARDO SANTOS"    ~ "EDUARDO",
    TRUE ~ Player))

banco_rb_bragantino <- banco_rb_bragantino %>% left_join(players_rb_bragantino %>% select(Player, Position), by = "Player")
banco_rb_bragantino <- banco_rb_bragantino %>% relocate(Position, .after = 2)
banco_rb_bragantino %>% filter(is.na(Position)) %>% select(Player)
table(players_rb_bragantino$Player)
rm(players_rb_bragantino)

  ## Sao Paulo

banco_s_o_paulo <- banco_s_o_paulo %>%
  mutate(Player = case_when(
    Player == "DIEGO"                 ~ "DIEGO COSTA",
    Player == "FERREIRA"              ~ "FERREIRINHA",
    Player == "HENRIQUE"              ~ "HENRIQUE CARMO",
    Player == "IGOR"                  ~ "IGAO",
    Player == "RODRIGO"               ~ "RODRIGUINHO",
    Player == "JOSE SABINO"           ~ "SABINO",
    Player == "JUAN SANTOS"           ~ "JUAN",
    Player == "IGOR VINICIUS DE SOUZA"~ "IGOR VINICIUS",
    TRUE ~ Player))

banco_s_o_paulo <- banco_s_o_paulo %>% left_join(players_s_o_paulo %>% select(Player, Position), by = "Player")
banco_s_o_paulo <- banco_s_o_paulo %>% relocate(Position, .after = 2)
banco_s_o_paulo %>% filter(is.na(Position)) %>% select(Player)
table(players_s_o_paulo$Player)
rm(players_s_o_paulo)

  ## Vasco da Gama

banco_vasco_da_gama <- banco_vasco_da_gama %>%
  mutate(Player = case_when(
    Player == "MAX ALEGRIA"                   ~ "MAXSUELL ALEGRIA",
    Player == "JOSE LUIS RODRIGUEZ BEBANZ"    ~ "JOSE LUIS RODRIGUEZ",
    Player == "MATEUS COCAO"                  ~ "MATEUS CARVALHO",
    Player == "SERGIO ANTONIO DE LUIZ JUNIOR" ~ "SERGINHO",
    Player == "PABLO GALDAMES MILLAN"         ~ "PABLO GALDAMES",
    Player == "JOAO PEDRO"                    ~ "JP",   
    Player == "LEO PELE"                      ~ "LEO",
    Player == "BRUNO PRAXEDES"                ~ "PRAXEDES",
    Player == "GABRIEL SOUZA"                 ~ "GB",
    Player == "JOSEF DE SOUZA"                ~ "SOUZA",
    Player == "RAYAN VITOR"                   ~ "RAYAN",
    TRUE ~ Player))

banco_vasco_da_gama <- banco_vasco_da_gama %>% left_join(players_vasco_da_gama %>% select(Player, Position), by = "Player")
banco_vasco_da_gama <- banco_vasco_da_gama %>% relocate(Position, .after = 2)
banco_vasco_da_gama %>% filter(is.na(Position)) %>% select(Player)
table(players_vasco_da_gama$Player)
rm(players_vasco_da_gama)

  ## Vitoria

banco_vit_ria <- banco_vit_ria %>%
  mutate(Player = case_when(
    Player == "PABLO BAIANINHO"          ~ "PABLO",
    Player == "JOSE BRENO"               ~ "ZE BRENO",
    Player == "PATRIC CALMON PK"         ~ "PK",
    Player == "IURY DE CASTILHO"         ~ "IURY CASTILHO",
    Player == "ERICK CASTILLO"           ~ "ERYC CASTILLO",
    Player == "REYNALDO CESAR MORAES"    ~ "REYNALDO",
    Player == "EDU"                      ~ "EDU RIBEIRO",
    Player == "MATEUS GONCALVES MARTINS" ~ "MATEUS GONCALVES",
    Player == "JOSE HUGO"                ~ "ZE HUGO",
    Player == "MACHADO"                  ~ "FILIPE MACHADO",
    Player == "MATHEUSINHO"              ~ "MATHEUZINHO",
    Player == "GUSTAVO MOSQUITO"         ~ "GUSTAVO SILVA",
    Player == "RICARDO"                  ~ "RICARDO RYLLER",
    Player == "LUAN SILVA"               ~ "LUAN SANTOS",
    Player == "WAGNER"                   ~ "WAGNER LEONARDO",
    Player == "WILLIAN"                  ~ "WILLIAN OLIVEIRA",
    TRUE ~ Player))

banco_vit_ria <- banco_vit_ria %>% left_join(players_vit_ria %>% select(Player, Position), by = "Player")
banco_vit_ria <- banco_vit_ria %>% relocate(Position, .after = 2)
banco_vit_ria %>% filter(is.na(Position)) %>% select(Player)
table(players_vit_ria$Player)
rm(players_vit_ria)

# Juntando os bancos

rm(players_positions)

banco_tratado <- bind_rows(banco_ath_paranaense, banco_atl_goianiense, banco_atl_tico_mineiro, banco_bahia,
  banco_botafogo__rj_, banco_corinthians, banco_crici_ma, banco_cruzeiro, banco_cuiab_, banco_flamengo,
  banco_fluminense, banco_fortaleza, banco_gr_mio, banco_internacional, banco_juventude, banco_palmeiras,
  banco_rb_bragantino, banco_s_o_paulo, banco_vasco_da_gama, banco_vit_ria)

rm(banco_ath_paranaense, banco_atl_goianiense, banco_atl_tico_mineiro, banco_bahia,
   banco_botafogo__rj_, banco_corinthians, banco_crici_ma, banco_cruzeiro, banco_cuiab_, banco_flamengo,
   banco_fluminense, banco_fortaleza, banco_gr_mio, banco_internacional, banco_juventude, banco_palmeiras,
   banco_rb_bragantino, banco_s_o_paulo, banco_vasco_da_gama, banco_vit_ria)

banco_tratado <- banco_tratado[, -4]

write_xlsx(banco_tratado, "banco_tratado_pos.xlsx")









