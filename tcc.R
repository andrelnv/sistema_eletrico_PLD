#Instalar Pacotes 
pacotes <- c("tidyverse","dplyr","shiny","car","readxl","ggcorrplot",
             "corrplot","ggside","plotly", "readr","lubridate","ggplot2","lmtest","nortest",
             "tidyr","shiny","shinydashboard","broom","DT","scales")

if (any(!pacotes %in% installed.packages())) {
  instalador <- pacotes[!pacotes %in% installed.packages()]
  install.packages(instalador, dependencies = TRUE)
}

# Carregar todos os pacotes
sapply(pacotes, require, character.only = TRUE)


#Importando Banco de Dados abertos AWS

dados_cmo2016<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/cmo_se/CMO_SEMANAL_2016.csv")
dados_cmo2017<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/cmo_se/CMO_SEMANAL_2017.csv")
dados_cmo2018<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/cmo_se/CMO_SEMANAL_2018.csv")
dados_cmo2019<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/cmo_se/CMO_SEMANAL_2019.csv")
dados_cmo2020<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/cmo_se/CMO_SEMANAL_2020.csv")
dados_cmo2021<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/cmo_se/CMO_SEMANAL_2021.csv")
dados_cmo2022<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/cmo_se/CMO_SEMANAL_2022.csv")
dados_cmo2023<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/cmo_se/CMO_SEMANAL_2023.csv")
dados_cmo2024<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/cmo_se/CMO_SEMANAL_2024.csv")
cmo_select<-rbind(dados_cmo2016,dados_cmo2017,dados_cmo2018,dados_cmo2019,dados_cmo2020,dados_cmo2021,dados_cmo2022,dados_cmo2023,dados_cmo2024)
cmo_select <- cmo_select %>%
  mutate(across(
    c(val_cmomedia),
    ~ round(as.numeric(.), 2)
  ))

dados_carga2016<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/carga_energia_di/CARGA_ENERGIA_2016.csv")
dados_carga2017<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/carga_energia_di/CARGA_ENERGIA_2017.csv")
dados_carga2018<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/carga_energia_di/CARGA_ENERGIA_2018.csv")
dados_carga2019<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/carga_energia_di/CARGA_ENERGIA_2019.csv")
dados_carga2020<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/carga_energia_di/CARGA_ENERGIA_2020.csv")
dados_carga2021<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/carga_energia_di/CARGA_ENERGIA_2021.csv")
dados_carga2022<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/carga_energia_di/CARGA_ENERGIA_2022.csv")
dados_carga2023<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/carga_energia_di/CARGA_ENERGIA_2023.csv")
dados_carga2024<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/carga_energia_di/CARGA_ENERGIA_2024.csv")
carga_select<-rbind(dados_carga2016,dados_carga2017,dados_carga2018,dados_carga2019,dados_carga2020,dados_carga2021,dados_carga2022,dados_carga2023,dados_carga2024)
carga_select <- carga_select %>%
  mutate(
    val_cargaenergiamwmed = as.numeric(val_cargaenergiamwmed) / 1e8  # converte para MW
  )
carga_select <- carga_select %>%
  mutate(across(
    c(val_cargaenergiamwmed),
    ~ round(as.numeric(.))
  ))

balanco_energia2016<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/balanco_energia_subsistema_ho/BALANCO_ENERGIA_SUBSISTEMA_2016.csv")
balanco_energia2017<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/balanco_energia_subsistema_ho/BALANCO_ENERGIA_SUBSISTEMA_2017.csv")
balanco_energia2018<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/balanco_energia_subsistema_ho/BALANCO_ENERGIA_SUBSISTEMA_2018.csv")
balanco_energia2019<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/balanco_energia_subsistema_ho/BALANCO_ENERGIA_SUBSISTEMA_2019.csv")
balanco_energia2020<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/balanco_energia_subsistema_ho/BALANCO_ENERGIA_SUBSISTEMA_2020.csv")
balanco_energia2021<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/balanco_energia_subsistema_ho/BALANCO_ENERGIA_SUBSISTEMA_2021.csv")
balanco_energia2022<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/balanco_energia_subsistema_ho/BALANCO_ENERGIA_SUBSISTEMA_2022.csv")
balanco_energia2023<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/balanco_energia_subsistema_ho/BALANCO_ENERGIA_SUBSISTEMA_2023.csv")
balanco_energia2024<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/balanco_energia_subsistema_ho/BALANCO_ENERGIA_SUBSISTEMA_2024.csv")
balanco_select<-rbind(balanco_energia2016,balanco_energia2017,balanco_energia2018,balanco_energia2019,balanco_energia2020,balanco_energia2021,balanco_energia2022,balanco_energia2023,balanco_energia2024)
balanco_select <- balanco_select %>%
  mutate(
    val_gertermica = as.numeric(val_gertermica) / 1e8  # converte para MW
  )
balanco_select <- balanco_select %>%
  mutate(
    val_gerhidraulica = as.numeric(val_gerhidraulica) / 1e8  # converte para MW
  )
balanco_select <- balanco_select %>%
  mutate(across(
    c(val_gerhidraulica, val_gertermica, val_gereolica, val_gersolar),
    ~ round(as.numeric(.))
  ))
#view(balanco_select)
dados_ena2016<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/ena_subsistema_di/ENA_DIARIO_SUBSISTEMA_2016.csv")
dados_ena2017<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/ena_subsistema_di/ENA_DIARIO_SUBSISTEMA_2017.csv")
dados_ena2018<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/ena_subsistema_di/ENA_DIARIO_SUBSISTEMA_2018.csv")
dados_ena2019<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/ena_subsistema_di/ENA_DIARIO_SUBSISTEMA_2019.csv")
dados_ena2020<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/ena_subsistema_di/ENA_DIARIO_SUBSISTEMA_2020.csv")
dados_ena2021<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/ena_subsistema_di/ENA_DIARIO_SUBSISTEMA_2021.csv")
dados_ena2022<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/ena_subsistema_di/ENA_DIARIO_SUBSISTEMA_2022.csv")
dados_ena2023<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/ena_subsistema_di/ENA_DIARIO_SUBSISTEMA_2023.csv")
dados_ena2024<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/ena_subsistema_di/ENA_DIARIO_SUBSISTEMA_2024.csv")
ena_select<-rbind(dados_ena2016,dados_ena2017,dados_ena2018,dados_ena2019,dados_ena2020,dados_ena2021,dados_ena2022,dados_ena2023,dados_ena2024)
ena_select <- ena_select %>%
  mutate(
    ena_armazenavel_regiao_mwmed = as.numeric(ena_armazenavel_regiao_mwmed) / 1e8  # converte para MW
  )
ena_select <- ena_select %>%
  mutate(across(
    c(ena_armazenavel_regiao_mwmed),
    ~ round(as.numeric(.))
  ))

dados_ear2016<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/ear_subsistema_di/EAR_DIARIO_SUBSISTEMA_2016.csv")
dados_ear2017<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/ear_subsistema_di/EAR_DIARIO_SUBSISTEMA_2017.csv")
dados_ear2018<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/ear_subsistema_di/EAR_DIARIO_SUBSISTEMA_2018.csv")
dados_ear2019<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/ear_subsistema_di/EAR_DIARIO_SUBSISTEMA_2019.csv")
dados_ear2020<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/ear_subsistema_di/EAR_DIARIO_SUBSISTEMA_2020.csv")
dados_ear2021<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/ear_subsistema_di/EAR_DIARIO_SUBSISTEMA_2021.csv")
dados_ear2022<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/ear_subsistema_di/EAR_DIARIO_SUBSISTEMA_2022.csv")
dados_ear2023<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/ear_subsistema_di/EAR_DIARIO_SUBSISTEMA_2023.csv")
dados_ear2024<-readr::read_csv2("https://ons-aws-prod-opendata.s3.amazonaws.com/dataset/ear_subsistema_di/EAR_DIARIO_SUBSISTEMA_2024.csv")
ear_select<-rbind(dados_ear2016,dados_ear2017,dados_ear2018,dados_ear2019,dados_ear2020,dados_ear2021,dados_ear2022,dados_ear2023,dados_ear2024)
ear_select <- ear_select %>%
  mutate(
    ear_verif_subsistema_mwmes = as.numeric(ear_verif_subsistema_mwmes) / 1e8  # converte para MW
  )
ear_select <- ear_select %>%
  mutate(across(
    c(ear_verif_subsistema_mwmes),
    ~ round(as.numeric(.))
  ))

dados_demanda     <- read.csv2("dados/demandamaxima.csv")
vazaoturb_Itaipu  <- read.csv2("dados/vazaoturbItaipu.csv")
dados_pld         <- read.csv2("dados/pldsemanal2016.csv")
dados_geracao     <- read.csv2("dados/geracaosemanal.csv")
## Transformando dados diários para dados semanais com média

# EAR Médio Semanal

ear_nordeste<- filter(ear_select,ear_select$nom_subsistema=='NORDESTE')
ear_nordeste_semanal<-ear_nordeste %>%
  mutate(din_intante = floor_date(ear_nordeste$ear_data, unit = "week")) %>%
  group_by(din_intante)  %>% 
  summarise(earmedia_semanal = mean(ear_verif_subsistema_mwmes, na.rm = TRUE))

ear_norte<- filter(ear_select,ear_select$nom_subsistema=='NORTE')
ear_norte_semanal<-ear_norte %>%
  mutate(semanal = floor_date(ear_norte$ear_data, unit = "week")) %>%
  group_by(semanal)  %>% 
  summarise(earmedia_semanal = mean(ear_verif_subsistema_mwmes, na.rm = TRUE))

ear_sudeste<- filter(ear_select,ear_select$nom_subsistema=='SUDESTE')
ear_sudeste_semanal<-ear_sudeste %>%
  mutate(semanal = floor_date(ear_sudeste$ear_data, unit = "week")) %>%
  group_by(semanal)  %>% 
  summarise(earmedia_semanal = mean(ear_verif_subsistema_mwmes, na.rm = TRUE))

ear_sul<- filter(ear_select,ear_select$nom_subsistema=='SUL')
ear_sul_semanal<-ear_sul %>%
  mutate(semanal = floor_date(ear_sul$ear_data, unit = "week")) %>%
  group_by(semanal)  %>% 
  summarise(earmedia_semanal = mean(ear_verif_subsistema_mwmes, na.rm = TRUE))

# Carga Médio Semanal

carga_nordeste<- filter(carga_select,carga_select$id_subsistema=='NE')
carga_nordeste_semanal<-carga_nordeste %>%
  mutate(semanal = floor_date(carga_nordeste$din_instante, unit = "week")) %>%
  group_by(semanal)  %>% 
  summarise(cargamed_semanal = mean(val_cargaenergiamwmed, na.rm = TRUE))

carga_norte<- filter(carga_select,carga_select$id_subsistema=='N')
carga_norte_semanal<-carga_norte %>%
  mutate(semanal = floor_date(carga_norte$din_instante, unit = "week")) %>%
  group_by(semanal)  %>% 
  summarise(cargamed_semanal = mean(val_cargaenergiamwmed, na.rm = TRUE))

carga_sudeste<- filter(carga_select,carga_select$id_subsistema=='SE')
carga_sudeste_semanal<-carga_sudeste %>%
  mutate(semanal = floor_date(carga_sudeste$din_instante, unit = "week")) %>%
  group_by(semanal)  %>% 
  summarise(cargamed_semanal = mean(val_cargaenergiamwmed, na.rm = TRUE))

carga_sul<- filter(carga_select,carga_select$id_subsistema=='S')
carga_sul_semanal<-carga_sul %>%
  mutate(semanal = floor_date(carga_sul$din_instante, unit = "week")) %>%
  group_by(semanal)  %>% 
  summarise(cargamed_semanal = mean(val_cargaenergiamwmed, na.rm = TRUE))

# ENA Média Semanal (Energia Natural Afluente)
ena_nordeste<- filter(ena_select,ena_select$nom_subsistema=='NORDESTE')
ena_nordeste_semanal<-ena_nordeste %>%
  mutate(din_intante = floor_date(ena_nordeste$ena_data, unit = "week")) %>%
  group_by(din_intante)  %>% 
  summarise(enamedia_semanal = mean(ena_armazenavel_regiao_mwmed, na.rm = TRUE))

ena_norte<- filter(ena_select,ena_select$nom_subsistema=='NORTE')
ena_norte_semanal<-ena_norte %>%
  mutate(semanal = floor_date(ena_norte$ena_data, unit = "week")) %>%
  group_by(semanal)  %>% 
  summarise(enamedia_semanal = mean(ena_armazenavel_regiao_mwmed, na.rm = TRUE))

ena_sudeste<- filter(ena_select,ena_select$nom_subsistema=='SUDESTE')
ena_sudeste_semanal<-ena_sudeste %>%
  mutate(semanal = floor_date(ena_sudeste$ena_data, unit = "week")) %>%
  group_by(semanal)  %>% 
  summarise(enamedia_semanal = mean(ena_armazenavel_regiao_mwmed, na.rm = TRUE))

ena_sul<- filter(ena_select,ena_select$nom_subsistema=='SUL')
ena_sul_semanal<-ena_sul %>%
  mutate(semanal = floor_date(ena_sul$ena_data, unit = "week")) %>%
  group_by(semanal)  %>% 
  summarise(enamedia_semanal = mean(ena_armazenavel_regiao_mwmed, na.rm = TRUE))


# Geração Hidrelétrica Total Semanal

hidreletrica_nordeste<- filter(balanco_select,balanco_select$id_subsistema=='NE')
hidreletrica_nordeste_semanal<-hidreletrica_nordeste %>%
  mutate(semanal = floor_date(hidreletrica_nordeste$din_instante, unit = "week")) %>%
  group_by(semanal)  %>% 
  summarise(gerhidreletricatot_semanal = sum(as.numeric(val_gerhidraulica), na.rm = TRUE))

hidreletrica_norte<- filter(balanco_select,balanco_select$id_subsistema=='N')
hidreletrica_norte_semanal<-hidreletrica_norte %>%
  mutate(semanal = floor_date(hidreletrica_norte$din_instante, unit = "week")) %>%
  group_by(semanal)  %>% 
  summarise(gerhidreletricatot_semanal = sum(as.numeric(val_gerhidraulica), na.rm = TRUE))
#view(hidreletrica_norte_semanal)
hidreletrica_sudeste<- filter(balanco_select,balanco_select$id_subsistema=='SE')
hidreletrica_sudeste_semanal<-hidreletrica_sudeste %>%
  mutate(semanal = floor_date(hidreletrica_sudeste$din_instante, unit = "week")) %>%
  group_by(semanal)  %>% 
  summarise(gerhidreletricatot_semanal = sum(as.numeric(val_gerhidraulica), na.rm = TRUE))

hidreletrica_sul<- filter(balanco_select,balanco_select$id_subsistema=='S')
hidreletrica_sul_semanal<-hidreletrica_sul %>%
  mutate(semanal = floor_date(hidreletrica_sul$din_instante, unit = "week")) %>%
  group_by(semanal)  %>% 
  summarise(gerhidreletricatot_semanal = sum(as.numeric(val_gerhidraulica), na.rm = TRUE))


# Geração Termelétrica Total Semanal

termeletrica_nordeste<- filter(balanco_select,balanco_select$id_subsistema=='NE')
termeletrica_nordeste_semanal<-termeletrica_nordeste %>%
  mutate(semanal = floor_date(termeletrica_nordeste$din_instante, unit = "week")) %>%
  group_by(semanal)  %>% 
  summarise(gertermeletricatot_semanal = sum(as.numeric(val_gertermica), na.rm = TRUE))

termeletrica_norte<- filter(balanco_select,balanco_select$id_subsistema=='N')
termeletrica_norte_semanal<-termeletrica_norte %>%
  mutate(semanal = floor_date(termeletrica_norte$din_instante, unit = "week")) %>%
  group_by(semanal)  %>% 
  summarise(gertermeletricatot_semanal = sum(as.numeric(val_gertermica), na.rm = TRUE))

termeletrica_sudeste<- filter(balanco_select,balanco_select$id_subsistema=='SE')
termeletrica_sudeste_semanal<-termeletrica_sudeste %>%
  mutate(semanal = floor_date(termeletrica_sudeste$din_instante, unit = "week")) %>%
  group_by(semanal)  %>% 
  summarise(gertermeletricatot_semanal = sum(as.numeric(val_gertermica), na.rm = TRUE))

termeletrica_sul<- filter(balanco_select,balanco_select$id_subsistema=='S')
termeletrica_sul_semanal<-termeletrica_sul %>%
  mutate(semanal = floor_date(termeletrica_sul$din_instante, unit = "week")) %>%
  group_by(semanal)  %>% 
  summarise(gertermeletricatot_semanal = sum(as.numeric(val_gertermica), na.rm = TRUE))

# Geração Eolica Total Semanal

eolica_nordeste<- filter(balanco_select,balanco_select$id_subsistema=='NE')
eolica_nordeste_semanal<-eolica_nordeste %>%
  mutate(semanal = floor_date(eolica_nordeste$din_instante, unit = "week")) %>%
  group_by(semanal)  %>% 
  summarise(gereolicatot_semanal = sum(as.numeric(val_gereolica), na.rm = TRUE))

eolica_norte<- filter(balanco_select,balanco_select$id_subsistema=='N')
eolica_norte_semanal<-eolica_norte %>%
  mutate(semanal = floor_date(eolica_norte$din_instante, unit = "week")) %>%
  group_by(semanal)  %>% 
  summarise(gereolicatot_semanal = sum(as.numeric(val_gereolica), na.rm = TRUE))

eolica_sudeste<- filter(balanco_select,balanco_select$id_subsistema=='SE')
eolica_sudeste_semanal<-eolica_sudeste %>%
  mutate(semanal = floor_date(eolica_sudeste$din_instante, unit = "week")) %>%
  group_by(semanal)  %>% 
  summarise(gereolicatot_semanal = sum(as.numeric(val_gereolica), na.rm = TRUE))

eolica_sul<- filter(balanco_select,balanco_select$id_subsistema=='S')
eolica_sul_semanal<-eolica_sul %>%
  mutate(semanal = floor_date(eolica_sul$din_instante, unit = "week")) %>%
  group_by(semanal)  %>% 
  summarise(gereolicatot_semanal = sum(as.numeric(val_gereolica), na.rm = TRUE))

# Geração Solar Total Semanal

solar_nordeste<- filter(balanco_select,balanco_select$id_subsistema=='NE')
solar_nordeste_semanal<-solar_nordeste %>%
  mutate(semanal = floor_date(solar_nordeste$din_instante, unit = "week")) %>%
  group_by(semanal)  %>% 
  summarise(gersolartot_semanal = sum(as.numeric(val_gersolar), na.rm = TRUE))

solar_norte<- filter(balanco_select,balanco_select$id_subsistema=='N')
solar_norte_semanal<-solar_norte %>%
  mutate(semanal = floor_date(solar_norte$din_instante, unit = "week")) %>%
  group_by(semanal)  %>% 
  summarise(gersolartot_semanal = sum(as.numeric(val_gersolar), na.rm = TRUE))

solar_sudeste<- filter(balanco_select,balanco_select$id_subsistema=='SE')
solar_sudeste_semanal<-solar_sudeste %>%
  mutate(semanal = floor_date(solar_sudeste$din_instante, unit = "week")) %>%
  group_by(semanal)  %>% 
  summarise(gersolartot_semanal = sum(as.numeric(val_gersolar), na.rm = TRUE))

solar_sul<- filter(balanco_select,balanco_select$id_subsistema=='S')
solar_sul_semanal<-solar_sul %>%
  mutate(semanal = floor_date(solar_sul$din_instante, unit = "week")) %>%
  group_by(semanal)  %>% 
  summarise(gersolartot_semanal = sum(as.numeric(val_gersolar), na.rm = TRUE))

## CMO SUDESTE E CENTRO-OESTE sEMANAL
cmo_sudeste <- filter(cmo_select,cmo_select$id_subsistema=='SE', cmo_select$id_subsistema=='SE') %>% select(din_instante,val_cmomedia)
cmo_var_sudeste <- as.data.frame(cbind(cmo_sudeste$val_cmomedia, dados_pld$SE,ear_sudeste_semanal$earmedia_semanal,carga_sudeste_semanal$cargamed_semanal,hidreletrica_sudeste_semanal$gerhidreletricatot_semanal,termeletrica_sudeste_semanal$gertermeletricatot_semanal,eolica_sudeste_semanal$gereolicatot_semanal,solar_sudeste_semanal$gersolartot_semanal,ena_sudeste_semanal$enamedia_semanal))
colnames(cmo_var_sudeste) <- c("val_cmomedia","pld_SE" ,"ear_SE", "carga_SE", "hidreletrica_SE", "termeletrica_SE", "eolica_SE", "solar_SE","ena_SE")
cmo_var_sudeste <- cmo_var_sudeste %>%
  mutate_at(vars(ear_SE,carga_SE,hidreletrica_SE,termeletrica_SE,eolica_SE,solar_SE,ena_SE), as.numeric)
cmo_sudeste_vf <- head(cmo_var_sudeste , 470)
#view(cmo_sudeste_vf)


##MODELAGEM REGRESSÃO LINEAR PARA O PLD

#CRIAR DATAFRAME COM OS DADOS COLETADOS
pld_se <- dados_pld %>%
  mutate(data = as.Date(din_instante, format = "%d/%m/%Y")) %>%
  transmute(data = floor_date(data, "week"),
            pld_SE = as.numeric(SE))
#view(pld_se)
cmo_se <- cmo_sudeste %>%
  mutate(data = floor_date(as.Date(din_instante), "week")) %>%
  transmute(data, CMO_SE = as.numeric(val_cmomedia))
#view(cmo_se)
carga_se <- carga_sudeste_semanal %>%
  transmute(data = as.Date(semanal),
            Carga_SE = as.numeric(cargamed_semanal))
#view(carga_se)
ear_se <- ear_sudeste_semanal %>%
  transmute(data = as.Date(semanal),
            Ear = as.numeric(earmedia_semanal))
#view(ear_se)
ena_se <- ena_sudeste_semanal %>%
  transmute(data = as.Date(semanal),
            ENA_SE = as.numeric(enamedia_semanal))
#view(ena_se)
hid_se <- hidreletrica_sudeste_semanal %>%
  transmute(data = as.Date(semanal),
            hidreletrica_SE = as.numeric(gerhidreletricatot_semanal))
#view(hid_se)
term_se <- termeletrica_sudeste_semanal %>%
  transmute(data = as.Date(semanal),
            termeletrica_SE = as.numeric(gertermeletricatot_semanal))
#view(term_se)
sol_se <- solar_sudeste_semanal %>%
  transmute(data = as.Date(semanal),
            solar_SE = as.numeric(gersolartot_semanal))
#view(sol_se)
eol_se <- eolica_sudeste_semanal %>%
  transmute(data = as.Date(semanal),
            eolica_SE = as.numeric(gereolicatot_semanal))
#view(eol_se)

demanda_se <- dados_demanda %>%
  mutate(data = as.Date(data)) %>%
  transmute(data = floor_date(data, "week"),
            Demanda = as.numeric(demanda))
#view(demanda_se)

vazao_Itaipu <- vazaoturb_Itaipu %>%
  mutate(data = floor_date(as.Date(data, "%d/%m/%Y"), "week")) %>%
  transmute(data, vazaoItaipu = as.numeric(vazaoturbItaipu))
#view(vazao_Itaipu)

dfs <- list(
  pld_se, cmo_se, carga_se, ear_se, ena_se,
  hid_se, term_se, sol_se, eol_se, demanda_se,
  vazao_Itaipu)

dados_modelo <- reduce(dfs, ~ inner_join(.x, .y, by = "data")) %>%
  arrange(data)
#view(dados_modelo)


#MODELO DE REGRESSÃO LINEAR MÚLTIPLA PARA O PLD
modelo_pld <- lm(pld_SE ~ CMO_SE + Carga_SE + Ear + ENA_SE +
                   hidreletrica_SE + termeletrica_SE + solar_SE +
                   eolica_SE + Demanda + vazaoItaipu,
                 data = dados_modelo)
summary(modelo_pld)

##TESTES PARA VALIDAÇÃO DO MODELO

#TESTE DE MULTICOLINEARIDADE (VIF) 
vif(modelo_pld)

#TESTE DE NORMALIDADE DOS RESÍDUOS (Shapiro-Francia) 
teste_shapiro <- shapiro.test(residuals(modelo_pld))

# TESTE DE HETEROCEDASTICIDADE (Breusch-Pagan)
teste_bp <- bptest(modelo_pld)

# TESTE DE AUTOCORRELAÇÃO DOS RESÍDUOS (Breusch-Godfrey)
teste_bg <- bgtest(modelo_pld, order = 1)

#TESTE DE ESPECIFICAÇÃO DO MODELO (RESET)
teste_reset <- resettest(modelo_pld)

# Rodar os testes
teste_shapiro <- shapiro.test(residuals(modelo_pld))
teste_bp <- bptest(modelo_pld)
teste_bg <- bgtest(modelo_pld, order = 1)
teste_reset <- resettest(modelo_pld)

# Criar data frame com os resultados
resultados_testes <- data.frame(
  Teste = c("Shapiro-Wilk (Normalidade)", 
            "Breusch-Pagan (Heterocedasticidade)", 
            "Breusch-Godfrey (Autocorrelação)", 
            "RESET (Especificação do Modelo)"),
  Estatística = c(
    round(teste_shapiro$statistic, 4),
    round(teste_bp$statistic, 4),
    round(teste_bg$statistic, 4),
    round(teste_reset$statistic, 4)
  ),
  `Valor-p` = c(
    format.pval(teste_shapiro$p.value, digits = 3, eps = .001),
    format.pval(teste_bp$p.value, digits = 3, eps = .001),
    format.pval(teste_bg$p.value, digits = 3, eps = .001),
    format.pval(teste_reset$p.value, digits = 3, eps = .001)
  ),
  `Resultado` = c(
    ifelse(teste_shapiro$p.value < 0.05, "Não normal", "Normal"),
    ifelse(teste_bp$p.value < 0.05, "Heterocedasticidade", "Homoscedástico"),
    ifelse(teste_bg$p.value < 0.05, "Autocorrelação", "Sem autocorrelação"),
    ifelse(teste_reset$p.value < 0.05, "Má especificação", "Bem especificado")
  )
)

# Visualizar resultado
print(resultados_testes)


 theme_set(theme_bw())
 
 ###Função padronização imagens para o TCC
 
 theme_tcc <- function() {
   theme(
     plot.title = element_blank(),
     
     # remover grades e preenchimentos
     panel.grid.major = element_blank(),
     panel.grid.minor = element_blank(),
     panel.background = element_blank(),
     plot.background  = element_blank(),
     panel.border     = element_blank(),
     
     # eixos: linha preta sólida largura ~0,53 mm (≈ 1,5 pt)
     axis.line = element_line(color = "black", linewidth = 1.50),
     
     # títulos dos eixos
     axis.title = element_text(family = "Arial", size = 11, colour = "black"),
     
     # textos dos eixos
     axis.text  = element_text(family = "Arial", size = 10, colour = "black"),
     
     # ticks dos eixos
     axis.ticks = element_line(color = "black")
   )
 }
 
## DESENVOLVIMENTO DOS GRÁFICOS
 
# Gráfico de linha para CMO

cmo_select$val_cmomedia <- as.numeric(as.character(cmo_select$val_cmomedia))
cmo_select$din_instante <- as.POSIXct(as.character(cmo_select$din_instante))
formatted_cmo_select <- format(cmo_select, nsmall = 2)
summary(cmo_select$val_cmomedia)

ggplotly(
  ggplot(cmo_select) +
  geom_line(aes(x = din_instante, y = val_cmomedia, group =id_subsistema, color=id_subsistema), size=1)+xlab("Tempo") +
  ylab("CMO Médio") + 
  ggtitle("Custo Marginal da Operação") +labs(color="Região")+
  scale_x_datetime(date_labels = "%d-%m-%Y", date_breaks = "1 year")
 )

# Gráfico de linha para Energia Armazenada (EAR)

ear_select$ear_verif_subsistema_mwmes <- as.numeric(as.character(ear_select$ear_verif_subsistema_mwmes))
ear_select$ear_data <- as.POSIXct(ear_select$ear_data, format = "%y/%m/%d")
#formatted_ear_select <- format(ear_select, nsmall = 2)
#summary(ear_select$ear_verif_subsistema_mwmes)
ggplotly(
  ggplot(ear_select) +
    geom_line(aes(x = ear_data, y = ear_verif_subsistema_mwmes, group =id_subsistema, color=id_subsistema), size=1)+xlab("Tempo") +
    ylab("Energia Armzenada [MWmês]") + 
    ggtitle("Energia Armazenada em Reservatórios") +labs(color="Região")+
    scale_x_datetime(date_labels = "%d-%m-%Y",date_breaks ="1 year")
)

# Gráfico de linha para Carga de Energia

carga_select$val_cargaenergiamwmed <- as.numeric(as.character(carga_select$val_cargaenergiamwmed))
carga_select$din_instante <- as.POSIXct(carga_select$din_instante, format = "%y/%m/%d")
#formatted_carga_select <- format(carga_select, nsmall = 2)
#summary(carga_select$val_cmomedia)

ggplotly(
  ggplot(carga_select) +
    geom_line(aes(x = din_instante, y = val_cargaenergiamwmed, group =id_subsistema, color=id_subsistema), size=1)+xlab("Tempo") +
    ylab("Carga de Energia[MWmed]") + 
    ggtitle("Carga de Energia Despachadas e/ou programadas pela ONS") +labs(color="Região")+
    scale_x_datetime(date_labels = "%d-%m-%Y", date_breaks = "1 year")
)

# Gráfico de linha para Geração por Usinas Hidreletricas

hidreletrica_v1<-as.data.frame(cbind(hidreletrica_norte_semanal,hidreletrica_nordeste_semanal$gerhidreletricatot_semanal,hidreletrica_sul_semanal$gerhidreletricatot_semanal,hidreletrica_sudeste_semanal$gerhidreletricatot_semanal))
colnames(hidreletrica_v1) <- c("din_instante", "N", "NE", "S", "SE")
hidreletrica_vf <- pivot_longer(
  hidreletrica_v1,
  cols = matches("^N|^NE|^S|^SE"),  
  names_to = "id_subsistema",
  values_to = "geracaohidraulica"
)
hidreletrica_vf <- hidreletrica_vf %>%
  mutate(across(
    c(geracaohidraulica),
    ~ round(as.numeric(.))
  ))

ggplotly(
  ggplot(hidreletrica_vf) +
    geom_line(aes(x = din_instante, y = geracaohidraulica, group =(id_subsistema), color=id_subsistema), size=1)+xlab("Tempo") +
    ylab(" Energia gerada [MWmed]") + 
    ggtitle("Energia gerada por Usinas Hidreletricas") +labs(color="Região")+
    scale_x_datetime(date_labels = "%d-%m-%Y", date_breaks = "1 year")
)

# Gráfico de linha para Geração por Usinas Termeletricas

termeletrica_v1<-as.data.frame(cbind(termeletrica_norte_semanal,termeletrica_nordeste_semanal$gertermeletricatot_semanal,termeletrica_sul_semanal$gertermeletricatot_semanal,termeletrica_sudeste_semanal$gertermeletricatot_semanal))
colnames(termeletrica_v1) <- c("din_instante", "N", "NE", "S", "SE")
termeletrica_vf <- pivot_longer(
  termeletrica_v1,
  cols = matches("^N|^NE|^S|^SE"),  
  names_to = "id_subsistema",
  values_to = "geracaotermeletrica"
)
termeletrica_vf <- termeletrica_vf %>%
  mutate(across(
    c(geracaotermeletrica),
    ~ round(as.numeric(.))
  ))

ggplotly(
  ggplot(termeletrica_vf) +
    geom_line(aes(x = din_instante, y = geracaotermeletrica, group =(id_subsistema), color=id_subsistema), size=1)+xlab("Tempo") +
    ylab(" Energia gerada [MWmed]") + 
    ggtitle("Energia gerada por Usinas Termlétricas") +labs(color="Região")+
    scale_x_datetime(date_labels = "%d-%m-%Y", date_breaks = "1 year")
)
# Gráfico de linha para Geração por Usinas Eólicas

eolica_v1<-as.data.frame(cbind(eolica_norte_semanal,eolica_nordeste_semanal$gereolicatot_semanal,eolica_sul_semanal$gereolicatot_semanal,eolica_sudeste_semanal$gereolicatot_semanal))
colnames(eolica_v1) <- c("din_instante", "N", "NE", "S", "SE")
eolica_vf <- pivot_longer(
  eolica_v1,
  cols = matches("^N|^NE|^S|^SE"),  
  names_to = "id_subsistema",
  values_to = "geracaoeolica"
)
eolica_vf <- eolica_vf %>%
  mutate(across(
    c(geracaoeolica),
    ~ round(as.numeric(.))
  ))
ggplotly(
  ggplot(eolica_vf) +
    geom_line(aes(x = din_instante, y = geracaoeolica, group =(id_subsistema), color=id_subsistema), size=1)+xlab("Tempo") +
    ylab(" Energia gerada [MWmed]") + 
    ggtitle("Energia gerada por Usinas Eólicas") +labs(color="Região")+
    scale_x_datetime(date_labels = "%d-%m-%Y", date_breaks = "1 year")
)

# Gráfico de linha para Geração por Usinas Solares

solar_v1<-as.data.frame(cbind(solar_norte_semanal,solar_nordeste_semanal$gersolartot_semanal,solar_sul_semanal$gersolartot_semanal,solar_sudeste_semanal$gersolartot_semanal))
colnames(solar_v1) <- c("din_instante", "N", "NE", "S", "SE")
solar_vf <- pivot_longer(
  solar_v1,
  cols = matches("^N|^NE|^S|^SE"),  
  names_to = "id_subsistema",
  values_to = "geracaosolar"
)
solar_vf <- solar_vf %>%
  mutate(across(
    c(geracaosolar),
    ~ round(as.numeric(.))
  ))
ggplotly(
  ggplot(solar_vf) +
    geom_line(aes(x = din_instante, y = geracaosolar, group =(id_subsistema), color=id_subsistema), size=1)+xlab("Tempo") +
    ylab(" Energia gerada [MWmed]") + 
    ggtitle("Energia gerada por Usinas Solares") +labs(color="Região")+
    scale_x_datetime(date_labels = "%d-%m-%Y", date_breaks = "1 year")
)


## Gráfico de linha para o PLD

# Transformando dados_pld em um dataframe no formato longer
pld_vf <- pivot_longer(
  dados_pld,
  cols = matches("^N|^NE|^S|^SE"),  
  names_to = "id_subsistema",
  values_to = "pld"
)
pld_vf$pld <- as.numeric(as.character(pld_vf$pld))
pld_vf$din_instante  <- as.POSIXct(pld_vf$din_instante , format = "%d/%m/%Y")
#summary(cmo_select$val_cmomedia)

ggplotly(
  ggplot(pld_vf) +
    geom_line(aes(x = din_instante, y = pld, group =(id_subsistema),color=id_subsistema), size=1)+xlab("Tempo") +
    ylab("PLD [R$/MWh]") + 
    ggtitle("Preço de Liquidação das Diferenças")+labs(color="Região")+
    scale_x_datetime(date_labels = "%d-%m-%Y", date_breaks = "1 year")
  )

# GRAFICO PLD REAL x PREVISTO
pld_real     <- dados_modelo$pld_SE
pld_previsto <- predict(modelo_pld)

df_predicao <- data.frame(PLD_Real = pld_real,
                          PLD_Previsto = pld_previsto)

ggplot(df_predicao, aes(x = PLD_Real, y = PLD_Previsto)) +
  geom_point(color = "skyblue", alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              color = "black", size = 1) +
    labs(x = "Preço Liquidação das Diferenças - Real ",
       y = "valores Previsto ",
       title = "PLD Real vs. PLD Previsto — Modelo Próprio") +
  theme_tcc()

# GRAFICO COMPARAÇÃO MODELO X REAL
df_ts <- dados_modelo
df_ts$Previsto <- predict(modelo_pld)
ggplot(df_ts, aes(x = data)) +
  geom_line(aes(y = pld_SE, color = "Real", linetype = "Real"), size = 1) +
  geom_line(aes(y = Previsto, color = "Previsto", linetype = "Previsto"), size = 1) +
  scale_color_manual(values = c("Real" = "black", "Previsto" = "#5DADE2"), name = "") +
  scale_linetype_manual(values = c("Real" = "solid", "Previsto" = "solid"), name = "") +
  labs(x = "Tempo", y = "PLD (R$/MWh)",
       title = "PLD Real vs. Previsto ao longo do tempo") +
  theme_tcc()


## GRAFICOS PARA ANÁLISES ESTATÍSTICAS
#
pld_df <- pld_vf[pld_vf$id_subsistema == "SE", ]
#view(pld_df)
stopifnot(all(c("din_instante","pld") %in% names(pld_df)))
pld_df <- pld_df %>%
  mutate(
    data = as.Date(din_instante),
    pld  = as.numeric(pld)
  ) %>%
  arrange(din_instante) %>%
  filter(!is.na(din_instante), !is.na(pld))

# Agregar para frequência MENSAL (média mensal)
pld_mensal <- pld_df %>%
  mutate(mes = floor_date(din_instante, "month")) %>%
  group_by(mes) %>%
  summarise(pld = mean(pld, na.rm = TRUE), .groups = "drop") %>%
  arrange(mes)
#view(pld_mensal)


# MÉDIAS MÓVEIS 
pld_ma <- pld_mensal %>%
  mutate(
    mm_3  = rollapply(pld, width = 3,  FUN = mean, align = "right", fill = NA, na.rm = TRUE),
    mm_6  = rollapply(pld, width = 6,  FUN = mean, align = "right", fill = NA, na.rm = TRUE),
    mm_12 = rollapply(pld, width = 12, FUN = mean, align = "right", fill = NA, na.rm = TRUE)
  )

g_mm <- ggplot(pld_ma, aes(x = mes)) +
  geom_line(aes(y = pld, color = "PLD (média mensal)"), linewidth = 0.7, alpha = .6) +
  geom_line(aes(y = mm_3,  color = "MM 3 meses"),  linewidth = 1) +
  geom_line(aes(y = mm_6,  color = "MM 6 meses"),  linewidth = 1) +
  geom_line(aes(y = mm_12, color = "MM 12 meses"), linewidth = 1) +
  scale_color_manual(NULL, values = c(
    "PLD (média mensal)" = "#7F8C8D",
    "MM 3 meses"         = "#2E86C1",
    "MM 6 meses"         = "#27AE60",
    "MM 12 meses"        = "#E67E22"
  )
  ) +
  scale_y_continuous(labels = label_number(big.mark=".", decimal.mark=",")) +
  labs(title = "PLD com Médias Móveis (3, 6 e 12 meses)", x = "Ano", y = "PLD") +
  theme_tcc() +
  theme(legend.position = "bottom")
print(g_mm)

# VOLATILIDADE: DESVIO-PADRÃO MÓVEL e COEFICIENTE DE VARIAÇÃO
janela_vol <- 12  # meses
pld_vol <- pld_mensal %>%
  mutate(
    sd_mov   = rollapply(pld, width = janela_vol, FUN = sd,   align = "right", fill = NA, na.rm = TRUE),
    mean_mov = rollapply(pld, width = janela_vol, FUN = mean, align = "right", fill = NA, na.rm = TRUE),
    cv_mov   = if_else(mean_mov > 0, sd_mov / mean_mov, NA_real_)  # em fração; *100 se quiser em %
  )

g_vol <- ggplot(pld_vol, aes(x = mes)) +
  geom_line(aes(y = sd_mov, color = "Desvio-padrão móvel"), linewidth = 1) +
  geom_line(aes(y = cv_mov*100, color = "CV móvel (%)"), linewidth = 1, linetype = "longdash") +
  scale_color_manual(NULL, values = c("Desvio-padrão móvel"="#C0392B","CV móvel (%)"="#7D3C98")) +
  labs(title = paste0("Volatilidade do PLD (janela = ", janela_vol, " meses)"),
       x = "Ano", y = "SD e CV (%) em eixo único") +
  theme_tcc() +
  theme(legend.position = "bottom")
print(g_vol)

# DISTRIBUIÇÃO: HISTOGRAMA e BOXPLOT POR ANO
g_hist <- ggplot(pld_mensal, aes(x = pld)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#5DADE2", color = "white") +
  geom_density(color = "#2C3E50", linewidth = 1) +
  labs(title = "Distribuição do PLD (Histograma + Densidade)", x = "PLD", y = "Densidade") +
  theme_tcc()

pld_ano <- pld_mensal %>% mutate(ano = year(mes))
g_box <- ggplot(pld_ano, aes(x = factor(ano), y = pld)) +
  geom_boxplot(fill = "#5DADE2", outlier.color = "#CB4335") +
  labs(title = "Boxplot do PLD por Ano", x = "Ano", y = "PLD") +
  theme_tcc()


#OUTLIERS (AIQ)
# Quartis e AIQ na série mensal completa
Q        <- quantile(pld_mensal$pld, probs = c(0.25, 0.75), na.rm = TRUE)
Q1       <- Q[[1]]
Q3       <- Q[[2]]
AIQ_val  <- Q3 - Q1

# Limites moderados (±1,5×AIQ) e extremos (±3×AIQ)
lim_inf_mod <- Q1 - 1.5 * AIQ_val
lim_sup_mod <- Q3 + 1.5 * AIQ_val
lim_inf_ext <- Q1 - 3.0 * AIQ_val
lim_sup_ext <- Q3 + 3.0 * AIQ_val

# Classificação de outliers
pld_flag <- pld_mensal %>%
  dplyr::mutate(
    outlier_tipo = dplyr::case_when(
      pld < lim_inf_ext | pld > lim_sup_ext ~ "Extremo (±3×AIQ)",
      pld < lim_inf_mod | pld > lim_sup_mod ~ "Moderado (±1,5×AIQ)",
      TRUE                                  ~ "Não-outlier"
    )
  )

cores_out <- c(
  "Não-outlier"        = "#5DADE2",
  "Moderado (±1,5×AIQ)"= "#F39C12",
  "Extremo (±3×AIQ)"   = "red"
)

# Linha base 
g_out <- ggplot(pld_flag, aes(x = mes, y = pld)) +
  geom_line(color = "#5DADE2", linewidth = 0.9, alpha = 0.9) +
  # pontos somente para moderados/extremos
  geom_point(
    data = subset(pld_flag, outlier_tipo != "Não-outlier"),
    aes(color = outlier_tipo, shape = outlier_tipo), size = 2.6
  ) +
  # Linhas horizontais de referência (limites)
  geom_hline(yintercept = lim_inf_mod, linetype = "dashed", color = "#F39C12", linewidth = 0.7) +
  geom_hline(yintercept = lim_sup_mod, linetype = "dashed", color = "#F39C12", linewidth = 0.7) +
  geom_hline(yintercept = lim_inf_ext, linetype = "dotted", color = "red", linewidth = 0.8) +
  geom_hline(yintercept = lim_sup_ext, linetype = "dotted", color = "red", linewidth = 0.8) +
  scale_color_manual(values = cores_out, name = NULL) +
  scale_shape_manual(values = c("Moderado (±1,5×AIQ)" = 17,  # triângulo
                                "Extremo (±3×AIQ)"   = 16),  # círculo
                     name = NULL) +
  labs(
    title = "PLD com marcação de outliers (AIQ/IQR)",
    subtitle = sprintf("Q1 = %.2f | Q3 = %.2f | AIQ = %.2f", Q1, Q3, AIQ_val),
    x = "Ano", y = "PLD (R$/MWh)"
  ) +
  theme_tcc() +
  theme(legend.position = "bottom")

############################################################
# EXIBIR GRÁFICOS
############################################################
print(g_mm)    # série + MM(3/6/12)
print(g_vol)   # SD móvel e CV móvel
print(g_hist)  # histograma + densidade
print(g_box)   # boxplot por ano
print(g_out)   # outliers por IQR
summary(modelo_pld)





