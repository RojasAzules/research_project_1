##### 
# Tabla Covid 19 PA Adm Terit
#
# Variables:
# Datums - Fecha
# AdministrativiTeritorialasVienibasNosaukums - Nomre Unidad Territorial
# ATVK - Código Unidad Territorial
# ApstiprinataCOVID19infekcija - Confirmados
# AktivaCOVID19infekcija - Activos
# X14DienuKumulativaSaslimstiba - x14 Acumulados

casos <- read.csv('./data/raw/covid_19_pa_adm_terit.csv', sep = ';') %>% 
  rename(Fecha = Datums,
         NombreUnidadTerritorial = AdministrativiTeritorialasVienibasNosaukums, 
         CodigoUnidadTerritorial = ATVK,
         Confirmados = ApstiprinataCOVID19infekcija,
         Activos = AktivaCOVID19infekcija,
         Acumulados = X14DienuKumulativaSaslimstiba) %>% 
  select(-Activos, -Acumulados) %>% 
  # Quitiamos registros sin dirección especificada, tienen datos de confirmados en 0
  filter(NombreUnidadTerritorial != 'Adrese nav norādīta') %>% 
  # Convertimos Confirmados a numérico
  mutate(Confirmados = case_when(Confirmados == "no 1 līdz 5" ~ 3,
                                 Confirmados != "no 1 līdz 5" ~ as.double(Confirmados)),
         Fecha = ymd(Fecha)) %>% 
  # Creamos variable Nuevos (número de casos nuevos por unidad territorial)
  group_by(NombreUnidadTerritorial) %>% 
  mutate(Nuevos = Confirmados - lag(Confirmados)) %>% 
  ungroup()

# Se tienen 469 (fechas) * 120 (unidades territoriales) = 56280 observaciones
# Los datos de las variables AktivaCOVID19infekcija y X14DienuKumulativaSaslimstiba
#   parecieran ser complementarios, del 18 al 19 de octubre ocurre un corte.
#   No pareciera ser evidente saber qué hacer con estas variables.

reciente <- read.csv('./data/raw/covid_19_pa_adm_terit_new.csv', sep = ';') %>% 
  rename(Fecha = Datums,
         NombreUnidadTerritorial = AdministrativiTeritorialasVienibasNosaukums, 
         CodigoUnidadTerritorial = ATVK,
         Confirmados = ApstiprinataCOVID19infekcija,
         Acumulados = X14DienuKumulativaSaslimstiba) %>% 
  select(-Acumulados)%>%
  # Quitiamos registros sin dirección especificada, tienen datos de confirmados en 0
  filter(NombreUnidadTerritorial != 'Adrese nav norādīta') %>% 
  # Convertimos Confirmados a numérico
  mutate(Confirmados = case_when(Confirmados == "no 1 līdz 5" ~ 3,
                                 Confirmados != "no 1 līdz 5" ~ as.double(Confirmados)),
         Fecha = ymd(Fecha)) %>% 
  # Creamos variable Nuevos (número de casos nuevos por unidad territorial)
  group_by(NombreUnidadTerritorial) %>% 
  mutate(Nuevos = Confirmados - lag(Confirmados)) %>% 
  ungroup()

casos <- rbind(casos, reciente)

##### 
# Datos de etnicidad por municipio
etnicidad <- read_dta('./data/raw/Ethnic_nationality_2020_Alone.dta') %>% 
  rename(NombreUnidadTerritorial = l0_name,
         ut = FID) %>% 
  mutate(ut = ut + 1) %>% 
  select(-l0_code) %>% 
  select(!starts_with('z'))
# Agregárselos a la base de casos
casos <- casos %>% left_join(etnicidad) %>%
  # Tasa de nuevos confirmados por cada 100 mil habitantes
  mutate(tasa_confirmados = Nuevos/Total*100000) %>% 
  relocate(Fecha, 
          ut,
          NombreUnidadTerritorial,
          tasa_confirmados,
          share_Russian,
          share_Latvian,
          share_Belarusian,
          share_Ukrainian,
          share_Polish,
          share_Lithuanian,
          share_Other)


vaccines <- read_dta('./data/raw/Vaccination_Center_Date_Type_Dose_Panel.dta')

