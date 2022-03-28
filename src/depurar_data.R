source('src/librerias.R')

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
         nombre_unidad_territorial = AdministrativiTeritorialasVienibasNosaukums, 
         codigo_unidad_territorial = ATVK,
         Confirmados = ApstiprinataCOVID19infekcija,
         Activos = AktivaCOVID19infekcija,
         Acumulados = X14DienuKumulativaSaslimstiba) %>% 
  select(-Activos, -Acumulados) %>% 
  # Quitiamos registros sin dirección especificada, tienen datos de confirmados en 0
  filter(nombre_unidad_territorial != 'Adrese nav norādīta') %>% 
  # Convertimos Confirmados a numérico
  mutate(Confirmados = case_when(Confirmados == "no 1 līdz 5" ~ 3,
                                 Confirmados != "no 1 līdz 5" ~ as.double(Confirmados)),
         Fecha = ymd(Fecha)) %>% 
  # Creamos variable Nuevos (número de casos nuevos por unidad territorial)
  group_by(nombre_unidad_territorial) %>% 
  mutate(Nuevos = Confirmados - lag(Confirmados)) %>% 
  ungroup()

# Se tienen 469 (fechas) * 120 (unidades territoriales) = 56280 observaciones
# Los datos de las variables AktivaCOVID19infekcija y X14DienuKumulativaSaslimstiba
#   parecieran ser complementarios, del 18 al 19 de octubre ocurre un corte.
#   No pareciera ser evidente saber qué hacer con estas variables.

##### 
# Tabla Covid 19 PA Adm Terit new
reciente <- read.csv('./data/raw/covid_19_pa_adm_terit_new.csv', sep = ';') %>% 
  rename(Fecha = Datums,
         nombre_unidad_territorial = AdministrativiTeritorialasVienibasNosaukums, 
         codigo_unidad_territorial = ATVK,
         Confirmados = ApstiprinataCOVID19infekcija,
         Acumulados = X14DienuKumulativaSaslimstiba) %>% 
  select(-Acumulados)%>%
  # Quitiamos registros sin dirección especificada, tienen datos de confirmados en 0
  filter(nombre_unidad_territorial != 'Adrese nav norādīta') %>% 
  # Convertimos Confirmados a numérico
  mutate(Confirmados = case_when(Confirmados == "no 1 līdz 5" ~ 3,
                                 Confirmados != "no 1 līdz 5" ~ as.double(Confirmados)),
         Fecha = ymd(Fecha)) %>% 
  # Creamos variable Nuevos (número de casos nuevos por unidad territorial)
  group_by(nombre_unidad_territorial) %>% 
  mutate(Nuevos = Confirmados - lag(Confirmados)) %>% 
  ungroup()

casos <- rbind(casos, reciente)
rm(reciente)

##### 
# Datos demograficos por municipio
demograficos <- read_dta('./data/raw/Ethnic_nationality_2020_Alone.dta') %>%
  rename(nombre_unidad_territorial = l0_name,
         ut = FID) %>%
  mutate(ut = ut + 1) %>%
  select(-l0_code) %>%
  select(!starts_with('z'))
# Tabla con traducciones de los nombres de las unidades territoriales
lost_in_translation <- read_xlsx('data/raw/territorial_units.xlsx')
# Average wage
temp <- read_xlsx(path = 'data/raw/Average monthly wages and salaries in cities and municipalities (in euro) by Sector, Gross Net, Territorial unit and Time period.xlsx')[4:(4+119-1),3:4]
colnames(temp) <- c('english_name', 'average_wage')
temp <- temp %>% left_join(lost_in_translation, by = 'english_name')
demograficos <- demograficos %>% left_join(temp %>% select(-note, -english_name), 
                                           by = 'nombre_unidad_territorial')
# Average age
temp <- read_xlsx(path = 'data/raw/Average age of population in regions, cities, municipalities, towns, rural territories, neighbourhoods and densely populated areas by All territories and Time period.xlsx', 
                  col_names = c('english_name', 'average_age', 'eamesta')) %>% 
  select(-eamesta) %>% 
  slice(3:n()) %>% 
  mutate(
    english_name = str_trim(str_replace( string = english_name,
                                          pattern = 'LV[0-9]{0,8}', 
                                          replacement = ''))
  ) %>% 
  left_join(lost_in_translation, by = 'english_name') %>% 
  filter(!is.na(nombre_unidad_territorial))
demograficos <- demograficos %>% left_join(temp %>% select(-note, -english_name), 
                                           by = 'nombre_unidad_territorial')
# Index of mean years of schooling
temp <- read_xlsx(path = 'data/raw/Index of mean years of schooling of permanent residents aged 25 and over in regions, cities, municipalities, towns, rural territories (based on the boundaries in force at the beginning of 2021), ne.xlsx', 
                  col_names = c('english_name', 'average_schooling')) %>% 
  slice(3:n()) %>% 
  mutate(
    english_name = str_trim(str_replace( string = english_name,
                                         pattern = 'LV[0-9]{0,8}', 
                                         replacement = ''))
  ) %>% 
  left_join(lost_in_translation, by = 'english_name') %>% 
  filter(!is.na(nombre_unidad_territorial))
demograficos <- demograficos %>% left_join(temp %>% select(-note, -english_name), 
                                           by = 'nombre_unidad_territorial')

# Share of women
temp <- read_xlsx(path = 'data/raw/Population by sex in regions, cities, municipalities, towns and rural territories at the beginning of the year by Sex, Territorial unit, Indicator and Time period.xlsx', 
                  col_names = c('genero', 'english_name', 'categoria', 'n_personas', 'eamesta')) %>% 
  filter(categoria == 'By actual place') %>% 
  select(-eamesta, -categoria) %>% 
  mutate(genero = na.locf(genero)) %>%  
  left_join(lost_in_translation, by = 'english_name') %>% 
  filter(!is.na(nombre_unidad_territorial)) %>% 
  select(-note, -english_name)
# Obtenemos el cociente
temp <- data.frame(
  'nombre_unidad_territorial' = temp %>% 
    filter(genero == 'Females') %>% 
    arrange(nombre_unidad_territorial) %>% 
    pull(nombre_unidad_territorial),
  'women_share' = c( temp %>% 
  filter(genero == 'Females') %>% 
  arrange(nombre_unidad_territorial) %>% 
  mutate(n_personas = as.numeric(n_personas)) %>% 
  pull(n_personas) / 
    temp %>% 
    filter(genero == 'Total') %>% 
    arrange(nombre_unidad_territorial) %>% 
    mutate(n_personas = as.numeric(n_personas)) %>% 
    pull(n_personas) )
)
demograficos <- demograficos %>% left_join(temp, by = 'nombre_unidad_territorial')

# Nothing to see
rm(lost_in_translation, temp)

##### 
# Agregar datos demográficos a la base de casos
casos <- casos %>% left_join(demograficos, by = "nombre_unidad_territorial") %>%
  # Tasa de nuevos confirmados por cada 100 mil habitantes
  mutate(tasa_confirmados = Nuevos/Total*100000, 
         average_wage = as.numeric(average_wage), 
         average_schooling = as.numeric(average_schooling), 
         average_age = as.numeric(average_age), 
         women_share = as.numeric(women_share)) %>% 
  relocate(Fecha, 
          ut,
          nombre_unidad_territorial,
          tasa_confirmados,
          share_Russian,
          share_Latvian,
          share_Belarusian,
          share_Ukrainian,
          share_Polish,
          share_Lithuanian,
          share_Other) %>% 
  select(-codigo_unidad_territorial, -Confirmados, -share_misinformed) %>% 
  filter(Fecha > '2020-03-19')


##### 
# Generar variable de semanas
casos <- casos %>% 
  mutate(semana = week(Fecha) + year(Fecha)*100)
  # mutate(semana = min(week(Fecha),52) + year(Fecha)*100)

##### 
# Respaldar base de casos
write_dta(casos, 'data/processed/casos.dta')
write_csv(casos, 'data/processed/casos.csv')



##### 
# Tabla de eventos

eventos <- read_excel('data/raw/Events.xlsx', sheet = 1) %>% 
  mutate(semana = week(Date) + year(Date)*100) %>% 
  mutate(semana = replace(semana, semana == 202053, 202052))
  # mutate(semana = min(week(Fecha),52) + year(Fecha)*100)

# disinfo_per_month <- read_excel('data/raw/Events.xlsx', sheet = 2)

# eventos_semanales <- read_excel('data/raw/Events.xlsx', sheet = 4) %>% 
#   mutate(semana = week(Date) + year(Date)*100)


casos <- casos %>% filter(!is.na(ut))








