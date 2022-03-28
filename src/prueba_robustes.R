#####
# Cargamos la base
source('src/depurar_data.R')

#####
# Generación de las regresiones considerando eventos detectados
precarga <- TRUE
if (precarga) {
  load('data/interim/efecto_acumulado_sin_dic.RData')
} else {
  # Suponemos que los efectos de los eventos tienen un periodo de latencia y una
  # duración. Calcularemos el efecto acumulado de los eventos en la variable
  # acumulado considerando ocho posibles versiones
  #     1. Latency = 0, Drag = 1
  #     2. Latency = 0, Drag = 2
  #     3. Latency = 0, Drag = 3
  #     4. Latency = 0, Drag = 4
  #     5. Latency = 1, Drag = 1
  #     6. Latency = 1, Drag = 2
  #     7. Latency = 1, Drag = 3
  #     8. Latency = 1, Drag = 4
  # 
  acumulado <- casos %>% 
    select(semana) %>% 
    pull() %>% 
    unique() %>% 
    as.data.frame() %>% 
    rename('semana' = '.') %>% 
    filter(semana != 202053)

  semanasConEventos <- eventos %>% 
    filter(Consider == 1) %>% 
    select(Date) %>% 
    mutate(semana = year(Date)*100 + week(Date)) %>% 
    mutate(semana = replace(semana, semana == 202053, 202052)) %>% 
    select(semana) %>% 
    pull()
  
  efecto <- function(effect_weeks, total_weeks, latency, drag){
    # La "estela" es el periodo durante el cual el efecto un evento es observado,
    # comienza después del periodo de latencia (latency) y tiene una duración en
    # semanas (drag), 
    estela <- function(total_weeks, semana, latency, drag){
      estela <- rep(0, length(total_weeks))
      for (i in seq(drag)) {
        estela <- estela + shift(as.integer(total_weeks == semana),
                                 latency + i - 1)
      }
      estela
    }
    # Calculamos la estela de los eventos, la lista de semanas con eventos 
    # corresponde a la variable effect_weeks, por cada valor en este arreglo
    # se calcula su estela. La suma de la estela de todos los eventos 
    # corresponde a la salida de la función.
    rowSums(sapply(effect_weeks, estela, total_weeks = total_weeks, 
                   latency = latency, drag = drag))
    
  }
  
  # La variable acumulado tiene nueve variables, una para las semanas que comprende
  # la ventana de observación y ocho para las distintas versiones del efecto 
  # acumulado derivado de los eventos
  acumulado <- acumulado %>% 
    mutate(efecto_1 = efecto(semanasConEventos, acumulado$semana, latency = 0, drag = 1),
           efecto_2 = efecto(semanasConEventos, acumulado$semana, latency = 0, drag = 2),
           efecto_3 = efecto(semanasConEventos, acumulado$semana, latency = 0, drag = 3),
           efecto_4 = efecto(semanasConEventos, acumulado$semana, latency = 0, drag = 4),
           efecto_5 = efecto(semanasConEventos, acumulado$semana, latency = 1, drag = 1),
           efecto_6 = efecto(semanasConEventos, acumulado$semana, latency = 1, drag = 2),
           efecto_7 = efecto(semanasConEventos, acumulado$semana, latency = 1, drag = 3),
           efecto_8 = efecto(semanasConEventos, acumulado$semana, latency = 1, drag = 4))
  
  # Agregamos variable de efecto (en sus 8 versiones) a la base de datos de casos
  casos <- left_join(casos, acumulado, by = "semana")
  
  estimacion <- c()

  #     1. Latency = 0, Drag = 1        MODELO PRINCIPAL sin diciembre
  coeficiente <- casos %>%
    filter(month(Fecha)!=12) %>% 
    lm(formula = tasa_confirmados ~ 
         share_Russian * efecto_1 + 
         average_wage * efecto_1 + 
         average_age * efecto_1 + 
         average_schooling * efecto_1 + 
         women_share * efecto_1 + 
         factor(semana) + factor(ut)) %>%
    coeftest(., vcov. = vcov(., type = "HC0"))
  estimacion <- rbind(estimacion, coeficiente[c('(Intercept)', 
                                                'efecto_1',
                                                'share_Russian',
                                                'average_wage',
                                                'average_age',
                                                'average_schooling',
                                                'women_share',
                                                'share_Russian:efecto_1',
                                                'efecto_1:average_wage',
                                                'efecto_1:average_age',
                                                'efecto_1:average_schooling',
                                                'efecto_1:women_share'), ])
  
  #     1. Latency = 0, Drag = 1        MODELO PRINCIPAL sin enero
  coeficiente <- casos %>%
    filter(month(Fecha)!=1) %>% 
    lm(formula = tasa_confirmados ~ 
         share_Russian * efecto_1 + 
         average_wage * efecto_1 + 
         average_age * efecto_1 + 
         average_schooling * efecto_1 + 
         women_share * efecto_1 + 
         factor(semana) + factor(ut)) %>%
    coeftest(., vcov. = vcov(., type = "HC0"))
  estimacion <- rbind(estimacion, coeficiente[c('(Intercept)', 
                                                'efecto_1',
                                                'share_Russian',
                                                'average_wage',
                                                'average_age',
                                                'average_schooling',
                                                'women_share',
                                                'share_Russian:efecto_1',
                                                'efecto_1:average_wage',
                                                'efecto_1:average_age',
                                                'efecto_1:average_schooling',
                                                'efecto_1:women_share'), ])
  
  #     1. Latency = 0, Drag = 1        MODELO PRINCIPAL sin dic ni enero
  coeficiente <- casos %>%
    filter(month(Fecha)!=12, month(Fecha)!=1) %>% 
    lm(formula = tasa_confirmados ~ 
         share_Russian * efecto_1 + 
         average_wage * efecto_1 + 
         average_age * efecto_1 + 
         average_schooling * efecto_1 + 
         women_share * efecto_1 + 
         factor(semana) + factor(ut)) %>%
    coeftest(., vcov. = vcov(., type = "HC0"))
  estimacion <- rbind(estimacion, coeficiente[c('(Intercept)', 
                                                'efecto_1',
                                                'share_Russian',
                                                'average_wage',
                                                'average_age',
                                                'average_schooling',
                                                'women_share',
                                                'share_Russian:efecto_1',
                                                'efecto_1:average_wage',
                                                'efecto_1:average_age',
                                                'efecto_1:average_schooling',
                                                'efecto_1:women_share'), ])
  
  

  leads_n_lags <- c()
  
  #     1. Latency = 0, Drag = 1        LEADS & LAGS sin dic
  coeficiente <- casos %>%
    filter(month(Fecha)!=12) %>% 
    mutate(lead_1 = lead(share_Russian * efecto_1, 1),
           lead_2 = lead(share_Russian * efecto_1, 2),
           lead_3 = lead(share_Russian * efecto_1, 3),
           lag_1  = lag (share_Russian * efecto_1, 1),
           lag_2  = lag (share_Russian * efecto_1, 2),
           lag_3  = lag (share_Russian * efecto_1, 3)) %>% 
    lm(formula = tasa_confirmados ~ 
         share_Russian * efecto_1 + 
         average_wage * efecto_1 + 
         average_age * efecto_1 + 
         average_schooling * efecto_1 + 
         women_share * efecto_1 + 
         lead_1 + lead_2 + lead_3 + 
         lag_1 + lag_2 + lag_3 + 
         factor(semana) + factor(ut)) %>%
    coeftest(., vcov. = vcov(., type = "HC0"))
  leads_n_lags <- rbind(leads_n_lags, 
                        coeficiente[c('share_Russian:efecto_1', 'lead_1', 'lead_2', 'lead_3', 
                                      'lag_1', 'lag_2', 'lag_3'),])

  #     1. Latency = 0, Drag = 1        LEADS & LAGS sin ene
  coeficiente <- casos %>%
    filter(month(Fecha)!=01) %>% 
    mutate(lead_1 = lead(share_Russian * efecto_1, 1),
           lead_2 = lead(share_Russian * efecto_1, 2),
           lead_3 = lead(share_Russian * efecto_1, 3),
           lag_1  = lag (share_Russian * efecto_1, 1),
           lag_2  = lag (share_Russian * efecto_1, 2),
           lag_3  = lag (share_Russian * efecto_1, 3)) %>% 
    lm(formula = tasa_confirmados ~ 
         share_Russian * efecto_1 + 
         average_wage * efecto_1 + 
         average_age * efecto_1 + 
         average_schooling * efecto_1 + 
         women_share * efecto_1 + 
         lead_1 + lead_2 + lead_3 + 
         lag_1 + lag_2 + lag_3 + 
         factor(semana) + factor(ut)) %>%
    coeftest(., vcov. = vcov(., type = "HC0"))
  leads_n_lags <- rbind(leads_n_lags, 
                        coeficiente[c('share_Russian:efecto_1', 'lead_1', 'lead_2', 'lead_3', 
                                      'lag_1', 'lag_2', 'lag_3'),])
  
  #     1. Latency = 0, Drag = 1        LEADS & LAGS sin dic ni ene
  coeficiente <- casos %>%
    filter(month(Fecha)!=12, month(Fecha)!=1) %>% 
    mutate(lead_1 = lead(share_Russian * efecto_1, 1),
           lead_2 = lead(share_Russian * efecto_1, 2),
           lead_3 = lead(share_Russian * efecto_1, 3),
           lag_1  = lag (share_Russian * efecto_1, 1),
           lag_2  = lag (share_Russian * efecto_1, 2),
           lag_3  = lag (share_Russian * efecto_1, 3)) %>% 
    lm(formula = tasa_confirmados ~ 
         share_Russian * efecto_1 + 
         average_wage * efecto_1 + 
         average_age * efecto_1 + 
         average_schooling * efecto_1 + 
         women_share * efecto_1 + 
         lead_1 + lead_2 + lead_3 + 
         lag_1 + lag_2 + lag_3 + 
         factor(semana) + factor(ut)) %>%
    coeftest(., vcov. = vcov(., type = "HC0"))
  leads_n_lags <- rbind(leads_n_lags, 
                        coeficiente[c('share_Russian:efecto_1', 'lead_1', 'lead_2', 'lead_3', 
                                      'lag_1', 'lag_2', 'lag_3'),])
  
    

  
  estimacion_sin_dic <- estimacion
  leads_n_lags_sin_dic <- leads_n_lags

  save(estimacion_sin_dic, leads_n_lags_sin_dic, file= 'data/interim/efecto_acumulado_sin_dic.RData')
}
# rm(precarga, estimacion, leads_n_lags)














