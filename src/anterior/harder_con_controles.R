#####
# Cargamos la base
source('src/depurar_data.R')

casos <- casos %>% filter(!is.na(ut))

#####
# Generación de las regresiones considerando eventos detectados
precarga <- FALSE
if (precarga) {
  load('data/interim/efecto_acumulado_cc.RData')
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
  
  
  # Efectos fijos por semana
  delta_t <- casos %>%
    select(semana) %>%
    dummy_cols(select_columns = c('semana'),
               remove_first_dummy = TRUE) %>%
    select(-semana)
  
  controles <- c()
  for (iColumna in seq(1, length(delta_t))) {
    controles[paste('wage_t_',iColumna,sep = '')] <- delta_t[iColumna]*casos['average_wage']
  }
  for (iColumna in seq(1, length(delta_t))) {
    controles[paste('age_t_',iColumna,sep = '')] <- delta_t[iColumna]*casos['average_age']
  }
  for (iColumna in seq(1, length(delta_t))) {
    controles[paste('schooling_t_',iColumna,sep = '')] <- delta_t[iColumna]*casos['average_schooling']
  }
  for (iColumna in seq(1, length(delta_t))) {
    controles[paste('women_t_',iColumna,sep = '')] <- delta_t[iColumna]*casos['women_share']
  }
  
  
  
  
  
semana_202013:semana_202132
  
  #     1. Latency = 0, Drag = 1
  coeficiente <- 
    cbind(
    
    casos %>%
      filter(!is.na(ut)) %>% 
      select(semana, ut, tasa_confirmados, efecto_1, share_Russian, average_wage, 
             average_age, average_schooling, women_share) %>% 
      dummy_cols(select_columns = c('semana', 'ut'),
                 remove_first_dummy = TRUE) %>% 
      select(-semana, -ut),
    )

  # average_age, 
  # average_schooling, 
  # women_share,
  # 
  
    lm(formula = tasa_confirmados ~ 
         share_Russian * efecto_1 + 
         average_wage * factor(ut) + 
         average_age * factor(ut) + 
         average_schooling * factor(ut) + 
         women_share * factor(ut) + 
         factor(semana) + factor(ut)) %>%
    coeftest(., vcov. = vcov(., type = "HC0"))

    estimacion <- c()
    estimacion <- rbind(estimacion, coeficiente[c('(Intercept)', 
                                                'efecto_1',
                                                'share_Russian',
                                                'average_wage',
                                                'average_age',
                                                'average_schooling',
                                                'women_share',
                                                'share_Russian:efecto_1'), ])

  #     2. Latency = 0, Drag = 2
  coeficiente <- casos %>%
    lm(formula = tasa_confirmados ~ 
         share_Russian * efecto_2 + 
         average_wage * efecto_2 + 
         average_age * efecto_2 + 
         average_schooling * efecto_2 + 
         women_share * efecto_2 + 
         factor(semana) + factor(ut)) %>%
    coeftest(., vcov. = vcov(., type = "HC0"))
  estimacion <- rbind(estimacion, coeficiente[c('(Intercept)', 
                                                'efecto_2',
                                                'share_Russian',
                                                'average_wage',
                                                'average_age',
                                                'average_schooling',
                                                'women_share',
                                                'share_Russian:efecto_2',
                                                'efecto_2:average_wage',
                                                'efecto_2:average_age',
                                                'efecto_2:average_schooling',
                                                'efecto_2:women_share'), ])
  
  #     3. Latency = 0, Drag = 3
  coeficiente <- casos %>%
    lm(formula = tasa_confirmados ~ share_Russian * efecto_3 + 
         average_wage * efecto_3 + 
         average_age * efecto_3 + 
         average_schooling * efecto_3 + 
         women_share * efecto_3 + 
         factor(semana) + factor(ut)) %>%
    coeftest(., vcov. = vcov(., type = "HC0"))
  estimacion <- rbind(estimacion, coeficiente[c('(Intercept)', 
                                                'efecto_3',
                                                'share_Russian',
                                                'average_wage',
                                                'average_age',
                                                'average_schooling',
                                                'women_share',
                                                'share_Russian:efecto_3',
                                                'efecto_3:average_wage',
                                                'efecto_3:average_age',
                                                'efecto_3:average_schooling',
                                                'efecto_3:women_share'), ])
  
  #     4. Latency = 0, Drag = 4
  coeficiente <- casos %>%
    lm(formula = tasa_confirmados ~ share_Russian * efecto_4 + 
         average_wage * efecto_4 + 
         average_age * efecto_4 + 
         average_schooling * efecto_4 + 
         women_share * efecto_4 + 
         factor(semana) + factor(ut)) %>%
    coeftest(., vcov. = vcov(., type = "HC0"))
  estimacion <- rbind(estimacion, coeficiente[c('(Intercept)', 
                                                'efecto_4',
                                                'share_Russian',
                                                'average_wage',
                                                'average_age',
                                                'average_schooling',
                                                'women_share',
                                                'share_Russian:efecto_4',
                                                'efecto_4:average_wage',
                                                'efecto_4:average_age',
                                                'efecto_4:average_schooling',
                                                'efecto_4:women_share'), ])
  
  #     5. Latency = 1, Drag = 1
  coeficiente <- casos %>%
    lm(formula = tasa_confirmados ~ share_Russian * efecto_5 + 
         average_wage * efecto_5 + 
         average_age * efecto_5 + 
         average_schooling * efecto_5 + 
         women_share * efecto_5 + 
         factor(semana) + factor(ut)) %>%
    coeftest(., vcov. = vcov(., type = "HC0"))
  estimacion <- rbind(estimacion, coeficiente[c('(Intercept)', 
                                                'efecto_5',
                                                'share_Russian',
                                                'average_wage',
                                                'average_age',
                                                'average_schooling',
                                                'women_share',
                                                'share_Russian:efecto_5',
                                                'efecto_5:average_wage',
                                                'efecto_5:average_age',
                                                'efecto_5:average_schooling',
                                                'efecto_5:women_share'), ])
  
  #     6. Latency = 1, Drag = 2
  coeficiente <- casos %>%
    lm(formula = tasa_confirmados ~ share_Russian * efecto_6 + 
         average_wage * efecto_6 + 
         average_age * efecto_6 + 
         average_schooling * efecto_6 + 
         women_share * efecto_6 + 
         factor(semana) + factor(ut)) %>%
    coeftest(., vcov. = vcov(., type = "HC0"))
  estimacion <- rbind(estimacion, coeficiente[c('(Intercept)', 
                                                'efecto_6',
                                                'share_Russian',
                                                'average_wage',
                                                'average_age',
                                                'average_schooling',
                                                'women_share',
                                                'share_Russian:efecto_6',
                                                'efecto_6:average_wage',
                                                'efecto_6:average_age',
                                                'efecto_6:average_schooling',
                                                'efecto_6:women_share'), ])
  
  #     7. Latency = 1, Drag = 3
  coeficiente <- casos %>%
    lm(formula = tasa_confirmados ~ share_Russian * efecto_7 + 
         average_wage * efecto_7 + 
         average_age * efecto_7 + 
         average_schooling * efecto_7 + 
         women_share * efecto_7 + 
         factor(semana) + factor(ut)) %>%
    coeftest(., vcov. = vcov(., type = "HC0"))
  estimacion <- rbind(estimacion, coeficiente[c('(Intercept)', 
                                                'efecto_7',
                                                'share_Russian',
                                                'average_wage',
                                                'average_age',
                                                'average_schooling',
                                                'women_share',
                                                'share_Russian:efecto_7',
                                                'efecto_7:average_wage',
                                                'efecto_7:average_age',
                                                'efecto_7:average_schooling',
                                                'efecto_7:women_share'), ])
  
  #     8. Latency = 1, Drag = 4
  coeficiente <- casos %>%
    lm(formula = tasa_confirmados ~ share_Russian * efecto_8 + 
         average_wage * efecto_8 + 
         average_age * efecto_8 + 
         average_schooling * efecto_8 + 
         women_share * efecto_8 + 
         factor(semana) + factor(ut)) %>%
    coeftest(., vcov. = vcov(., type = "HC0"))
  estimacion <- rbind(estimacion, coeficiente[c('(Intercept)', 
                                                'efecto_8',
                                                'share_Russian',
                                                'average_wage',
                                                'average_age',
                                                'average_schooling',
                                                'women_share',
                                                'share_Russian:efecto_8',
                                                'efecto_8:average_wage',
                                                'efecto_8:average_age',
                                                'efecto_8:average_schooling',
                                                'efecto_8:women_share'), ])
  
    

  leads_n_lags <- c()
  
  #     1. Latency = 0, Drag = 1
  coeficiente <- casos %>%
    mutate(interaccion = share_Russian * efecto_1) %>%
    mutate(lead_1 = lead(interaccion, 1),
           lead_2 = lead(interaccion, 2),
           lead_3 = lead(interaccion, 3),
           lag_1  = lag (interaccion, 1),
           lag_2  = lag (interaccion, 2),
           lag_3  = lag (interaccion, 3)) %>% 
    lm(formula = tasa_confirmados ~ interaccion + lead_1 + lead_2 + lead_3 + 
         lag_1 + lag_2 + lag_3 + average_wage + average_age + 
         average_schooling + women_share + factor(semana) + factor(ut)) %>%
    coeftest(., vcov. = vcov(., type = "HC0"))
  leads_n_lags <- rbind(leads_n_lags, 
                        coeficiente[c('interaccion', 'lead_1', 'lead_2', 'lead_3', 
                                      'lag_1', 'lag_2', 'lag_3'),])
  
  #     2. Latency = 0, Drag = 2
  coeficiente <- casos %>%
    mutate(interaccion = share_Russian * efecto_2) %>%
    mutate(lead_1 = lead(interaccion, 1),
           lead_2 = lead(interaccion, 2),
           lead_3 = lead(interaccion, 3),
           lag_1  = lag (interaccion, 1),
           lag_2  = lag (interaccion, 2),
           lag_3  = lag (interaccion, 3)) %>% 
    lm(formula = tasa_confirmados ~ interaccion + lead_1 + lead_2 + lead_3 + 
         lag_1 + lag_2 + lag_3 + average_wage + average_age + 
         average_schooling + women_share + factor(semana) + factor(ut)) %>%
    coeftest(., vcov. = vcov(., type = "HC0"))
  leads_n_lags <- rbind(leads_n_lags, 
                        coeficiente[c('interaccion', 'lead_1', 'lead_2', 'lead_3', 
                                      'lag_1', 'lag_2', 'lag_3'),])
  
  #     3. Latency = 0, Drag = 3
  coeficiente <- casos %>%
    mutate(interaccion = share_Russian * efecto_3) %>%
    mutate(lead_1 = lead(interaccion, 1),
           lead_2 = lead(interaccion, 2),
           lead_3 = lead(interaccion, 3),
           lag_1  = lag (interaccion, 1),
           lag_2  = lag (interaccion, 2),
           lag_3  = lag (interaccion, 3)) %>% 
    lm(formula = tasa_confirmados ~ interaccion + lead_1 + lead_2 + lead_3 + 
         lag_1 + lag_2 + lag_3 + average_wage + average_age + 
         average_schooling + women_share + factor(semana) + factor(ut)) %>%
    coeftest(., vcov. = vcov(., type = "HC0"))
  leads_n_lags <- rbind(leads_n_lags, 
                        coeficiente[c('interaccion', 'lead_1', 'lead_2', 'lead_3', 
                                      'lag_1', 'lag_2', 'lag_3'),])
  
  #     4. Latency = 0, Drag = 4
  coeficiente <- casos %>%
    mutate(interaccion = share_Russian * efecto_4) %>%
    mutate(lead_1 = lead(interaccion, 1),
           lead_2 = lead(interaccion, 2),
           lead_3 = lead(interaccion, 3),
           lag_1  = lag (interaccion, 1),
           lag_2  = lag (interaccion, 2),
           lag_3  = lag (interaccion, 3)) %>% 
    lm(formula = tasa_confirmados ~ interaccion + lead_1 + lead_2 + lead_3 + 
         lag_1 + lag_2 + lag_3 + average_wage + average_age + 
         average_schooling + women_share + factor(semana) + factor(ut)) %>%
    coeftest(., vcov. = vcov(., type = "HC0"))
  leads_n_lags <- rbind(leads_n_lags, 
                        coeficiente[c('interaccion', 'lead_1', 'lead_2', 'lead_3', 
                                      'lag_1', 'lag_2', 'lag_3'),])
  
  #     5. Latency = 1, Drag = 1
  coeficiente <- casos %>%
    mutate(interaccion = share_Russian * efecto_5) %>%
    mutate(lead_1 = lead(interaccion, 1),
           lead_2 = lead(interaccion, 2),
           lead_3 = lead(interaccion, 3),
           lag_1  = lag (interaccion, 1),
           lag_2  = lag (interaccion, 2),
           lag_3  = lag (interaccion, 3)) %>% 
    lm(formula = tasa_confirmados ~ interaccion + lead_1 + lead_2 + lead_3 + 
         lag_1 + lag_2 + lag_3 + average_wage + average_age + 
         average_schooling + women_share + factor(semana) + factor(ut)) %>%
    coeftest(., vcov. = vcov(., type = "HC0"))
  leads_n_lags <- rbind(leads_n_lags, 
                        coeficiente[c('interaccion', 'lead_1', 'lead_2', 'lead_3', 
                                      'lag_1', 'lag_2', 'lag_3'),])
  
  #     6. Latency = 1, Drag = 2
  coeficiente <- casos %>%
    mutate(interaccion = share_Russian * efecto_6) %>%
    mutate(lead_1 = lead(interaccion, 1),
           lead_2 = lead(interaccion, 2),
           lead_3 = lead(interaccion, 3),
           lag_1  = lag (interaccion, 1),
           lag_2  = lag (interaccion, 2),
           lag_3  = lag (interaccion, 3)) %>% 
    lm(formula = tasa_confirmados ~ interaccion + lead_1 + lead_2 + lead_3 + 
         lag_1 + lag_2 + lag_3 + average_wage + average_age + 
         average_schooling + women_share + factor(semana) + factor(ut)) %>%
    coeftest(., vcov. = vcov(., type = "HC0"))
  leads_n_lags <- rbind(leads_n_lags, 
                        coeficiente[c('interaccion', 'lead_1', 'lead_2', 'lead_3', 
                                      'lag_1', 'lag_2', 'lag_3'),])
  
  #     7. Latency = 1, Drag = 3
  coeficiente <- casos %>%
    mutate(interaccion = share_Russian * efecto_7) %>%
    mutate(lead_1 = lead(interaccion, 1),
           lead_2 = lead(interaccion, 2),
           lead_3 = lead(interaccion, 3),
           lag_1  = lag (interaccion, 1),
           lag_2  = lag (interaccion, 2),
           lag_3  = lag (interaccion, 3)) %>% 
    lm(formula = tasa_confirmados ~ interaccion + lead_1 + lead_2 + lead_3 + 
         lag_1 + lag_2 + lag_3 + average_wage + average_age + 
         average_schooling + women_share + factor(semana) + factor(ut)) %>%
    coeftest(., vcov. = vcov(., type = "HC0"))
  leads_n_lags <- rbind(leads_n_lags, 
                        coeficiente[c('interaccion', 'lead_1', 'lead_2', 'lead_3', 
                                      'lag_1', 'lag_2', 'lag_3'),])
  
  #     8. Latency = 1, Drag = 4
  coeficiente <- casos %>%
    mutate(interaccion = share_Russian * efecto_8) %>%
    mutate(lead_1 = lead(interaccion, 1),
           lead_2 = lead(interaccion, 2),
           lead_3 = lead(interaccion, 3),
           lag_1  = lag (interaccion, 1),
           lag_2  = lag (interaccion, 2),
           lag_3  = lag (interaccion, 3)) %>% 
    lm(formula = tasa_confirmados ~ interaccion + lead_1 + lead_2 + lead_3 + 
         lag_1 + lag_2 + lag_3 + average_wage + average_age + 
         average_schooling + women_share + factor(semana) + factor(ut)) %>%
    coeftest(., vcov. = vcov(., type = "HC0"))
  leads_n_lags <- rbind(leads_n_lags, 
                        coeficiente[c('interaccion', 'lead_1', 'lead_2', 'lead_3', 
                                      'lag_1', 'lag_2', 'lag_3'),])
  
  estimacion_harder <- estimacion
  leads_n_lags_harder <- leads_n_lags

  save(estimacion_harder, leads_n_lags_harder, file= 'data/interim/efecto_acumulado_harder.RData')
}
# rm(precarga, estimacion, leads_n_lags)


source('src/calculos_con_controles.R')
source('src/calculos.R')













