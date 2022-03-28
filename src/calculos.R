#####
# Cargamos la base
source('src/depurar_data.R')

#####
# Generación de las regresiones considerando eventos detectados
precarga <- TRUE
if (precarga) {
  load('data/interim/efecto_acumulado.RData')
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
  
  #     1. Latency = 0, Drag = 1
  coeficiente <- casos %>%
    mutate(interaccion = share_Russian * efecto_1) %>%
    lm(formula = tasa_confirmados ~ interaccion + factor(semana) + factor(ut)) %>%
    coeftest(., vcov. = vcov(., type = "HC0"))
  estimacion <- rbind(estimacion, coeficiente['interaccion',])

  #     2. Latency = 0, Drag = 2
  coeficiente <- casos %>%
    mutate(interaccion = share_Russian * efecto_2) %>%
    lm(formula = tasa_confirmados ~ interaccion + factor(semana) + factor(ut)) %>%
    coeftest(., vcov. = vcov(., type = "HC0"))
  estimacion <- rbind(estimacion, coeficiente['interaccion',])
  
  #     3. Latency = 0, Drag = 3
  coeficiente <- casos %>%
    mutate(interaccion = share_Russian * efecto_3) %>%
    lm(formula = tasa_confirmados ~ interaccion + factor(semana) + factor(ut)) %>%
    coeftest(., vcov. = vcov(., type = "HC0"))
  estimacion <- rbind(estimacion, coeficiente['interaccion',])
  
  #     4. Latency = 0, Drag = 4
  coeficiente <- casos %>%
    mutate(interaccion = share_Russian * efecto_4) %>%
    lm(formula = tasa_confirmados ~ interaccion + factor(semana) + factor(ut)) %>%
    coeftest(., vcov. = vcov(., type = "HC0"))
  estimacion <- rbind(estimacion, coeficiente['interaccion',])
  
  #     5. Latency = 1, Drag = 1
  coeficiente <- casos %>%
    mutate(interaccion = share_Russian * efecto_5) %>%
    lm(formula = tasa_confirmados ~ interaccion + factor(semana) + factor(ut)) %>%
    coeftest(., vcov. = vcov(., type = "HC0"))
  estimacion <- rbind(estimacion, coeficiente['interaccion',])
  
  #     6. Latency = 1, Drag = 2
  coeficiente <- casos %>%
    mutate(interaccion = share_Russian * efecto_6) %>%
    lm(formula = tasa_confirmados ~ interaccion + factor(semana) + factor(ut)) %>%
    coeftest(., vcov. = vcov(., type = "HC0"))
  estimacion <- rbind(estimacion, coeficiente['interaccion',])
  
  #     7. Latency = 1, Drag = 3
  coeficiente <- casos %>%
    mutate(interaccion = share_Russian * efecto_7) %>%
    lm(formula = tasa_confirmados ~ interaccion + factor(semana) + factor(ut)) %>%
    coeftest(., vcov. = vcov(., type = "HC0"))
  estimacion <- rbind(estimacion, coeficiente['interaccion',])
  
  #     8. Latency = 1, Drag = 4
  coeficiente <- casos %>%
    mutate(interaccion = share_Russian * efecto_8) %>%
    lm(formula = tasa_confirmados ~ interaccion + factor(semana) + factor(ut)) %>%
    coeftest(., vcov. = vcov(., type = "HC0"))
  estimacion <- rbind(estimacion, coeficiente['interaccion',])
  

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
         lag_1 + lag_2 + lag_3 + factor(semana) + factor(ut)) %>%
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
         lag_1 + lag_2 + lag_3 + factor(semana) + factor(ut)) %>%
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
         lag_1 + lag_2 + lag_3 + factor(semana) + factor(ut)) %>%
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
         lag_1 + lag_2 + lag_3 + factor(semana) + factor(ut)) %>%
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
         lag_1 + lag_2 + lag_3 + factor(semana) + factor(ut)) %>%
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
         lag_1 + lag_2 + lag_3 + factor(semana) + factor(ut)) %>%
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
         lag_1 + lag_2 + lag_3 + factor(semana) + factor(ut)) %>%
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
         lag_1 + lag_2 + lag_3 + factor(semana) + factor(ut)) %>%
    coeftest(., vcov. = vcov(., type = "HC0"))
  leads_n_lags <- rbind(leads_n_lags, 
                        coeficiente[c('interaccion', 'lead_1', 'lead_2', 'lead_3', 
                                      'lag_1', 'lag_2', 'lag_3'),])

  save(estimacion, leads_n_lags, file= 'data/interim/efecto_acumulado.RData')
}
rm(precarga)





