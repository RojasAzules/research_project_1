precarga <- TRUE

if (precarga == TRUE) {
  load('data/interim/resultados_semana.RData')
} else {

  reg_por_semana <- function(sem){
    coeficiente <- casos %>% 
      mutate(tratamiento = semana == sem,
             interaccion = share_Russian * tratamiento,
             interaccion_c1 = average_wage * tratamiento,
             interaccion_c2 = average_age * tratamiento,
             interaccion_c3 = average_schooling * tratamiento,
             interaccion_c4 = women_share * tratamiento) %>% 
      lm(formula = tasa_confirmados ~ 
           interaccion + interaccion_c1 + interaccion_c2 + interaccion_c3 + interaccion_c4 +
           factor(semana) + factor(ut)) %>% 
      coeftest(., vcov. = vcov(., type = "HC0"))
    
    coeficiente['interaccion',]
  }
  
  resultados_por_semana <- sapply(as.list(unique(casos$semana)), reg_por_semana)
  
  # Convertimos resultados a df y agregamos variables de día y semana
  resultados_por_semana <- as.data.frame(t(resultados_por_semana)) %>% 
    mutate(semana = unique(casos$semana)) %>% 
    relocate(semana)
  
  save(resultados_por_semana, file= 'data/interim/resultados_semana.RData')
}