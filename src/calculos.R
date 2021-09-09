# El cálculo de las regresiones por día tarda aprox 3 horas, por semana media hora
# Active esta bandera si desea volver a correr las regresiones
# De lo contrario se cargaran las que fueron respaldadas en \data\interim
tenemosTiempo <- FALSE

#####
# Cargamos la base
source('src/depurar_data.R')

#####
# Generación de las regresiones considerando indicadoras por dìa y semana

if (tenemosTiempo) {
  reg_por_dia <- function(dia){
    coeficiente <- casos %>% 
      mutate(tratamiento = Fecha == dia,
             interaccion = share_Russian * tratamiento) %>% 
      lm(formula = tasa_confirmados ~ interaccion + factor(Fecha) + factor(ut)) %>% 
      coeftest(., vcov. = vcov(., type = "HC0"))
    
    coeficiente['interaccion',]
  }
  
  reg_por_semana <- function(sem){
    coeficiente <- casos %>% 
      mutate(tratamiento = semana == sem,
             interaccion = share_Russian * tratamiento) %>% 
      lm(formula = tasa_confirmados ~ interaccion + factor(Fecha) + factor(ut)) %>% 
      coeftest(., vcov. = vcov(., type = "HC0"))
    
    coeficiente['interaccion',]
  }
  
  resultados_por_semana <- sapply(as.list(unique(casos$semana)), reg_por_semana)
  save(resultados_por_semana, file= 'data/interim/resultados_semana.RData')
  
  resultados_por_dia <- sapply(as.list(unique(casos$Fecha)), reg_por_dia)
  save(resultados_por_dia, file= 'data/interim/resultados_dia.RData')
} else {
  load('data/interim/resultados_semana.RData')
  load('data/interim/resultados_dia.RData')
}

#####
# Convertimos resultados a df y agregamos variables de día y semana
por_dia <- as.data.frame(t(resultados_por_dia)) %>% 
  mutate(Fecha = unique(casos$Fecha)) %>% 
  relocate(Fecha)
por_semana <- as.data.frame(t(resultados_por_semana)) %>% 
  mutate(semana = unique(casos$semana)) %>% 
  relocate(semana)

