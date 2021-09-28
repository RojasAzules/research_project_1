# El cálculo de las regresiones por día tarda aprox 3 horas, por semana 3 mins
# Active estas banderas si desea volver a correr las regresiones
# De lo contrario se cargaran las que fueron respaldadas en \data\interim
russian_por_dia <- FALSE
russian_por_semana <- FALSE
latvian_por_dia <- FALSE
latvian_por_semana <- FALSE
belarussian_por_dia <- FALSE
belarussian_por_semana <- FALSE
ukranian_por_semana <- FALSE
ukranian_por_dia <- FALSE
polish_por_dia <- FALSE
polish_por_semana <- FALSE
lithuanian_por_semana <- FALSE
lithuanian_por_dia <- FALSE
other_por_semana <- FALSE
other_por_dia <- FALSE
non_latvian_por_semana <- FALSE
non_latvian_por_dia <- FALSE
baltic_por_semana <- FALSE
non_baltic_por_semana <- FALSE


#####
# Cargamos la base
source('src/depurar_data.R')

#####
# Generación de las regresiones considerando indicadoras por dìa y semana
# Russian

if (russian_por_dia) {
  reg_por_dia <- function(dia){
    coeficiente <- casos %>% 
      mutate(tratamiento = Fecha == dia,
             interaccion = share_Russian * tratamiento) %>% 
      lm(formula = tasa_confirmados ~ interaccion + factor(Fecha) + factor(ut)) %>% 
      coeftest(., vcov. = vcov(., type = "HC0"))
    
    coeficiente['interaccion',]
  }

  resultados_por_dia <- sapply(as.list(unique(casos$Fecha)), reg_por_dia)
  save(resultados_por_dia, file= 'data/interim/resultados_dia.RData')
} else {
  load('data/interim/resultados_dia.RData')
}

# Convertimos resultados a df y agregamos variables de día y semana
resultados_por_dia <- as.data.frame(t(resultados_por_dia)) %>% 
  mutate(Fecha = unique(casos$Fecha)) %>% 
  relocate(Fecha)

if (russian_por_semana) {
  reg_por_semana <- function(sem){
    coeficiente <- casos %>% 
      mutate(tratamiento = semana == sem,
             interaccion = share_Russian * tratamiento) %>% 
      lm(formula = tasa_confirmados ~ interaccion + factor(semana) + factor(ut)) %>% 
      coeftest(., vcov. = vcov(., type = "HC0"))
    
    coeficiente['interaccion',]
  }
  
  resultados_por_semana <- sapply(as.list(unique(casos$semana)), reg_por_semana)
  save(resultados_por_semana, file= 'data/interim/resultados_semana.RData')
} else {
  load('data/interim/resultados_semana.RData')
}

# Convertimos resultados a df y agregamos variables de día y semana
resultados_por_semana <- as.data.frame(t(resultados_por_semana)) %>% 
  mutate(semana = unique(casos$semana)) %>% 
  relocate(semana)

#####
# Belarussian
if (belarussian_por_semana) {
  reg_por_semana <- function(sem){
    coeficiente <- casos %>%
      mutate(tratamiento = semana == sem,
             interaccion = share_Belarusian * tratamiento) %>%
      lm(formula = tasa_confirmados ~ interaccion + factor(semana) + factor(ut)) %>%
      coeftest(., vcov. = vcov(., type = "HC0"))

    coeficiente['interaccion',]
  }

  belarussian_por_semana <- sapply(as.list(unique(casos$semana)), reg_por_semana)
  save(belarussian_por_semana, file= 'data/interim/belarussian_semana.RData')

} else {
  load('data/interim/belarussian_semana.RData')
}

belarussian_por_semana <- as.data.frame(t(belarussian_por_semana)) %>%
  mutate(semana = unique(casos$semana)) %>%
  relocate(semana)

if (belarussian_por_dia) {
  reg_por_dia <- function(dia){
    coeficiente <- casos %>% 
      mutate(tratamiento = Fecha == dia,
             interaccion = share_Belarusian * tratamiento) %>% 
      lm(formula = tasa_confirmados ~ interaccion + factor(Fecha) + factor(ut)) %>% 
      coeftest(., vcov. = vcov(., type = "HC0"))
    
    coeficiente['interaccion',]
  }
  
  belarussian_por_dia <- sapply(as.list(unique(casos$Fecha)), reg_por_dia)
  save(belarussian_por_dia, file= 'data/interim/belarussian_dia.RData')
} else {
  load('data/interim/belarussian_dia.RData')
}

belarussian_por_dia <- as.data.frame(t(belarussian_por_dia)) %>%
  mutate(Fecha = unique(casos$Fecha)) %>% 
  relocate(Fecha)

#####
# Latvian
if (latvian_por_semana) {
  reg_por_semana <- function(sem){
    coeficiente <- casos %>%
      mutate(tratamiento = semana == sem,
             interaccion = share_Latvian * tratamiento) %>%
      lm(formula = tasa_confirmados ~ interaccion + factor(semana) + factor(ut)) %>%
      coeftest(., vcov. = vcov(., type = "HC0"))
    
    coeficiente['interaccion',]
  }
  
  latvian_por_semana <- sapply(as.list(unique(casos$semana)), reg_por_semana)
  save(latvian_por_semana, file= 'data/interim/latvian_semana.RData')
  
} else {
  load('data/interim/latvian_semana.RData')
}

latvian_por_semana <- as.data.frame(t(latvian_por_semana)) %>%
  mutate(semana = unique(casos$semana)) %>%
  relocate(semana)

if (latvian_por_dia) {
  reg_por_dia <- function(dia){
    coeficiente <- casos %>% 
      mutate(tratamiento = Fecha == dia,
             interaccion = share_Latvian * tratamiento) %>% 
      lm(formula = tasa_confirmados ~ interaccion + factor(Fecha) + factor(ut)) %>% 
      coeftest(., vcov. = vcov(., type = "HC0"))
    
    coeficiente['interaccion',]
  }
  
  latvian_por_dia <- sapply(as.list(unique(casos$Fecha)), reg_por_dia)
  save(latvian_por_dia, file= 'data/interim/latvian_dia.RData')
} else {
  load('data/interim/latvian_dia.RData')
}

latvian_por_dia <- as.data.frame(t(latvian_por_dia)) %>%
  mutate(Fecha = unique(casos$Fecha)) %>% 
  relocate(Fecha)


#####
# Ukranian
if (ukranian_por_semana) {
  reg_por_semana <- function(sem){
    coeficiente <- casos %>%
      mutate(tratamiento = semana == sem,
             interaccion = share_Ukrainian * tratamiento) %>%
      lm(formula = tasa_confirmados ~ interaccion + factor(semana) + factor(ut)) %>%
      coeftest(., vcov. = vcov(., type = "HC0"))
    
    coeficiente['interaccion',]
  }
  
  ukranian_por_semana <- sapply(as.list(unique(casos$semana)), reg_por_semana)
  save(ukranian_por_semana, file= 'data/interim/ukranian_semana.RData')
  
} else {
  load('data/interim/ukranian_semana.RData')
}

ukranian_por_semana <- as.data.frame(t(ukranian_por_semana)) %>%
  mutate(semana = unique(casos$semana)) %>%
  relocate(semana)
  
if (ukranian_por_dia) {
  reg_por_dia <- function(dia){
    coeficiente <- casos %>% 
      mutate(tratamiento = Fecha == dia,
             interaccion = share_Ukrainian * tratamiento) %>% 
      lm(formula = tasa_confirmados ~ interaccion + factor(Fecha) + factor(ut)) %>% 
      coeftest(., vcov. = vcov(., type = "HC0"))
    
    coeficiente['interaccion',]
  }
  
  ukranian_por_dia <- sapply(as.list(unique(casos$Fecha)), reg_por_dia)
  save(ukranian_por_dia, file= 'data/interim/ukranian_dia.RData')
} else {
  load('data/interim/ukranian_dia.RData')
}

ukranian_por_dia <- as.data.frame(t(ukranian_por_dia)) %>%
  mutate(Fecha = unique(casos$Fecha)) %>% 
  relocate(Fecha)



#####
# Polish
if (polish_por_semana) {
  reg_por_semana <- function(sem){
    coeficiente <- casos %>%
      mutate(tratamiento = semana == sem,
             interaccion = share_Polish * tratamiento) %>%
      lm(formula = tasa_confirmados ~ interaccion + factor(semana) + factor(ut)) %>%
      coeftest(., vcov. = vcov(., type = "HC0"))
    
    coeficiente['interaccion',]
  }
  
  polish_por_semana <- sapply(as.list(unique(casos$semana)), reg_por_semana)
  save(polish_por_semana, file= 'data/interim/polish_semana.RData')
  
} else {
  load('data/interim/polish_semana.RData')
}

polish_por_semana <- as.data.frame(t(polish_por_semana)) %>%
  mutate(semana = unique(casos$semana)) %>%
  relocate(semana)

if (polish_por_dia) {
  reg_por_dia <- function(dia){
    coeficiente <- casos %>% 
      mutate(tratamiento = Fecha == dia,
             interaccion = share_Polish * tratamiento) %>% 
      lm(formula = tasa_confirmados ~ interaccion + factor(Fecha) + factor(ut)) %>% 
      coeftest(., vcov. = vcov(., type = "HC0"))
    
    coeficiente['interaccion',]
  }
  
  polish_por_dia <- sapply(as.list(unique(casos$Fecha)), reg_por_dia)
  save(polish_por_dia, file= 'data/interim/polish_dia.RData')
} else {
  load('data/interim/polish_dia.RData')
}

polish_por_dia <- as.data.frame(t(polish_por_dia)) %>%
  mutate(Fecha = unique(casos$Fecha)) %>% 
  relocate(Fecha)



#####
# Lithuanian
if (lithuanian_por_semana) {
  reg_por_semana <- function(sem){
    coeficiente <- casos %>%
      mutate(tratamiento = semana == sem,
             interaccion = share_Lithuanian * tratamiento) %>%
      lm(formula = tasa_confirmados ~ interaccion + factor(semana) + factor(ut)) %>%
      coeftest(., vcov. = vcov(., type = "HC0"))
    
    coeficiente['interaccion',]
  }
  
  lithuanian_por_semana <- sapply(as.list(unique(casos$semana)), reg_por_semana)
  save(lithuanian_por_semana, file= 'data/interim/lithuanian_semana.RData')
  
} else {
  load('data/interim/lithuanian_semana.RData')
}

lithuanian_por_semana <- as.data.frame(t(lithuanian_por_semana)) %>%
  mutate(semana = unique(casos$semana)) %>%
  relocate(semana)

if (lithuanian_por_dia) {
  reg_por_dia <- function(dia){
    coeficiente <- casos %>% 
      mutate(tratamiento = Fecha == dia,
             interaccion = share_Lithuanian * tratamiento) %>% 
      lm(formula = tasa_confirmados ~ interaccion + factor(Fecha) + factor(ut)) %>% 
      coeftest(., vcov. = vcov(., type = "HC0"))
    
    coeficiente['interaccion',]
  }
  
  lithuanian_por_dia <- sapply(as.list(unique(casos$Fecha)), reg_por_dia)
  save(lithuanian_por_dia, file= 'data/interim/lithuanian_dia.RData')
} else {
  load('data/interim/lithuanian_dia.RData')
}

lithuanian_por_dia <- as.data.frame(t(lithuanian_por_dia)) %>%
  mutate(Fecha = unique(casos$Fecha)) %>% 
  relocate(Fecha)



#####
# Other
if (other_por_semana) {
  reg_por_semana <- function(sem){
    coeficiente <- casos %>%
      mutate(tratamiento = semana == sem,
             interaccion = share_Other * tratamiento) %>%
      lm(formula = tasa_confirmados ~ interaccion + factor(semana) + factor(ut)) %>%
      coeftest(., vcov. = vcov(., type = "HC0"))
    
    coeficiente['interaccion',]
  }
  
  other_por_semana <- sapply(as.list(unique(casos$semana)), reg_por_semana)
  save(other_por_semana, file= 'data/interim/other_semana.RData')
  
} else {
  load('data/interim/other_semana.RData')
}

other_por_semana <- as.data.frame(t(other_por_semana)) %>%
  mutate(semana = unique(casos$semana)) %>%
  relocate(semana)

if (other_por_dia) {
  reg_por_dia <- function(dia){
    coeficiente <- casos %>% 
      mutate(tratamiento = Fecha == dia,
             interaccion = share_Other * tratamiento) %>% 
      lm(formula = tasa_confirmados ~ interaccion + factor(Fecha) + factor(ut)) %>% 
      coeftest(., vcov. = vcov(., type = "HC0"))
    
    coeficiente['interaccion',]
  }
  
  other_por_dia <- sapply(as.list(unique(casos$Fecha)), reg_por_dia)
  save(other_por_dia, file= 'data/interim/other_dia.RData')
} else {
  load('data/interim/other_dia.RData')
}

other_por_dia <- as.data.frame(t(other_por_dia)) %>%
  mutate(Fecha = unique(casos$Fecha)) %>% 
  relocate(Fecha)

#####
# Non Latvian

if (non_latvian_por_semana) {
  reg_por_semana <- function(sem){
    coeficiente <- casos %>%
      mutate(tratamiento = semana == sem,
             interaccion = (1-share_Latvian) * tratamiento) %>%
      lm(formula = tasa_confirmados ~ interaccion + factor(semana) + factor(ut)) %>%
      coeftest(., vcov. = vcov(., type = "HC0"))
    
    coeficiente['interaccion',]
  }
  
  non_latvian_por_semana <- sapply(as.list(unique(casos$semana)), reg_por_semana)
  save(non_latvian_por_semana, file= 'data/interim/non_latvian_semana.RData')
  
} else {
  load('data/interim/non_latvian_semana.RData')
}

non_latvian_por_semana <- as.data.frame(t(non_latvian_por_semana)) %>%
  mutate(semana = unique(casos$semana)) %>%
  relocate(semana)

if (non_latvian_por_dia) {
  reg_por_dia <- function(dia){
    coeficiente <- casos %>% 
      mutate(tratamiento = Fecha == dia,
             interaccion = (1-share_Latvian) * tratamiento) %>% 
      lm(formula = tasa_confirmados ~ interaccion + factor(Fecha) + factor(ut)) %>% 
      coeftest(., vcov. = vcov(., type = "HC0"))
    
    coeficiente['interaccion',]
  }
  
  non_latvian_por_dia <- sapply(as.list(unique(casos$Fecha)), reg_por_dia)
  save(non_latvian_por_dia, file= 'data/interim/non_latvian_dia.RData')
} else {
  load('data/interim/non_latvian_dia.RData')
}

non_latvian_por_dia <- as.data.frame(t(non_latvian_por_dia)) %>%
  mutate(Fecha = unique(casos$Fecha)) %>% 
  relocate(Fecha)

#####
# Baltic & Non Baltic

if (baltic_por_semana) {
  reg_por_semana <- function(sem){
    coeficiente <- casos %>%
      mutate(tratamiento = semana == sem,
             interaccion = (share_Latvian+share_Lithuanian) * tratamiento) %>%
      lm(formula = tasa_confirmados ~ interaccion + factor(semana) + factor(ut)) %>%
      coeftest(., vcov. = vcov(., type = "HC0"))
    
    coeficiente['interaccion',]
  }
  
  baltic_por_semana <- sapply(as.list(unique(casos$semana)), reg_por_semana)
  save(baltic_por_semana, file= 'data/interim/baltic_semana.RData')
  
} else {
  load('data/interim/baltic_semana.RData')
}

baltic_por_semana <- as.data.frame(t(baltic_por_semana)) %>%
  mutate(semana = unique(casos$semana)) %>%
  relocate(semana)



if (non_baltic_por_semana) {
  reg_por_semana <- function(sem){
    coeficiente <- casos %>%
      mutate(tratamiento = semana == sem,
             interaccion = (1-share_Latvian-share_Lithuanian) * tratamiento) %>%
      lm(formula = tasa_confirmados ~ interaccion + factor(semana) + factor(ut)) %>%
      coeftest(., vcov. = vcov(., type = "HC0"))
    
    coeficiente['interaccion',]
  }
  
  non_baltic_por_semana <- sapply(as.list(unique(casos$semana)), reg_por_semana)
  save(non_baltic_por_semana, file= 'data/interim/non_baltic_semana.RData')
  
} else {
  load('data/interim/non_baltic_semana.RData')
}

non_baltic_por_semana <- as.data.frame(t(non_baltic_por_semana)) %>%
  mutate(semana = unique(casos$semana)) %>%
  relocate(semana)


