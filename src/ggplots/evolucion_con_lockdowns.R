#####
source('src/calculos.R')

#####
resultados_por_semana %>% 
  filter(semana != 202053,
         semana != 202132) %>%
  mutate( # Generamos variables Date e intervalos de confianza y significancia para los coeficientes
    Date = case_when( # Reconvirtiendo de número de semana a fecha
      semana-202000 < 100 ~ as.Date(paste(floor(semana/100), # Si está en 2020
                                          semana-202000, 1, sep="-"), "%Y-%U-%u"),
      TRUE ~ as.Date(paste(floor(semana/100), # Si está en 2021
                           semana-202100, 1, sep="-"), "%Y-%U-%u")),
    lower = Estimate - `Std. Error`*qnorm(1-0.05/2),
    upper = Estimate + `Std. Error`*qnorm(1-0.05/2),
    significancia = as.factor(case_when(
      (0.05 < `Pr(>|t|)`                     ) ~ 'No',
      (0.01 < `Pr(>|t|)` & `Pr(>|t|)` <= 0.05) ~ '5%',
      (                    `Pr(>|t|)` <= 0.01) ~ '1%'))) %>% 
  filter(Date > ymd("2020-11-01"), Date < ymd("2021-04-15")) %>% # Ventana de interès
  ggplot(aes(x = Date, y = Estimate, colour = significancia)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) + # Coeficientes semanales
  geom_vline(xintercept = ymd("2020-11-09"), # Inicio del lockdown
             color = "#29211F",
             alpha = 0.75,
             linetype = "longdash") +
  geom_vline(xintercept = ymd("2021-04-07"), # Fin del lockdown
             color = "#29211F",
             alpha = 0.75,
             linetype = "longdash") +
  geom_vline(xintercept = ymd("2020-12-30"), # Inicio del 1st curfew
             color = "#FF0000",
             alpha = 0.75,
             linetype = "dashed") +
  geom_vline(xintercept = ymd("2021-01-04"), # Fin del 1st curfew
             color = "#FF0000",
             alpha = 0.75,
             linetype = "dashed") +
  geom_vline(xintercept = ymd("2021-01-08"), # Inicio del 2nd curfew
             color = "#00A08A",
             alpha = 0.75,
             linetype = "dashed") +
  geom_vline(xintercept = ymd("2021-01-10"), # Fin del 2nd curfew
             color = "#00A08A",
             alpha = 0.75,
             linetype = "dashed") +
  ylab('New cases') + 
  labs(title = 'New cases per 100 thou. hab.',
       subtitle = 'Given the share of RSP',
       colour = 'Significance') +
  scale_color_manual(values = wes_palette(n = 4, name = "Moonrise2")) + 
  scale_x_date(date_breaks = "2 weeks",
               limits = c(ymd("2020-11-01"), ymd("2021-04-15"))) +
  theme(axis.text.x = element_text(angle=45, hjust = 1),
        legend.position = "bottom")
