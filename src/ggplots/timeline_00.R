
#####
source('src/calculos.R')

#####
# Base coeficientes
coeficientes <- resultados_por_semana %>% 
  filter(semana != 202053,
         semana != 202132) %>%
  mutate(
    Date = case_when( # Reconvirtiendo de número de semana a fecha
      semana-202000 < 100 ~ as.Date(paste(floor(semana/100), # Si está en 2020
                                          semana-202000, 1, sep="-"), "%Y-%U-%u"),
      TRUE ~ as.Date(paste(floor(semana/100), # Si está en 2021
                           semana-202100, 1, sep="-"), "%Y-%U-%u")),
    lower = Estimate - `Std. Error`*qnorm(1-0.05/2), # cota inferior para 
    upper = Estimate + `Std. Error`*qnorm(1-0.05/2),
    significancia = as.factor(case_when(
      (0.05 < `Pr(>|t|)`                     ) ~ 'No',
      (0.01 < `Pr(>|t|)` & `Pr(>|t|)` <= 0.05) ~ '5%',
      (                    `Pr(>|t|)` <= 0.01) ~ '1%'))) %>% 
  left_join(eventos_semanales %>% rename(date_text = Date), 
            by = "semana")


#####
coeficientes %>% 
  filter(Date < ymd("2020-07-01")) %>% 
  ggplot(aes(x = Date, y = Estimate, colour = significancia)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  geom_vline(xintercept = ymd("2020-03-12"), # Inicio del lockdown
             color = "#29211F",
             alpha = 0.75,
             linetype = "longdash") +
  geom_vline(xintercept = ymd("2020-06-10"), # Fin del lockdown
             color = "#29211F",
             alpha = 0.75,
             linetype = "longdash") +
  geom_point(aes(x = ymd(date_text), y = 0), # Eventos, puntos
             size = 0.75, color = "black", na.rm = TRUE) +
  geom_segment(aes(x = ymd(date_text), y = Position, yend = 0, xend = ymd(date_text)),  # Eventos, lìneas
               color = 'black', 
               size = 0.5,
               linetype = "dashed",
               na.rm = TRUE) +
  geom_richtext(aes(x = ymd(date_text),  # Eventos, texto
                    y = Position, 
                    hjust = 0, 
                    vjust = 1, 
                    label = Note),
                color = "black",
                na.rm = TRUE) + 
  ylab('New cases') + 
  labs(title = 'Weekly evolution of new cases per 100 thou. hab.',
       subtitle = 'Given the share of Russian Speaking Population of a territorial unit',
       colour = 'Significance') +
  scale_color_manual(values = wes_palette(n = 4, name = "Moonrise2")) +
  scale_x_date(date_breaks = "1 week",
               limits = c(ymd("2020-03-10"), ymd("2020-07-01"))) +
  theme(axis.text.x = element_text(angle=45, hjust = 1),
        legend.position = "bottom") +
  ylim(-90,10)


#####
coeficientes %>% 
  filter(ymd("2020-07-01") < Date, Date < ymd("2020-10-01")) %>% 
  ggplot(aes(x = Date, y = Estimate, colour = significancia)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  geom_point(aes(x = ymd(date_text), y = 0), # Eventos, puntos
             size = 0.75, color = "black", na.rm = TRUE) +
  geom_segment(aes(x = ymd(date_text), y = Position, yend = 0, xend = ymd(date_text)),  # Eventos, lìneas
               color = 'black', 
               size = 0.5,
               linetype = "dashed",
               na.rm = TRUE) +
  geom_richtext(aes(x = ymd(date_text),  # Eventos, texto
                    y = Position, 
                    hjust = 0, 
                    vjust = 1, 
                    label = Note),
                color = "black",
                na.rm = TRUE) + 
  ylab('New cases') + 
  labs(title = 'Weekly evolution of new cases per 100 thou. hab.',
       subtitle = 'Given the share of Russian Speaking Population of a territorial unit',
       colour = 'Significance') +
  scale_color_manual(values = wes_palette(n = 4, name = "Moonrise2")) +
  scale_x_date(date_breaks = "1 week",
               limits = c(ymd("2020-07-01"), ymd("2020-10-01"))) +
  theme(axis.text.x = element_text(angle=45, hjust = 1),
        legend.position = "bottom") +
  ylim(-70,60)



#####
coeficientes %>% 
  filter(ymd("2020-07-01") < Date, Date < ymd("2020-10-01")) %>% 
  ggplot(aes(x = Date, y = Estimate, colour = significancia)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  geom_point(aes(x = ymd(date_text), y = 0), # Eventos, puntos
             size = 0.75, color = "black", na.rm = TRUE) +
  geom_segment(aes(x = ymd(date_text), y = Position, yend = 0, xend = ymd(date_text)),  # Eventos, lìneas
               color = 'black', 
               size = 0.5,
               linetype = "dashed",
               na.rm = TRUE) +
  geom_richtext(aes(x = ymd(date_text),  # Eventos, texto
                    y = Position, 
                    hjust = 0, 
                    vjust = 1, 
                    label = Note),
                color = "black",
                na.rm = TRUE) + 
  ylab('New cases') + 
  labs(title = 'Weekly evolution of new cases per 100 thou. hab.',
       subtitle = 'Given the share of Russian Speaking Population of a territorial unit',
       colour = 'Significance') +
  scale_color_manual(values = wes_palette(n = 4, name = "Moonrise2")) +
  scale_x_date(date_breaks = "1 week",
               limits = c(ymd("2020-07-01"), ymd("2020-10-01"))) +
  theme(axis.text.x = element_text(angle=45, hjust = 1),
        legend.position = "bottom") +
  ylim(-70,60)



#####
coeficientes %>% 
  filter(ymd("2020-10-01") < Date, Date < ymd("2021-02-01")) %>% 
  ggplot(aes(x = Date, y = Estimate, colour = significancia)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  geom_point(aes(x = ymd(date_text), y = 0), # Eventos, puntos
             size = 0.75, color = "black", na.rm = TRUE) +
  geom_segment(aes(x = ymd(date_text), y = Position, yend = 0, xend = ymd(date_text)),  # Eventos, lìneas
               color = 'black', 
               size = 0.5,
               linetype = "dashed",
               na.rm = TRUE) +
  geom_richtext(aes(x = ymd(date_text),  # Eventos, texto
                    y = Position,
                    hjust = 0,
                    vjust = 1,
                    label = Note),
                color = "black",
                na.rm = TRUE) +
  ylab('New cases') + 
  labs(title = 'Weekly evolution of new cases per 100 thou. hab.',
       subtitle = 'Given the share of Russian Speaking Population of a territorial unit',
       colour = 'Significance') +
  scale_color_manual(values = wes_palette(n = 4, name = "Moonrise2")) +
  scale_x_date(date_breaks = "1 week",
               limits = c(ymd("2020-10-01"), ymd("2021-02-01"))) +
  theme(axis.text.x = element_text(angle=45, hjust = 1),
        legend.position = "bottom") + 
  ylim(-80,120)






