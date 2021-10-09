############################################################
############################################################

library(tidyverse)
library(lubridate)
library(splines)
library(MASS)

rm(list = ls())

#####################################################
#####################################################

prueba <- readxl::read_xlsx('data/prueba_eficacia.xlsx')

prueba <- prueba %>% 
  mutate(fecha = as.Date(fecha, origin = '1970-01-01'),
         semana = substr(ISOweek::ISOweek(fecha), 7, 9), 
         dia_semana = factor(wday(fecha, label = TRUE, abbr = FALSE), 
                             ordered = FALSE))
head(prueba, 8)

# El fichero contiene, por día, los casos entre vacunados y no vacunados
# y las personas en riesgo entre vacunados y no vacunados.
# Las personas en riesgo entre no vacunados son la población no vacunada
# en ese día, menos los que han tenido covid anteriormente y menos los 
# han recibido al menos una dosis.
# Las personas en riesgo entre vacunadas son aquellas que han recibido las
# dos dosis y ha pasado un periodo de inducción que es distinto según el 
# tipo de vacuna (Pfizer, etc.) recibida en 2ª dosis y que han respetado
# el periodo establecido entre dosis.

n_semanas <- nrow(prueba)%/%7
table(prueba$semana) %>% length()


# Splines para el ajuste de la tendencia
# Elegido así ya que para estos datos minimiza AIC.
spl <- splines::ns(x = 1:nrow(prueba), knots = 21 * (1:((n_semanas)/3))) 

colnames(spl) <- paste0('spl', 1:ncol(spl))

# Los añadimos al conjunto de datos
prueba <- prueba %>% 
  cbind(spl)

# Manipulamos el conjunto de datos para tener un registro según días y una variable
# indicadora del estado de vacunación

# Para los casos
prueba_casos <- prueba %>% 
  dplyr::select(fecha, dia_semana, starts_with ('spl'), n_casos_novac, n_casos_vacunados) %>% 
  pivot_longer(cols = c(n_casos_vacunados, n_casos_novac), 
               names_to = 'estado_vacunacion', 
               values_to = 'ncasos') %>% 
  mutate(estado_vacunacion = factor(estado_vacunacion == 'n_casos_novac', 
                                    levels = c(TRUE, FALSE), 
                                    labels = c('No vacunados', 'Vacunados')))

# Para las personas en riesgo
prueba_nriesgo <- prueba %>% 
  dplyr::select(fecha, dia_semana, starts_with('spl'), n_riesgo_novacunados, n_riesgo_vacunados) %>% 
  pivot_longer(cols = c(n_riesgo_vacunados, n_riesgo_novacunados), 
               names_to = 'estado_vacunacion', 
               values_to = 'nriesgo') %>% 
  mutate(estado_vacunacion = factor(estado_vacunacion == 'n_riesgo_novacunados', 
                                    levels = c(TRUE, FALSE), 
                                    labels = c('No vacunados', 'Vacunados')))

# Unimos los dataframes
prueba2 <- prueba_casos %>% 
  bind_cols(prueba_nriesgo %>% 
              dplyr::select(nriesgo))

head(prueba2, 6)

#########################################################
# Modelo 1 que no ajusta ni por tendencia ni periodicidad

form <- 'ncasos ~ estado_vacunacion + offset(log(nriesgo))'

form1 <- formula(form)

m1 <- glm.nb(form,
             data = prueba2)

summary(m1)

efectividad <- function(mod){
  efecto <- coef(mod)[grepl('vacuna', names(coef(mod)))]
  efectiv <- round(100 * (1 - exp(efecto)), 2)
  ic <- round(100 * (1 - exp(rev(confint(mod)[grepl('vacuna', names(coef(mod))), ]))), 2)
  efectiv <- paste0(efectiv, ' (', ic[1], ' a ', ic[2], ')')
  names(efectiv) <- 'Efectividad % (IC 95 %)'
  efectiv
} 

efectividad(m1)

# Gráfíco observados vs predichos
pred <- predict(m1, type = 'response')

prueba2 %>% 
  bind_cols(pred = pred) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = fecha, y = pred, color = estado_vacunacion), lwd = 1.35) +
  geom_point(mapping = aes(x = fecha, y = ncasos, color = estado_vacunacion)) +
  scale_x_date('Fecha', 
               breaks = seq(as.Date('2021-01-01'), as.Date('2021-08-31'), by = 'month'),
               date_labels = '%b') + 
  scale_y_continuous('Observados vs predichos según estado vacunación', 
                     breaks = seq(0, 1500, by = 250)) + 
  labs(color = 'Estado vacunación')

# Correlación residuales

res <- residuals(m1, type = 'deviance')

acf1 <- as.vector(acf(res, lag.max = 39)$acf)

Lag <-0:39

acf_df <- tibble(Lag, acf1)

acf_df %>% 
  ggplot(mapping = aes(x = Lag, y = 0)) + 
  geom_segment(aes(xend = Lag, yend = acf1)) + 
  geom_hline(aes(yintercept = 0)) + 
  scale_y_continuous(limits = c(-1, 1)) + 
  geom_hline(aes(yintercept = + 1.96 * 1/sqrt(length(res))), lty = 2, color = "blue") + 
  geom_hline(aes(yintercept = - 1.96 * 1/sqrt(length(res))), lty = 2, color = "blue") +
  ylab("Autocorrelaciones") 

# no es un ruido normal, por lo que las bandas no son adecuadas
# pero puede servir como orientación. Además, no sé si sería más 
# adecuado a length(res)/2

######################################
# Modelo 2 que ajusta por la tendencia

form <- paste0('ncasos ~ estado_vacunacion + ', paste0(colnames(spl), collapse = ' + '), sep = '')
form <- paste0(form, ' + offset(log(nriesgo))')

form1 <- formula(form)

m2 <- glm.nb(form,
             data = prueba2)

summary(m2)

efectividad(m2)

# Gráfíco observados vs predichos
pred <- predict(m2, type = 'response')

prueba2 %>% 
  bind_cols(pred = pred) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = fecha, y = pred, color = estado_vacunacion), lwd = 1.35) +
  geom_point(mapping = aes(x = fecha, y = ncasos, color = estado_vacunacion)) +
  scale_x_date('Fecha', 
               breaks = seq(as.Date('2021-01-01'), as.Date('2021-08-31'), by = 'month'),
               date_labels = '%b') + 
  scale_y_continuous('Observados vs predichos según estado vacunación', 
                     breaks = seq(0, 1500, by = 250)) + 
  labs(color = 'Estado vacunación')

# Correlación residuales

res <- residuals(m2, type = 'deviance')

acf1 <- as.vector(acf(res, lag.max = 39)$acf)

Lag <-0:39

acf_df <- tibble(Lag, acf1)

acf_df %>% 
  ggplot(mapping = aes(x = Lag, y = 0)) + 
  geom_segment(aes(xend = Lag, yend = acf1)) + 
  geom_hline(aes(yintercept = 0)) + 
  scale_y_continuous(limits = c(-1, 1)) + 
  geom_hline(aes(yintercept = + 1.96 * 1/sqrt(length(res))), lty = 2, color = "blue") + 
  geom_hline(aes(yintercept = - 1.96 * 1/sqrt(length(res))), lty = 2, color = "blue") +
  ylab("Autocorrelaciones")

####################################################
# Modelo 3 que ajusta por la tendencia y periodicidad

form <- paste0('ncasos ~ estado_vacunacion + dia_semana +', paste0(colnames(spl), collapse = ' + '), sep = '')
form <- paste0(form, ' + offset(log(nriesgo))')

form1 <- formula(form)

m3 <- glm.nb(form,
             data = prueba2)

summary(m3)

efectividad(m3)

# Gráfíco observados vs predichos
pred <- predict(m3, type = 'response')

prueba2 %>% 
  bind_cols(pred = pred) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = fecha, y = pred, color = estado_vacunacion), lwd = 1.35) +
  geom_point(mapping = aes(x = fecha, y = ncasos, color = estado_vacunacion)) +
  scale_x_date('Fecha', 
               breaks = seq(as.Date('2021-01-01'), as.Date('2021-08-31'), by = 'month'),
               date_labels = '%b') + 
  scale_y_continuous('Observados vs predichos según estado vacunación', 
                     breaks = seq(0, 1500, by = 250)) + 
  labs(color = 'Estado vacunación')

# Correlación residuales

res <- residuals(m3, type = 'deviance')

acf1 <- as.vector(acf(res, lag.max = 39)$acf)

Lag <-0:39

acf_df <- tibble(Lag, acf1)

acf_df %>% 
  ggplot(mapping = aes(x = Lag, y = 0)) + 
  geom_segment(aes(xend = Lag, yend = acf1)) + 
  geom_hline(aes(yintercept = 0)) + 
  scale_y_continuous(limits = c(-1, 1)) + 
  geom_hline(aes(yintercept = + 1.96 * 1/sqrt(length(res))), lty = 2, color = "blue") + 
  geom_hline(aes(yintercept = - 1.96 * 1/sqrt(length(res))), lty = 2, color = "blue") +
  ylab("Autocorrelaciones")


