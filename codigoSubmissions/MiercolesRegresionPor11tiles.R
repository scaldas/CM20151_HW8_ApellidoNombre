library(lubridate)
library(caret)
library(dplyr)

file <- '/Users/caldasrivera/Dropbox/UniAndes/Semestres Academicos/Septimo Semestre/Metodos Computacionales/Tareas/Tarea8/CM20151_HW8_CaldasSebastian/data/training_set.csv'
file_test <- '/Users/caldasrivera/Dropbox/UniAndes/Semestres Academicos/Septimo Semestre/Metodos Computacionales/Tareas/Tarea8/CM20151_HW8_CaldasSebastian/data/test_set.csv'

dat <- read.csv(file,header=T)
dat_test <- read.csv(file_test,header=T)

dat$diasemana <- wday(as.Date(dat$fecha,'%Y-%m-%d'), label=TRUE, abbr = FALSE)
dat_test$diasemana <- wday(as.Date(dat_test$fecha,'%Y-%m-%d'), label=TRUE, abbr = FALSE)

dat <- dplyr::mutate(dat, categoria = ifelse(dat$eventos=='Niebla' | dat$eventos== 'Niebla-Lluvia-Nieve' | dat$eventos== 'Niebla-Nieve' | dat$eventos== 'Lluvia-Tormenta', 'Otros', ifelse(dat$eventos=='Lluvia-Nieve','Lluvia-Nieve', ifelse(dat$eventos=='Niebla-Lluvia','Niebla-Lluvia',ifelse(dat$eventos=='Nieve','Nieve',ifelse(dat$eventos=='Lluvia', 'Lluvia', 'Ninguno'))))))
dat_test <- dplyr::mutate(dat_test, categoria = ifelse(dat_test$eventos=='Niebla' | dat_test$eventos== 'Niebla-Lluvia-Nieve' | dat_test$eventos== 'Niebla-Nieve' | dat_test$eventos== 'Lluvia-Tormenta', 'Otros', ifelse(dat_test$eventos=='Lluvia-Nieve','Lluvia-Nieve', ifelse(dat_test$eventos=='Niebla-Lluvia','Niebla-Lluvia',ifelse(dat_test$eventos=='Nieve','Nieve',ifelse(dat_test$eventos=='Lluvia', 'Lluvia', 'Ninguno'))))))

dat <- dplyr::mutate(dat, categoria_num = ifelse(dat$categoria=='Otros', 2, ifelse(dat$categoria=='Lluvia-Nieve',0, ifelse(dat$categoria=='Niebla-Lluvia',1,ifelse(dat$categoria=='Nieve',3,ifelse(dat$categoria=='Lluvia', 4, 5))))))
dat_test <- dplyr::mutate(dat_test, categoria_num = ifelse(dat_test$categoria=='Otros', 2, ifelse(dat_test$categoria=='Lluvia-Nieve',0, ifelse(dat_test$categoria=='Niebla-Lluvia',1,ifelse(dat_test$categoria=='Nieve',3,ifelse(dat_test$categoria=='Lluvia', 4, 5))))))

dat$temp_prom <- (dat$temp_min + dat$temp_max)/2
dat_test$temp_prom <- (dat_test$temp_min + dat_test$temp_max)/2

temps <- dplyr::select(dat, temp_prom)
temps$original <- 'training'
temps_test <- dplyr::select(dat_test, temp_prom)
temps_test$original <- 'test'
temps_tot <- dplyr::bind_rows(temps_test, temps)
temps_tot$ntiles <- ntile(temps_tot$temp_prom, 11)

precips <- dplyr::select(dat, precipitacion)
precips$original <- 'training'
precips_test <- dplyr::select(dat_test, precipitacion)
precips_test$original <- 'test'
precips_tot <- dplyr::bind_rows(precips_test, precips)
precips_tot$ntiles <- ntile(precips_tot$precipitacion, 11)

dat <- dplyr::mutate(dat, perc_temp = (dplyr::filter(temps_tot, original=='training'))$ntiles)
dat_test <- dplyr::mutate(dat_test, perc_temp = (dplyr::filter(temps_tot, original=='test'))$ntiles)

dat <- dplyr::mutate(dat, perc_precips = (dplyr::filter(precips_tot, original=='training'))$ntiles)
dat_test <- dplyr::mutate(dat_test, perc_precips = (dplyr::filter(precips_tot, original=='test'))$ntiles)

dias <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')

for (i in 1:7 ) 
{
  dat.dia <- dplyr::filter(dat, diasemana==dias[i])
  dat_test.dia <- dplyr::filter(dat_test, diasemana==dias[i])
  modFit <- train(conteo_ordenes ~ conteo_restaurantes + cod_calendario + perc_temp + perc_precips, method = "lm",data=dat.dia)
  finMod <- modFit$finalModel
  pred <- predict(modFit,dat_test.dia)
  dat_test.dia$prediction <- round(pred,digits=0)
  if (i == 1)
  {
    submission <- dat_test.dia
  }
  else
  {
    submission <- dplyr::union(submission, dat_test.dia)
  }
}

sumbission.write <- data.frame(fecha = submission$fecha, conteo_ordenes = submission$prediction)

write.csv(sumbission.write, '/Users/caldasrivera/Dropbox/UniAndes/Semestres Academicos/Septimo Semestre/Metodos Computacionales/Tareas/Tarea8/datos/scaldas_submission3.csv', row.names=FALSE)
