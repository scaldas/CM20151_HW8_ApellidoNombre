library(lubridate)
library(caret)
library(dplyr)

file <- '/Users/caldasrivera/Dropbox/UniAndes/Semestres Academicos/Septimo Semestre/Metodos Computacionales/Tareas/Tarea8/CM20151_HW8_CaldasSebastian/data/training_set.csv'
file_test <- '/Users/caldasrivera/Dropbox/UniAndes/Semestres Academicos/Septimo Semestre/Metodos Computacionales/Tareas/Tarea8/CM20151_HW8_CaldasSebastian/data/test_set.csv'

dat <- read.csv(file,header=T)
dat_test <- read.csv(file_test,header=T)

dat$diasemana <- wday(as.Date(dat$fecha,'%Y-%m-%d'), label=TRUE, abbr = FALSE)
dat_test$diasemana <- wday(as.Date(dat_test$fecha,'%Y-%m-%d'), label=TRUE, abbr = FALSE)
dat$mes <- month(dat$fecha) 
dat_test$mes <- month(dat_test$fecha) 
dat$temp_prom <- (dat$temp_min + dat$temp_max)/2
dat_test$temp_prom <- (dat_test$temp_min + dat_test$temp_max)/2

dias <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')

for (i in 1:7 ) 
{
  dat.dia <- dplyr::filter(dat, diasemana==dias[i])
  dat_test.dia <- dplyr::filter(dat_test, diasemana==dias[i])
  modFit <- train(conteo_ordenes ~ conteo_restaurantes + cod_calendario + temp_min, method = "lm",data=dat.dia)
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

write.csv(sumbission.write, '/Users/caldasrivera/Dropbox/UniAndes/Semestres Academicos/Septimo Semestre/Metodos Computacionales/Tareas/Tarea8/datos/scaldas_submission2.csv', row.names=FALSE)

