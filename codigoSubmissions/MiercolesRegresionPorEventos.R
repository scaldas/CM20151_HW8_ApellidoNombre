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

dat.cat <- dplyr::filter(dat, categoria_num==1 | categoria_num==2 | categoria_num==0)
dat_test.cat <- dplyr::filter(dat_test,  categoria_num==1 | categoria_num==2 | categoria_num==0)
modFit <- train(conteo_ordenes ~ conteo_restaurantes + cod_calendario, method = "lm",data=dat.cat)
finMod <- modFit$finalModel
pred <- predict(modFit,dat_test.cat)
dat_test.cat$prediction <- round(pred,digits=0)
submission <- dat_test.cat

for (i in 3:5) 
{
  dat.cat <- dplyr::filter(dat, categoria_num==i)
  dat_test.cat <- dplyr::filter(dat_test, categoria_num==i)
  modFit <- train(conteo_ordenes ~ conteo_restaurantes + cod_calendario, method = "lm",data=dat.cat)
  finMod <- modFit$finalModel
  pred <- predict(modFit,dat_test.cat)
  dat_test.cat$prediction <- round(pred,digits=0)
  submission <- dplyr::union(submission, dat_test.cat)
}

sumbission.write <- data.frame(fecha = submission$fecha, conteo_ordenes = submission$prediction)

write.csv(sumbission.write, '/Users/caldasrivera/Dropbox/UniAndes/Semestres Academicos/Septimo Semestre/Metodos Computacionales/Tareas/Tarea8/datos/scaldas_submission3.csv', row.names=FALSE)
