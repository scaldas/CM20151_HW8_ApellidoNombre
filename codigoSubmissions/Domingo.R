library(lubridate)
library(dplyr)
library(ggplot2)
library(caret)

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

#featurePlot(x=dat[,c("conteo_restaurantes","temp_max","temp_prom", "precipitacion")],
            y = dat$conteo_ordenes,
            plot="pairs")

#ggplot(dat, aes(x=fecha, y=conteo_ordenes)) + geom_point(shape=1) + facet_wrap( ~ diasemana, ncol=2)
#ggplot(dat, aes(x=fecha, y=conteo_ordenes)) + geom_point(shape=1) + facet_wrap( ~ mes, ncol=2)
#ggplot(dat, aes(x=fecha, y=conteo_ordenes)) + geom_point(shape=1) + facet_wrap( ~ cod_calendario, ncol=2)
#ggplot(dat, aes(x=fecha, y=conteo_ordenes)) + geom_point(shape=1) + facet_wrap( ~ eventos, ncol=2)

modFit <- train(conteo_ordenes ~ conteo_restaurantes + diasemana + cod_calendario + mes, method = "lm",data=dat)
finMod <- modFit$finalModel
pred <- predict(modFit,dat_test)
dat_test$prediction <- round(pred,digits=0)

submission <- data.frame(fecha = dat_test$fecha, conteo_ordenes = dat_test$prediction)
write.csv(submission, '/Users/caldasrivera/Dropbox/UniAndes/Semestres Academicos/Septimo Semestre/Metodos Computacionales/Tareas/Tarea8/datos/scaldas_submission1.csv', row.names=FALSE)

