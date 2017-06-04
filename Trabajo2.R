#REDES BAYESIANA
library(combinat)
library(prob)
Deporte<-data.frame(Deporte=c('si','no'),probs=c(0.1,0.9))
Alimentacion<-data.frame(Alimentacion=c('equilibrada','noequilibrada'),probs=c(0.4,0.6))
Fumador<-data.frame(Fumador=c('si','no'),probs=c(0.4,0.6))
Presion<-expand.grid(Alimentacion=c('equilibrada','noequilibrada'),Deporte=c('si','no'),Presion=c('alta','normal'))
Datos<-read.table(file='Datos.txt',col.names = 'Datos')
Presion<-data.frame(Presion,Datos)
Presion<-probspace(Presion)
for(i in 1:8)
{
  Presion$probs[i]= Presion$Datos[i]/sum(Presion$Datos)
}
Infarto<-expand.grid(Presion=c('alta','normal'),Fumador=c('si','no'))
Datos<-read.table(file='Datos2.txt',col.names = 'Datos')
Infarto<-data.frame(Infarto,Datos)
Infarto<-probspace(Infarto)
for(i in 1:8)
{
  Infarto$probs[i]= Infarto$Datos[i]/sum(Infarto$Datos)
}



