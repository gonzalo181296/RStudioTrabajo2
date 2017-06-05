library(combinat)
library(prob)
Ejercicio <- data.frame(Ejercicio=c('si','no'),probs=c(0.1,0.9))
Genero<-data.frame(Genero=c('hombre','mujer'),probs=c(0.4,0.6))
Raza<-data.frame(Raza=c('blanco','afroamericano','hispano','asiatico'))
Raza<-probspace(Raza)
Nutricion <- data.frame(Nutricion=c('saludable','nosaludable'),probs=c(0.4,0.6))
Peso <- data.frame(Nutricion=c('normal','sobrepeso'),probs=c(0.2,0.8))

Fuma <- expand.grid(Genero=c('hombre','mujer'),Raza=c('blanco','afroamericano','hispano','asiatico'),
                    Fuma=c('si','no'))
Datos <- read.table(file = 'Fuma.txt')
Fuma<- data.frame(Fuma,Datos)
Fuma<- probspace(Fuma)
for(i in 1:16){
  Fuma$probs[i] = Fuma$V1[i]/sum(Fuma$V1)
}


Presion<-expand.grid(Genero=c('hombre','mujer'),Raza=c('blanco','afroamericano','hispano','asiatico'),
                     Presion=c('alta','otra'))
Datos <- read.table(file = 'Presion.txt')
Presion<- data.frame(Presion,Datos)
Presion<- probspace(Presion)
for(i in 1:16){
  Presion$probs[i] = Presion$V1[i]/sum(Presion$V1)
}

Colesterol<-expand.grid(Genero=c('hombre','mujer'),
                        Colesterol=c('240mg/ml','200mg/ml'))
Datos <- read.table(file = 'Colesterol.txt')
Colesterol<- data.frame(Colesterol,Datos)
Colesterol<- probspace(Colesterol)
for(i in 1:4){
  Colesterol$probs[i] = Colesterol$V1[i]/sum(Colesterol$V1)
}

Glucosa<-expand.grid(Genero=c('hombre','mujer'),Raza=c('blanco','afroamericano','hispano','asiatico'),
                     Glucosa=c('diabetes','nodiabetes'))
Datos <- read.table(file = 'Glucosa.txt')
Glucosa<- data.frame(Glucosa,Datos)
Glucosa<- probspace(Glucosa)
for(i in 1:16){
  Glucosa$probs[i] = Glucosa$V1[i]/sum(Glucosa$V1)
}


Infarto <- expand.grid(Fuma=c('si','no'),Ejercicio=c('si','no'),
                       Peso=c('normal','obeso'),Glucosa=c('diabetes','nodiabetes'),Presion=c('alta','otra'),
                       Colesterol=c('240mg/ml','200mg/ml'),Nutricion=c('saludable','nosaludable'),Infarto=c('si','no'))

Datos <- read.table(file = 'Infarto.txt')
Infarto<-data.frame(Infarto,Datos)
Infarto<-probspace(Infarto)
for(i in 1:256){
  Infarto$probs[i] = Infarto$V1[i]/sum(Infarto$V1)
}

Infarto<-probspace(Infarto)
View(Infarto)