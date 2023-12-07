# Ana Sofia Bello Dueñas
# Parcial 1 Series de Tiempo
# Punto 2

# Ejercicio de simulación 
library(TSA)
library(lmtest)
library(forecast)
library(tseries)

# T=100 ####

## phi=0.5 ####
phi=0.5 # determinamos el valor de phi
Tlength=100 # determinamos el tamaño de la muestra

set.seed(276) #fijamos la semilla
sesgo_est<-c()
desv_est<-c()
acf_est<-c()

for(i in 1:500){
y=arima.sim(list(order =c(1,0,0),ar=c(phi)),n=Tlength)
hola<-acf(y)[1]
acf_est[i]<-as.numeric(hola$acf)
sesgo_est[i]<-as.numeric(hola$acf)-phi
desv_est[i]<-(as.numeric(hola$acf)-phi)^2
}

sesgo<-mean(sesgo_est);sesgo
desv<-mean(desv_est);desv
norm<-tseries::jarque.bera.test(acf_est)
p_valor<-norm$p.value;p_valor

## phi= -0.5 ####
phi=-0.5 # determinamos el valor de phi
Tlength=100 # determinamos el tamaño de la muestra

set.seed(276) #fijamos la semilla
sesgo_est<-c()
desv_est<-c()
acf_est<-c()

for(i in 1:500){
  y=arima.sim(list(order =c(1,0,0),ar=c(phi)),n=Tlength)
  hola<-acf(y)[1]
  acf_est[i]<-as.numeric(hola$acf)
  sesgo_est[i]<-as.numeric(hola$acf)-phi
  desv_est[i]<-(as.numeric(hola$acf)-phi)^2
}

sesgo<-mean(sesgo_est);sesgo
desv<-mean(desv_est);desv
norm<-tseries::jarque.bera.test(acf_est)
p_valor<-norm$p.value;p_valor

# T=200 ####

## phi=0.5 ####
phi=0.5 # determinamos el valor de phi
Tlength=200 # determinamos el tamaño de la muestra

set.seed(276) #fijamos la semilla
sesgo_est<-c()
desv_est<-c()
acf_est<-c()

for(i in 1:500){
  y=arima.sim(list(order =c(1,0,0),ar=c(phi)),n=Tlength)
  hola<-acf(y)[1]
  acf_est[i]<-as.numeric(hola$acf)
  sesgo_est[i]<-as.numeric(hola$acf)-phi
  desv_est[i]<-(as.numeric(hola$acf)-phi)^2
}

sesgo<-mean(sesgo_est);sesgo
desv<-mean(desv_est);desv
norm<-tseries::jarque.bera.test(acf_est)
p_valor<-norm$p.value;p_valor

## phi= -0.5 ####
phi=-0.5 # determinamos el valor de phi
Tlength=200 # determinamos el tamaño de la muestra

set.seed(276) #fijamos la semilla
sesgo_est<-c()
desv_est<-c()
acf_est<-c()

for(i in 1:500){
  y=arima.sim(list(order =c(1,0,0),ar=c(phi)),n=Tlength)
  hola<-acf(y)[1]
  acf_est[i]<-as.numeric(hola$acf)
  sesgo_est[i]<-as.numeric(hola$acf)-phi
  desv_est[i]<-(as.numeric(hola$acf)-phi)^2
}

sesgo<-mean(sesgo_est);sesgo
desv<-mean(desv_est);desv
norm<-tseries::jarque.bera.test(acf_est)
p_valor<-norm$p.value;p_valor

# T=500 ####

## phi=0.5 ####
phi=0.5 # determinamos el valor de phi
Tlength=500 # determinamos el tamaño de la muestra

set.seed(276) #fijamos la semilla
sesgo_est<-c()
desv_est<-c()
acf_est<-c()

for(i in 1:500){
  y=arima.sim(list(order =c(1,0,0),ar=c(phi)),n=Tlength)
  hola<-acf(y)[1]
  acf_est[i]<-as.numeric(hola$acf)
  sesgo_est[i]<-as.numeric(hola$acf)-phi
  desv_est[i]<-(as.numeric(hola$acf)-phi)^2
}

sesgo<-mean(sesgo_est);sesgo
desv<-mean(desv_est);desv
norm<-tseries::jarque.bera.test(acf_est)
p_valor<-norm$p.value;p_valor

## phi= -0.5 ####
phi=-0.5 # determinamos el valor de phi
Tlength=500 # determinamos el tamaño de la muestra

set.seed(276) #fijamos la semilla
sesgo_est<-c()
desv_est<-c()
acf_est<-c()

for(i in 1:500){
  y=arima.sim(list(order =c(1,0,0),ar=c(phi)),n=Tlength)
  hola<-acf(y)[1]
  acf_est[i]<-as.numeric(hola$acf)
  sesgo_est[i]<-as.numeric(hola$acf)-phi
  desv_est[i]<-(as.numeric(hola$acf)-phi)^2
}

sesgo<-mean(sesgo_est);sesgo
desv<-mean(desv_est);desv
norm<-tseries::jarque.bera.test(acf_est)
p_valor<-norm$p.value;p_valor