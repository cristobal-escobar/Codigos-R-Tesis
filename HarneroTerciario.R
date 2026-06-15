# Cargar Paquetes
library(readxl)
library(tidyverse)
library(fitdistrplus)



#Buscar Ruta archivo
file.choose()

#Copiar Ruta de Consola
ruta_excel<- "C:\\Users\\escobarc0705\\OneDrive - ARCADIS\\Escritorio\\Chancado\\Bases de Datos\\DatosChancado.xlsx"

#Hojas de Excel
excel_sheets(ruta_excel)

#Importar Datos de Excel de Harnero Terciario
datos<- read_excel(ruta_excel,sheet = "Harnero Terciario")


# TIEMPO ENTRE FALLAS

# Falla GENERAL
tiempo_h <- datos$`Tiempo Entre Fallas`
status_h<- datos$Status...3

# Quitamos los NaN
tiempo_h <- tiempo_h%>%
  na.omit()

status_h<- status_h%>%
  na.omit()

# Ajustar la distribución con datos censurados
f <- data.frame(
  left = tiempo_h, 
  right = ifelse(status_h == 1, tiempo_h, NA)
)

# Asegurar que no existan datos NAN en columna izquierda
colnames(f) <- c("left", "right")
f_w <- f[!is.na(f$left), ]

# Distribuciones tiempo entre fallas GENERALES
a_W <- fitdistcens(f_w, "weibull")
a_n <- fitdistcens(f_w, "norm")
a_ln <- fitdistcens(f_w, "lnorm")
a_exp<- fitdistcens(f_w, "exp")  

# Parametros
summary(a_W)
summary(a_n)
summary(a_ln)
summary(a_exp)


# Graficamos y vemos cual se acerca más 

par(mfrow = c(1,1), mar = c(4,4,2,1))
plot.legend <- c("Weibull","normal", "lognormal", "exponencial")
cdfcompcens(list(a_W,a_n,a_ln,a_exp), legendtext = plot.legend,main = "Tiempo entre fallas Generales")

# Graficamos Distribuciones obtenidas
tiempos <- seq(min(tiempo_h), max(tiempo_h), length.out = 1000)
den_weibull <- dweibull(tiempos, scale = a_W$estimate["scale"], shape = a_W$estimate["shape"]) 
den_normal <- dnorm(tiempos,mean = a_n$estimate["mean"],sd = a_n$estimate["sd"])
den_log <- dlnorm(tiempos, meanlog = a_ln$estimate["meanlog"],sdlog = a_ln$estimate["sdlog"])
den_exp <- dexp(tiempos, rate = a_exp$estimate["rate"])
par(mfrow = c(2,2), mar = c(4,4,2,1))
plot(tiempos,den_weibull,main = "Weibull", xlab = "Horas")
plot(tiempos,den_normal,main = "Normal", xlab = "Horas")
plot(tiempos,den_log,main = "Lognormal", xlab = "Horas")
plot(tiempos,den_exp,main = "Exponencial", xlab = "Horas")

# Histograma
par(mfrow = c(1,1), mar = c(4,4,2,1))
hist(tiempo_h, breaks = 10, prob = TRUE, main ="Histograma y curva aproximada", ylab = "Densidad")
lines(tiempos,den_log,col = "red",lwd = 2)
par(mfrow = c(1,1), mar = c(4,4,2,1))
hist(tiempo_h, prob = TRUE, col = "lightgray",
     main = "Comparación de distribuciones",
     xlab = "Horas", ylab = "Densidad")
lines(tiempos, den_weibull, col = "red", lwd = 2)
lines(tiempos, den_normal, col = "blue", lwd = 2)
lines(tiempos, den_log, col = "green", lwd = 2)
lines(tiempos, den_exp, col = "purple", lwd = 2)
legend("topright",
       legend = c("Weibull", "Normal", "Lognormal","Exponencial"),
       col = c("red", "blue", "green","purple"),
       lwd = 2)


# TIME TO REPAIR (Cuanto dura la falla)

falla_h <- datos$`Fallas Generales`

# Quitamos los NaN 

falla_h <- falla_h%>%
  na.omit()

tln <- fitdist(falla_h,"lnorm")
tg <- fitdist(falla_h,"gamma")
texp <- fitdist(falla_h,"exp")

# Parametros
summary(tln)
summary(tg)
summary(texp)

# Graficamos
par(mfrow = c(1,1), mar = c(4,4,2,1))
plot.legend <- c("lognormal","gamma","exponecial")
cdfcomp(list(tln,tg,texp),legendtext = plot.legend,main = "Tiempo reparacion fallas generales")


# Graficamos Distribuciones obtenidas
tiemposF_1 <- seq(min(falla_h), max(falla_h), length.out = 1000)
den_log4 <- dlnorm(tiemposF_1, meanlog = tln$estimate["meanlog"],sdlog = tln$estimate["sdlog"])
den_gamma4 <-dgamma(tiemposF_1,shape = tg$estimate["shape"], rate = tg$estimate["rate"])
den_exp4 <- dexp(tiemposF_1, rate = texp$estimate["rate"])
par(mfrow = c(2,2), mar = c(4,4,2,1))
plot(tiemposF_1,den_log4,main = "Lognormal", xlab = "Horas")
plot(tiemposF_1,den_gamma4,main = "gamma", xlab = "Horas")
plot(tiemposF_1,den_exp4,main = "Exponencial", xlab = "Horas")

# Histograma
par(mfrow = c(1,1), mar = c(4,4,2,1))
hist(falla_h, breaks = 10, prob = TRUE, main ="Histograma y curva aproximada", ylab = "Densidad")
lines(tiemposF_1,den_log4,col = "red",lwd = 2)
par(mfrow = c(1,1), mar = c(4,4,2,1))
hist(falla_h, prob = TRUE, col = "lightgray",
     main = "Comparación de distribuciones",
     xlab = "Horas", ylab = "Densidad")
lines(tiemposF_1, den_log4, col = "red", lwd = 2)
lines(tiemposF_1, den_gamma4, col = "blue", lwd = 2)
lines(tiemposF_1, den_exp4, col = "green", lwd = 2)
legend("topright",
       legend = c("Lognormal", "gamma", "Exponencial"),
       col = c("red", "blue", "green"),
       lwd = 2)

# Manteciones

tiempo_MA <- datos$`Tiempo Entre Mantencion`
status_MA<- datos$Status...7

# Quitamos los NaN

tiempo_MA <- tiempo_MA%>%
  na.omit()

status_MA<- status_MA%>%
  na.omit()


# Ajustar la distribución con datos censurados
f_ma <- data.frame(
  left = tiempo_MA, 
  right = ifelse(status_MA == 1, tiempo_MA, NA)
)

# Asegurar que no existan datos NAN en columna izquierda
colnames(f_ma) <- c("left", "right")
f_MA <- f_ma[!is.na(f_ma$left), ]


# Distribuciones tiempo entre MANTENCIONES
a_W2 <- fitdistcens(f_MA, "weibull")
a_n2 <- fitdistcens(f_MA, "norm")
a_ln2 <- fitdistcens(f_MA, "lnorm")
a_exp2<- fitdistcens(f_MA, "exp")

# Parametros
summary(a_W2)
summary(a_n2)
summary(a_ln2)
summary(a_exp2)

# Graficamos 
par(mfrow = c(1,1), mar = c(4,4,2,1))
plot.legend <- c("Weibull","normal", "lognormal","exponencial")
cdfcompcens(list(a_W2,a_n2,a_ln2,a_exp2), legendtext = plot.legend,main = "Tiempo entre Mantenciones")


# Graficamos Distribuciones obtenidas
tiempos_1 <- seq(min(tiempo_MA), max(tiempo_MA), length.out = 1000)
den_weibull1 <- dweibull(tiempos_1, scale = a_W2$estimate["scale"], shape = a_W2$estimate["shape"]) 
den_normal1 <- dnorm(tiempos_1,mean = a_n2$estimate["mean"],sd = a_n2$estimate["sd"])
den_log1 <- dlnorm(tiempos_1, meanlog = a_ln2$estimate["meanlog"],sdlog = a_ln2$estimate["sdlog"])
den_exp1 <- dexp(tiempos_1, rate = a_exp2$estimate["rate"])
par(mfrow = c(2,2), mar = c(4,4,2,1))
plot(tiempos_1,den_weibull1,main = "Weibull", xlab = "Horas")
plot(tiempos_1,den_normal1,main = "Normal", xlab = "Horas")
plot(tiempos_1,den_log1,main = "Lognormal", xlab = "Horas")
plot(tiempos_1,den_exp1,main = "Exponencial", xlab = "Horas")


# Histograma
par(mfrow = c(1,1), mar = c(4,4,2,1))
hist(tiempo_MA, breaks = 10, prob = TRUE, main ="Histograma y curva aproximada", ylab = "Densidad")
lines(tiempos_1,den_log1,col = "red",lwd = 2)
par(mfrow = c(1,1), mar = c(4,4,2,1))
hist(tiempo_MA, prob = TRUE, col = "lightgray",
     main = "Comparación de distribuciones",
     xlab = "Horas", ylab = "Densidad")
lines(tiempos_1, den_weibull1, col = "red", lwd = 2)
lines(tiempos_1, den_normal1, col = "blue", lwd = 2)
lines(tiempos_1, den_log1, col = "green", lwd = 2)
lines(tiempos_1, den_exp1, col = "purple", lwd = 2)
legend("topright",
       legend = c("Weibull", "Normal", "Lognormal","Exponencial"),
       col = c("red", "blue", "green","purple"),
       lwd = 2)




# Duracion de MANTENCIONES

falla_MA <- datos$Mantencion

# Quitamos los NaN 

falla_MA <- falla_MA%>%
  na.omit()

tln1 <- fitdist(falla_MA,"lnorm")
tg1 <- fitdist(falla_MA,"gamma")
texp1 <- fitdist(falla_MA,"exp")

# Parametros
summary(tln1)
summary(tg1)
summary(texp1)

# Graficamos
par(mfrow = c(1,1), mar = c(4,4,2,1))
plot.legend <- c("lognormal","gamma","exponecial")
cdfcomp(list(tln1,tg1,texp1),legendtext = plot.legend,main = "Duracion de Mantencion")


# Graficamos Distribuciones obtenidas
tiemposF_2 <- seq(min(falla_MA), max(falla_MA), length.out = 1000)
den_log5 <- dlnorm(tiemposF_2, meanlog = tln1$estimate["meanlog"],sdlog = tln1$estimate["sdlog"])
den_gamma5 <-dgamma(tiemposF_2,shape = tg1$estimate["shape"], rate = tg1$estimate["rate"])
den_exp5 <- dexp(tiemposF_2, rate = texp1$estimate["rate"])
par(mfrow = c(2,2), mar = c(4,4,2,1))
plot(tiemposF_2,den_log5,main = "Lognormal", xlab = "Horas")
plot(tiemposF_2,den_gamma5,main = "gamma", xlab = "Horas")
plot(tiemposF_2,den_exp5,main = "Exponencial", xlab = "Horas")

# Histograma
par(mfrow = c(1,1), mar = c(4,4,2,1))
hist(falla_MA, breaks = 20, prob = TRUE, main ="Histograma y curva aproximada", ylab = "Densidad")
lines(tiemposF_2,den_log5,col = "red",lwd = 2)
par(mfrow = c(1,1), mar = c(4,4,2,1))
hist(falla_MA, prob = TRUE, col = "lightgray",
     main = "Comparación de distribuciones",
     xlab = "Horas", ylab = "Densidad")
lines(tiemposF_2, den_log5, col = "red", lwd = 2)
lines(tiemposF_2, den_gamma5, col = "blue", lwd = 2)
lines(tiemposF_2, den_exp5, col = "green", lwd = 2)
legend("topright",
       legend = c("Lognormal", "gamma", "Exponencial"),
       col = c("red", "blue", "green"),
       lwd = 2)

