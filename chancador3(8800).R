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

#Importar Datos de Excel de Chancador Terciario 
datos<- read_excel(ruta_excel,sheet = "ChancadorN3 (8800)")


# TIEMPO ENTRE FALLAS

# Falla ELECTRICA
tiempo_E <- datos$`Tiempo Entre Fallas E`
status_E<- datos$Status_M

# Quitamos los NaN

tiempo_E <- tiempo_E%>%
  na.omit()

status_E<- status_E%>%
  na.omit()


# Ajustar la distribución con datos censurados
f <- data.frame(
  left = tiempo_E, 
  right = ifelse(status_E == 1, tiempo_E, NA)
)

# Asegurar que no existan datos NAN en columna izquierda
colnames(f) <- c("left", "right")
f_w <- f[!is.na(f$left), ]

# Distribuciones tiempo fallas
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
plot.legend <- c("Weibull","normal", "lognormal","exponencial")
cdfcompcens(list(a_W,a_n,a_ln,a_exp), legendtext = plot.legend,main = "Tiempo entre fallas Electricas")

# Graficamos Distribuciones obtenidas
tiempos <- seq(min(tiempo_E), max(tiempo_E), length.out = 1000)
den_weibull <- dweibull(tiempos, scale = a_W$estimate["scale"], shape = a_W$estimate["shape"]) 
den_normal <- dnorm(tiempos,mean = a_n$estimate["mean"],sd = a_n$estimate["sd"])
den_log <- dlnorm(tiempos, meanlog = a_ln$estimate["meanlog"],sdlog = a_ln$estimate["sdlog"])
den_exp <- dexp(tiempos, rate = a_exp$estimate["rate"])
par(mfrow = c(2,2), mar = c(4,4,2,1))
plot(tiempos,den_weibull,main = "Weibull", xlab = "Horas")
plot(tiempos,den_normal,main = "Normal", xlab = "Horas")
plot(tiempos,den_log,main = "Lognormal", xlab = "Horas")
plot(tiempos,den_exp,main = "Exponencial", xlab = "Horas")
par(mfrow = c(1,1), mar = c(4,4,2,1))


# Histograma
hist(tiempo_E, prob = TRUE, main ="Histograma y curva aproximada", ylab = "Densidad")
lines(tiempos,den_exp,col = "red",lwd = 2)
par(mfrow = c(1,1), mar = c(4,4,2,1))
hist(tiempo_E, prob = TRUE, col = "lightgray",
     main = "Comparación de distribuciones",
     xlab = "Horas", ylab = "Densidad")
lines(tiempos, den_weibull, col = "red", lwd = 2)
lines(tiempos, den_normal, col = "blue", lwd = 2)
lines(tiempos, den_log, col = "green", lwd = 2)
lines(tiempos, den_exp, col = "purple", lwd = 2)
legend("topright",
       legend = c("Weibull", "Normal", "Lognormal", "Exponencial"),
       col = c("red", "blue", "green", "purple"),
       lwd = 2)



# Falla ELECTRONICA/INSTRUMENTACION
tiempo_EL <- datos$`Tiempo Entre Fallas EL`
status_EL<- datos$Status_EL

# Quitamos los NaN 

tiempo_EL <- tiempo_EL%>%
  na.omit()
status_EL <- status_EL%>%
  na.omit()


# Ajustar la distribución con datos censurados
f_d <- data.frame(
  left = tiempo_EL, 
  right = ifelse(status_EL == 1, tiempo_EL, NA)
)

# Asegurar que no existan datos NAN en columna izquierda
colnames(f_d) <- c("left", "right")
f_D <- f_d[!is.na(f_d$left), ]


# Distribuciones tiempo entre fallas ELECTRONICAS/INSTRUMENTACION
a_W2 <- fitdistcens(f_D, "weibull")
a_n2 <- fitdistcens(f_D, "norm")
a_ln2 <- fitdistcens(f_D, "lnorm")
#a_exp2<- fitdistcens(f_D, "exp")    #No se ajusta a una exponencial

# Parametros
summary(a_W2)
summary(a_n2)
summary(a_ln2)


# Graficamos 
par(mfrow = c(1,1), mar = c(4,4,2,1))
plot.legend <- c("Weibull","normal", "lognormal")
cdfcompcens(list(a_W2,a_n2,a_ln2), legendtext = plot.legend,main = "Tiempo entre fallas Electronicas")


# Graficamos Distribuciones obtenidas
tiempos_1 <- seq(min(tiempo_EL), max(tiempo_EL), length.out = 1000)
den_weibull1 <- dweibull(tiempos_1, scale = a_W2$estimate["scale"], shape = a_W2$estimate["shape"]) 
den_normal1 <- dnorm(tiempos_1,mean = a_n2$estimate["mean"],sd = a_n2$estimate["sd"])
den_log1 <- dlnorm(tiempos_1, meanlog = a_ln2$estimate["meanlog"],sdlog = a_ln2$estimate["sdlog"])
par(mfrow = c(2,2), mar = c(4,4,2,1))
plot(tiempos_1,den_weibull1,main = "Weibull", xlab = "Horas")
plot(tiempos_1,den_normal1,main = "Normal", xlab = "Horas")
plot(tiempos_1,den_log1,main = "Lognormal", xlab = "Horas")


# Histograma
par(mfrow = c(1,1), mar = c(4,4,2,1))
hist(tiempo_EL, prob = TRUE, main ="Histograma y curva aproximada", ylab = "Densidad")
lines(tiempos_1,den_log1,col = "red",lwd = 2)
par(mfrow = c(1,1), mar = c(4,4,2,1))
hist(tiempo_EL, prob = TRUE, col = "lightgray",
     main = "Comparación de distribuciones",
     xlab = "Horas", ylab = "Densidad")
lines(tiempos_1, den_weibull1, col = "red", lwd = 2)
lines(tiempos_1, den_normal1, col = "blue", lwd = 2)
lines(tiempos_1, den_log1, col = "green", lwd = 2)
legend("topright",
       legend = c("Weibull", "Normal", "Lognormal"),
       col = c("red", "blue", "green"),
       lwd = 2)


# Falla OPERACIONAL
tiempo_O <- datos$`Tiempo Entre Fallas O`
status_O<- datos$Status_O

# Quitamos los NaN 

tiempo_O <- tiempo_O%>%
  na.omit()

status_O<- status_O%>%
  na.omit()


#Ajustar la distribución con datos censurados
f_o <- data.frame(
  left = tiempo_O, 
  right = ifelse(status_O == 1, tiempo_O, NA)
)

# Asegurar que no existan datos NAN en columna izquierda
colnames(f_o) <- c("left", "right")
f_O <- f_o[!is.na(f_o$left), ]

# Distribuciones tiempo entre fallas OPERACIONALES
a_W4 <- fitdistcens(f_O, "weibull")
a_n4 <- fitdistcens(f_O, "norm")
a_ln4 <- fitdistcens(f_O, "lnorm")
a_exp4<- fitdistcens(f_O, "exp")  

# Parametros
summary(a_W4)
summary(a_n4)
summary(a_ln4)
summary(a_exp4)

# Graficamos 
par(mfrow = c(1,1), mar = c(4,4,2,1))
plot.legend <- c("Weibull","normal", "lognormal","exponencial")
cdfcompcens(list(a_W4,a_n4,a_ln4,a_exp4), legendtext = plot.legend,main = "Tiempo entre fallas operacionales")


# Graficamos Distribuciones obtenidas
tiempos_3 <- seq(min(tiempo_O), max(tiempo_O), length.out = 1000)
den_weibull3 <- dweibull(tiempos_3, scale = a_W4$estimate["scale"], shape = a_W4$estimate["shape"]) 
den_normal3 <- dnorm(tiempos_3,mean = a_n4$estimate["mean"],sd = a_n4$estimate["sd"])
den_log3 <- dlnorm(tiempos_3, meanlog = a_ln4$estimate["meanlog"],sdlog = a_ln4$estimate["sdlog"])
den_exp3 <- dexp(tiempos_3, rate = a_exp4$estimate["rate"])
par(mfrow = c(2,2), mar = c(4,4,2,1))
plot(tiempos_3,den_weibull3,main = "Weibull", xlab = "Horas")
plot(tiempos_3,den_normal3,main = "Normal", xlab = "Horas")
plot(tiempos_3,den_log3,main = "Lognormal", xlab = "Horas")
plot(tiempos_3,den_exp3,main = "Exponencial", xlab = "Horas")



# Histograma
par(mfrow = c(1,1), mar = c(4,4,2,1))
hist(tiempo_O, prob = TRUE, main ="Histograma y curva aproximada", ylab = "Densidad")
lines(tiempos_3,den_weibull3,col = "red",lwd = 2)
par(mfrow = c(1,1), mar = c(4,4,2,1))
hist(tiempo_O, prob = TRUE, col = "lightgray",
     main = "Comparación de distribuciones",
     xlab = "Horas", ylab = "Densidad")
lines(tiempos_3, den_weibull3, col = "red", lwd = 2)
lines(tiempos_3, den_normal3, col = "blue", lwd = 2)
lines(tiempos_3, den_log3, col = "green", lwd = 2)
lines(tiempos_3, den_exp3, col = "purple", lwd = 2)
legend("topright",
       legend = c("Weibull", "Normal", "Lognormal","Exponencial"),
       col = c("red", "blue", "green","purple"),
       lwd = 2)


# TIME TO REPAIR (CUANTO DURA LA FALLA)

# Fallas ELECTRICAS

falla_E <- datos$Falla_E

# Quitamos los NaN 

falla_E <- falla_E%>%
  na.omit()

tln <- fitdist(falla_E,"lnorm")
tg <- fitdist(falla_E,"gamma")
texp <- fitdist(falla_E,"exp")

# Parametros
summary(tln)
summary(tg)
summary(texp)

# Graficamos
par(mfrow = c(1,1), mar = c(4,4,2,1))
plot.legend <- c("lognormal","gamma","exponecial")
cdfcomp(list(tln,tg,texp),legendtext = plot.legend,main = "Tiempo reparacion fallas Electricas")


# Graficamos Distribuciones obtenidas
tiemposF_1 <- seq(min(falla_E), max(falla_E), length.out = 1000)
den_log4 <- dlnorm(tiemposF_1, meanlog = tln$estimate["meanlog"],sdlog = tln$estimate["sdlog"])
den_gamma4 <-dgamma(tiemposF_1,shape = tg$estimate["shape"], rate = tg$estimate["rate"])
den_exp4 <- dexp(tiemposF_1, rate = texp$estimate["rate"])
par(mfrow = c(2,2), mar = c(4,4,2,1))
plot(tiemposF_1,den_log4,main = "Lognormal", xlab = "Horas")
plot(tiemposF_1,den_gamma4,main = "gamma", xlab = "Horas")
plot(tiemposF_1,den_exp4,main = "Exponencial", xlab = "Horas")


# Histograma
par(mfrow = c(1,1), mar = c(4,4,2,1))
max_y2 <- max(hist(falla_E, plot=FALSE)$density, den_log4)
hist(falla_E, breaks = 15, prob = TRUE, ylim = c(0,max_y2), main ="Comparación de distribuciones", ylab = "Densidad",xlab = "Horas")
lines(tiemposF_1, den_log4, col = "red", lwd = 2)
lines(tiemposF_1, den_gamma4, col = "blue", lwd = 2)
lines(tiemposF_1, den_exp4, col = "green", lwd = 2)
legend("topright",
       legend = c("Lognormal", "Gamma", "Exponencial"),
       col = c("red", "blue", "green"),
       lwd = 2)

# Fallas ELECTRONICAS/INSTRUMENTACION

falla_EL <- datos$Falla_EL

# Quitamos los NaN 
falla_EL <- falla_EL%>%
  na.omit()

tln1 <- fitdist(falla_EL,"lnorm")
tg1 <- fitdist(falla_EL,"gamma")
texp1 <- fitdist(falla_EL,"exp")

# Parametros
summary(tln1)
summary(tg1)
summary(texp1)

# Graficamos
par(mfrow = c(1,1), mar = c(4,4,2,1))
plot.legend <- c("lognormal","gamma","exponecial")
cdfcomp(list(tln1,tg1,texp1),legendtext = plot.legend,main = "Tiempo reparacion fallas electronicas")


# Graficamos Distribuciones obtenidas
tiemposF_2 <- seq(min(falla_EL), max(falla_EL), length.out = 1000)
den_log5 <- dlnorm(tiemposF_2, meanlog = tln1$estimate["meanlog"],sdlog = tln1$estimate["sdlog"])
den_gamma5 <-dgamma(tiemposF_2,shape = tg1$estimate["shape"], rate = tg1$estimate["rate"])
den_exp5 <- dexp(tiemposF_2, rate = texp1$estimate["rate"])
par(mfrow = c(2,2), mar = c(4,4,2,1))
plot(tiemposF_2,den_log5,main = "Lognormal", xlab = "Horas")
plot(tiemposF_2,den_gamma5,main = "gamma", xlab = "Horas")
plot(tiemposF_2,den_exp5,main = "Exponencial", xlab = "Horas")


# Histograma
par(mfrow = c(1,1), mar = c(4,4,2,1))
hist(falla_EL, prob = TRUE, main ="Histograma y curva aproximada", ylab = "Densidad")
lines(tiemposF_2,den_gamma5,col = "red",lwd = 2)
par(mfrow = c(1,1), mar = c(4,4,2,1))
hist(falla_EL, prob = TRUE, col = "lightgray",
     main = "Comparación de distribuciones",
     xlab = "Horas", ylab = "Densidad")
lines(tiemposF_2, den_log5, col = "red", lwd = 2)
lines(tiemposF_2, den_gamma5, col = "blue", lwd = 2)
lines(tiemposF_2, den_exp5, col = "green", lwd = 2)
legend("topright",
       legend = c("Weibull", "Normal", "Lognormal"),
       col = c("red", "blue", "green"),
       lwd = 2)



# Fallas OPERACIONALES
falla_O <- datos$Falla_O

# Quitamos los NaN 
falla_O <- falla_O%>%
  na.omit()

tln3 <- fitdist(falla_O,"lnorm")
tg3 <- fitdist(falla_O,"gamma")
texp3 <- fitdist(falla_O,"exp")

# Parametros
summary(tln3)
summary(tg3)
summary(texp3)


# Graficamos
par(mfrow = c(1,1), mar = c(4,4,2,1))
plot.legend <- c("lognormal","gamma","exponecial")
cdfcomp(list(tln3,tg3,texp3),legendtext = plot.legend, main = "Tiempo reparacion fallas operacionales")

# Graficamos Distribuciones obtenidas
tiemposF_4 <- seq(min(falla_O), max(falla_O), length.out = 1000)
den_log7 <- dlnorm(tiemposF_4, meanlog = tln3$estimate["meanlog"],sdlog = tln3$estimate["sdlog"])
den_gamma7 <-dgamma(tiemposF_4,shape = tg3$estimate["shape"], rate = tg3$estimate["rate"])
den_exp7 <- dexp(tiemposF_4, rate = texp3$estimate["rate"])
par(mfrow = c(2,2), mar = c(4,4,2,1))
plot(tiemposF_4,den_log7,main = "Lognormal", xlab = "Horas")
plot(tiemposF_4,den_gamma7,main = "gamma", xlab = "Horas")
plot(tiemposF_4,den_exp7,main = "Exponencial", xlab = "Horas")


# Histograma
par(mfrow = c(1,1), mar = c(4,4,2,1))
hist(falla_O, prob = TRUE, main ="Histograma y curva aproximada", ylab = "Densidad")
lines(tiemposF_4,den_log7,col = "red",lwd = 2)
par(mfrow = c(1,1), mar = c(4,4,2,1))
hist(falla_O, prob = TRUE, col = "lightgray",
     main = "Comparación de distribuciones",
     xlab = "Horas", ylab = "Densidad")
lines(tiemposF_4, den_log7, col = "red", lwd = 2)
lines(tiemposF_4, den_gamma7, col = "blue", lwd = 2)
lines(tiemposF_4, den_exp7, col = "green", lwd = 2)
legend("topright",
       legend = c("Lognormal", "Gamma", "Exponencial"),
       col = c("red", "blue", "green"),
       lwd = 2)


# Mantenciones
tiempo_MA <- datos$`Tiempo Entre MA`
status_MA<- datos$Status_MA

# Quitamos los NaN 
tiempo_MA <- tiempo_MA%>%
  na.omit()

status_MA<- status_MA%>%
  na.omit()


#Ajustar la distribución con datos censurados
f_ma <- data.frame(
  left = tiempo_MA, 
  right = ifelse(status_MA == 1, tiempo_MA, NA)
)

# Asegurar que no existan datos NAN en columna izquierda
colnames(f_ma) <- c("left", "right")
f_MA <- f_ma[!is.na(f_ma$left), ]

# Distribuciones tiempo entre MANTENCIONES
a_W5 <- fitdistcens(f_MA, "weibull")
a_n5 <- fitdistcens(f_MA, "norm")
a_ln5 <- fitdistcens(f_MA, "lnorm")
a_exp5<- fitdistcens(f_MA, "exp") 

# Parametros
summary(a_W5)
summary(a_n5)
summary(a_ln5)
summary(a_exp5)


# Graficamos 
par(mfrow = c(1,1), mar = c(4,4,2,1))
plot.legend <- c("Weibull","normal", "lognormal","exponencial")
cdfcompcens(list(a_W5,a_n5,a_ln5,a_exp5), legendtext = plot.legend,main = "Tiempo entre Mantenciones")


# Graficamos Distribuciones obtenidas
tiempos_4 <- seq(min(tiempo_MA), max(tiempo_MA), length.out = 1000)
den_weibullMA <- dweibull(tiempos_4, scale = a_W5$estimate["scale"], shape = a_W5$estimate["shape"]) 
den_normalMA <- dnorm(tiempos_4,mean = a_n5$estimate["mean"],sd = a_n5$estimate["sd"])
den_logMA <- dlnorm(tiempos_4, meanlog = a_ln5$estimate["meanlog"],sdlog = a_ln5$estimate["sdlog"])
den_expMA <- dexp(tiempos_4, rate = a_exp5$estimate["rate"])
par(mfrow = c(2,2), mar = c(4,4,2,1))
plot(tiempos_4,den_weibullMA,main = "Weibull", xlab = "Horas")
plot(tiempos_4,den_normalMA,main = "Normal", xlab = "Horas")
plot(tiempos_4,den_logMA,main = "Lognormal", xlab = "Horas")
plot(tiempos_4,den_expMA,main = "Exponencial", xlab = "Horas")


# Histograma
par(mfrow = c(1,1), mar = c(4,4,2,1))
max_y7 <- max(hist(tiempo_MA, plot=FALSE)$density, den_expMA)
hist(tiempo_MA, breaks = 10, prob = TRUE,ylim = c(0,max_y7), main ="Comparación de distribuciones", ylab = "Densidad",xlab = "Horas")
lines(tiempos_4, den_weibullMA, col = "red", lwd = 2)
lines(tiempos_4, den_normalMA, col = "blue", lwd = 2)
lines(tiempos_4, den_logMA, col = "green", lwd = 2)
lines(tiempos_4, den_expMA, col = "purple", lwd = 2)
legend("topright",
       legend = c("Weibull", "Normal", "Lognormal","Exponencial"),
       col = c("red", "blue", "green","purple"),
       lwd = 2)



# Duración de MANTENCIONES

falla_MA <- datos$Falla_MA

# Quitamos los NaN

falla_MA <- falla_MA%>%
  na.omit()
tln4 <- fitdist(falla_MA,"lnorm")
tg4 <- fitdist(falla_MA,"gamma")
texp4 <- fitdist(falla_MA,"exp")

# Parametros
summary(tln4)
summary(tg4)
summary(texp4)


# Graficamos
par(mfrow = c(1,1), mar = c(4,4,2,1))
plot.legend <- c("lognormal","gamma","exponecial")
cdfcomp(list(tln4,tg4,texp4),legendtext = plot.legend, main = "Tiempo de demora de mantenciones")

# Graficamos Distribuciones obtenidas
tiemposF_5 <- seq(min(falla_MA), max(falla_MA), length.out = 1000)
den_log8 <- dlnorm(tiemposF_5, meanlog = tln4$estimate["meanlog"],sdlog = tln4$estimate["sdlog"])
den_gamma8 <-dgamma(tiemposF_5,shape = tg4$estimate["shape"], rate = tg4$estimate["rate"])
den_exp8 <- dexp(tiemposF_5, rate = texp4$estimate["rate"])
par(mfrow = c(2,2), mar = c(4,4,2,1))
plot(tiemposF_5,den_log8,main = "Lognormal", xlab = "Horas")
plot(tiemposF_5,den_gamma8,main = "gamma", xlab = "Horas")
plot(tiemposF_5,den_exp8,main = "Exponencial", xlab = "Horas")


# Histograma
par(mfrow = c(1,1), mar = c(4,4,2,1))
max_y8 <- max(hist(falla_MA, plot=FALSE)$density, den_log8)
hist(falla_MA, breaks = 10, prob = TRUE,ylim = c(0,max_y8), main ="Comparación de distribuciones", ylab = "Densidad",xlab = "Horas")
lines(tiemposF_5, den_log8, col = "red", lwd = 2)
lines(tiemposF_5, den_gamma8, col = "blue", lwd = 2)
lines(tiemposF_5, den_exp8, col = "green", lwd = 2)
legend("topright",
       legend = c("Lognormal", "Gamma", "Exponencial"),
       col = c("red", "blue", "green"),
       lwd = 2)



