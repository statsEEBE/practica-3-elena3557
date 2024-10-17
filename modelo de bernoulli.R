#Pregunta1

#Modelo de Bernoulli

x <- c(0,1) #valores que puede tomar la variable aleatoria
f <- c(0.68, 0.32) #función de probabilidad

plot(x,f, type="h", ylim=c(0,1), col="red")

#Binomial: repetición de ensayos de Bernoulli; cuentos los 1

#Y= numero de personas que tienen como mínimo dos televisores

n <- 43
sample(x,n, f, replace=TRUE)
#obtengo todos los resultado; saca numeros aleatorios de una función de probabilidad

y<- sum(sample(x,n, f, replace=TRUE)) 
#total de 1 que tengo; el valor me variará

Y <- function(i){sum(sample(x,n, f, replace=TRUE))}
Y(i)

#quiero hacer cuando el número de encuestas tiende a infinito

encuestas <- sapply(1:40,Y)
#sapply me permite hacer un bucle
encuestas
#cada uno de los valores que me sale es el resultado de un experimento de bernullu

table(encuestas)

#ahora hago más encuestas
encuestas <- sapply(1:400000,Y)
fr <- table(encuestas)/400000 #lo divido entre el número de encuestas para tener el valor relativo

fr["13"] #el valor que tenga aquí será mi resultado

#BINOMIAL Y-> Binom(n,p)
dbinom(13,43,0.32)
#probabilidad de tener 13 muestras positivas en la encuesta de 43, cuando la pribabilidad de que alguien me diga que si es de 0.32
y <- 0:43
plot(y,dbinom(y,43,0.32), type="h", col='red')
#el type h es para que me haga las lineas horizontales


#PREGUNTA 2
#si me preguntaran la probabilidad que una muestra de 44 tenga 17 llars que como mínimo etngan dos televisores

dbinom(17,44,0.32)

#ahora me pregunta que P(Y<17) = P(Y<=16) = F(16)
#la función de distribución pbim
pbinom(16,44,0.32) #RESPUESTA DE LA PREGUNTA 2

#cálculo de la mediana
qbinom(0.5,44,0.32) #Respuesta: 14

#calculo del primer quartil
qbinom(0.25,44,0.32) #Respuesta: 12


#PREGUNTA3
#a) n*p= 24*0.68 media
#b) n*p*(1-p) = 24*0.68*0.32 varianza
#c) primer cuartil
qbinom(0.25, 24, 0.68)

#PREGUNTA4
#n*p= 46*0,32