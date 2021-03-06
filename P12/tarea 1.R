#LIBRERIAS
library(doParallel)
library(ggplot2)
cluster <- makeCluster(detectCores() - 1)
#FUNCIONES
binario <- function(d, l) {
  b <-  rep(FALSE, l)
  while (l > 0 | d > 0) {
    b[l] <- (d %% 2 == 1)
    l <- l - 1
    d <- bitwShiftR(d, 1)
  }
  return(b)
}
decimal <- function(bits, l) {
  valor <- 0
  for (pos in 1:l) {
    valor <- valor + 2^(l - pos) * bits[pos]
  }
  return(valor)
}

#LECTURA Y PROBABILIDADES 
modelos <- read.csv("modelo", sep=" ", header=FALSE, stringsAsFactors=F)
modelos[modelos=='n'] <- 0.995
modelos[modelos=='g'] <- 0.92
modelos[modelos=='b'] <- 0.002

#VARIABLES 
r <- 5
c <- 3
dim <- r * c
tranqui <- 0.99
tope <- 9
digitos <- 0:tope
k <- length(digitos)
replica<-30
datos<-data.frame(R�plica= integer(), Entrenamiento=integer(), Prueba=integer(), M�todo=character(),
                  
                  Tiempo= integer(), Porcentaje=integer())
##########################SIMULACI�N###############################
for(tmax in c(3000,4000,5000)){
entrenamiento <- ceiling(0.7 * tmax)
prueba <- tmax - entrenamiento
for (replicas in 1: replica ){
  print(replicas)
#VARIABLES
tasa <- 0.15
contadores <-vector()
n <- floor(log(k-1, 2)) + 1
neuronas <- matrix(runif(n * dim), nrow=n, ncol=dim) # perceptrones
####################PARALELO####################################
###############################################################
#ENTRENAMIENTO
inicio<-as.numeric(Sys.time())
for (t in 1:entrenamiento) { # entrenamiento
  d <- sample(0:tope, 1)
  pixeles <- runif(dim) < modelos[d + 1,]
  correcto <- binario(d, n)
  for (i in 1:n) {
    w <- neuronas[i,]
    deseada <- correcto[i]
    resultado <- sum(w * pixeles) >= 0
    if (deseada != resultado) {
      ajuste <- tasa * (deseada - resultado)
      tasa <- tranqui * tasa
      neuronas[i,] <- w + ajuste * pixeles
    }
  }
}
clusterExport(cluster, c( "neuronas", "binario", "decimal", "modelos", "tope" ,"k", "dim", "n"))
#PRUEBA 
contadores <-parSapply(cluster, 1:prueba , function(x){
  d <- sample(0:tope, 1)
  pixeles <- runif(dim) < modelos[d + 1,] # fila 1 contiene el cero, etc.
  correcto <-binario(d, n)
  salida <- rep(FALSE, n)
  for (i in 1:n) {
    w <- neuronas[i,]
    deseada <- correcto[i]
    resultado <- sum(w * pixeles) >= 0
    salida[i] <- resultado
  }
  r <- min(decimal(salida, n), k) 
  return(r==d)})
final<-as.numeric(Sys.time())
datos<-rbind(datos, data.frame(R�plica= replicas, Entrenamiento=entrenamiento, Prueba=prueba, M�todo="Paralelo",
                               
                               Tiempo= (final-inicio), Porcentaje=(sum(contadores)/prueba)*100))
######################SECUENCIAL################################
################################################################
tasa <- 0.15
contadores <- logical()
n <- floor(log(k-1, 2)) + 1
neuronas <- matrix(runif(n * dim), nrow=n, ncol=dim) # perceptrones
inicio<-as.numeric(Sys.time())
#ENTRENAMIENTO
for (t in 1:entrenamiento) { # entrenamiento
  d <- sample(0:tope, 1)
  pixeles <- runif(dim) < modelos[d + 1,]
  correcto <- binario(d, n)
  for (i in 1:n) {
    w <- neuronas[i,]
    deseada <- correcto[i]
    resultado <- sum(w * pixeles) >= 0
    if (deseada != resultado) {
      ajuste <- tasa * (deseada - resultado)
      tasa <- tranqui * tasa
      neuronas[i,] <- w + ajuste * pixeles
    }
  }
}
#PRUEBA 
for (t in 1:prueba) { # prueba
  d <- sample(0:tope, 1)
  pixeles <- runif(dim) < modelos[d + 1,] # fila 1 contiene el cero, etc.
  correcto <- binario(d, n)
  salida <- rep(FALSE, n)
  for (i in 1:n) {
    w <- neuronas[i,]
    deseada <- correcto[i]
    resultado <- sum(w * pixeles) >= 0
    salida[i] <- resultado
  }
  r <- min(decimal(salida, n), k) # todos los no-existentes van al final
  p<-(r==d)
  contadores<- c(contadores,p)
}
final<-as.numeric(Sys.time())
datos<-rbind(datos, data.frame(R�plica= replicas, Entrenamiento=entrenamiento, Prueba=prueba, M�todo="Secuencial",
                               
                               Tiempo= (final-inicio), Porcentaje=(sum(contadores)/prueba)*100))
}
}
stopCluster(cluster)
##GRAFICAS 

ggplot(data=datos, aes(x =as.factor(Prueba), y=Tiempo, fill=M�todo))+geom_boxplot()+ xlab("iteraciones de prueba ") + ylab("tiempo(s)")

