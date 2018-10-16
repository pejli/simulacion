#LIBRERIAS
library(testit)
library(parallel)
#VARIABLES GENERALES
pm <- 0.05
rep <- 50
tmax <- 50
init<-200
#FUNCIONES 
knapsack <- function(cap, peso, valor) {
  n <- length(peso)
  pt <- sum(peso) 
  assert(n == length(valor))
  vt <- sum(valor) 
  if (pt < cap) { 
    return(vt)
  } else {
    filas <- cap + 1 
    cols <- n + 1 
    tabla <- matrix(rep(-Inf, filas * cols),
                    nrow = filas, ncol = cols) 
    for (fila in 1:filas) {
      tabla[fila, 1] <- 0 
    }
    rownames(tabla) <- 0:cap 
    colnames(tabla) <- c(0, valor) 
    for (objeto in 1:n) { 
      for (acum in 1:(cap+1)) { # consideramos cada fila de la tabla
        anterior <- acum - peso[objeto]
        tabla[acum, objeto + 1] <- tabla[acum, objeto]               
        if (anterior > 0) { # si conocemos una combinacion con ese peso
          tabla[acum, objeto + 1] <- max(tabla[acum, objeto], tabla[anterior, objeto] + valor[objeto])
        }
      }
    }
    return(max(tabla))
  }
}
factible <- function(seleccion, pesos, capacidad) {
  return(sum(seleccion * pesos) <= capacidad)
}
objetivo <- function(seleccion, valores) {
  return(sum(seleccion * valores))
}
normalizar <- function(data) {
  menor <- min(data)
  mayor <- max(data)
  rango <- mayor - menor
  data <- data - menor # > 0
  return(data / rango) # entre 0 y 1
}
generador.pesos <- function(cuantos, min, max) {
  return(sort(round(normalizar(rnorm(cuantos)) * (max - min) + min)))
}
generador.valores <- function(pesos, min, max) {
  n <- length(pesos)
  valores <- double()
  for (i in 1:n) {
    media <- pesos[n]
    desv <- runif(1)
    valores <- c(valores, rnorm(1, media, desv))
  }
  valores <- normalizar(valores) * (max - min) + min
  return(valores)
}
poblacion.inicial <- function(n, tam) {
  pobl <- matrix(rep(FALSE, tam * n), nrow = tam, ncol = n)
  for (i in 1:tam) {
    pobl[i,] <- round(runif(n))
  }
  return(as.data.frame(pobl))
}
mutacion <- function(sol, n) {
  pos <- sample(1:n, 1)
  mut <- sol
  mut[pos] <- (!sol[pos]) * 1
  return(mut)
}
reproduccion <- function(x, y, n) {
  pos <- sample(2:(n-1), 1)
  xy <- c(x[1:pos], y[(pos+1):n])
  yx <- c(y[1:pos], x[(pos+1):n])
  return(c(xy, yx))
}
muttacion<-function(i){
  if (runif(1) < pm) {
    mutado <- mutacion(p[i,], n)
    
  }
  else{
    mutado<-rep(2,n)
  }
  return(as.numeric(mutado))
}
reproduxionreto<-function(i){
  padres <- sample(1:200, 2, replace=FALSE, prob =p$obj )
  hijos <- reproduccion(p[padres[1],], p[padres[2],], n)
  return(as.numeric(hijos))
}
reproduxion<-function(i){
  padres <- sample(1:tam, 2, replace=FALSE)
  hijos <- reproduccion(p[padres[1],], p[padres[2],], n)
  return(as.numeric(hijos))
}
objetivox<-function(i){
  return(objetivo(p[i,], valores))
}
factiblex<-function(i){
  return(factible(p[i,], pesos, capacidad))
}
#DATA.FRAME
datos<-data.frame(Réplica=integer(),Optimo=integer(), 
                  Método=character(),Objetivo=integer())
cluster <- makeCluster(detectCores() - 1)
n=30
init=200

  for (replicas in 1:3) {
    print(replicas)
    #######CON RUTELA
    p <- poblacion.inicial(n, init)
    pesos <-generador.pesos(n, 15, 80)
    valores <- generador.valores(pesos, 10, 500)
    capacidad <- round(sum(pesos) * 0.65)
    optimo <- knapsack(capacidad, pesos, valores)
    tam <- dim(p)[1]
    assert(tam == init)
    mejores <- double()
    for (iter in 1:tmax) {
      p$obj <- NULL
      p$fact <- NULL
      #Paralelizar "mutación"
      clusterExport(cluster,c("p" ,"pm","mutacion","n") )
      pp<-t(parSapply(cluster,1:tam,muttacion))
      pp<-pp[pp[,1] < 2,]
      #p<-rbind(p,pp)
      #AGREGAR OBJETIVO
      #Paralelizar objetivo
      tam <- dim(p)[1]
      clusterExport(cluster,c("objetivo", "valores","p"))
      obj<-parSapply(cluster, 1:tam,objetivox)
      p<-cbind(p,obj)
      suma<-sum(obj)
      #obtener la probabilidad
      p$obj<-p$obj/suma
      #Paralelizar "reproducir"
      clusterExport(cluster,c("p","reproduccion", "tam","n"))
      ppp<-parSapply(cluster, 1:rep, reproduxionreto)
      pppp<-(matrix(unlist(ppp), ncol=n ))
      p$obj<-NULL
      p<-rbind(p,pp,pppp)
      tam <- dim(p)[1]
      #Paralelizar factibilidad
      clusterExport(cluster,c("pesos","factible","capacidad","p"))
      fact<-parSapply(cluster, 1:tam,factiblex)
      #Paralelizar objetivo
      clusterExport(cluster,c("objetivo","valores","p"))
      obj<-parSapply(cluster, 1:tam,objetivox)
      p<-cbind(p,fact,obj)
      #FINAL
      mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
      p <- p[mantener,]
      tam <- dim(p)[1]
      assert(tam == init)
      factibles <- p[p$fact == TRUE,]
      mejor <- max(factibles$obj)
      mejores <- c(mejores, mejor)
    }
    datos<-rbind(datos,data.frame(Réplica=replicas,Optimo=optimo,
                                  Método="Con ruleta",Objetivo=(max(mejores))))    
    p <- poblacion.inicial(n, init)
    tam <- dim(p)[1]
    assert(tam == init)
    mejores <- double()
    for (iter in 1:tmax) {
      p$obj <- NULL
      p$fact <- NULL
      
      #Paralelizar "mutación"
      clusterExport(cluster,c("p" ,"pm","mutacion","n") )
      pp<-t(parSapply(cluster,1:tam,muttacion))
      pp<-pp[pp[,1] < 2,]
      p<-rbind(p,pp)
      
      #Paralelizar "reproducir"
      clusterExport(cluster,c("p","reproduccion","tam","n"))
      ppp<-parSapply(cluster, 1:rep, reproduxion)
      pppp<-(matrix(unlist(ppp), ncol=n ))
      p$obj<-NULL
      p<-rbind(p,pp,pppp)
      tam <- dim(p)[1]
      obj <- double()
      fact <- integer()
      #Paralelizar factibilidad
      clusterExport(cluster,c("pesos","factible","capacidad","p","fact"))
      fact<-parSapply(cluster, 1:tam,factiblex)
      p<-cbind(p,fact)
      #Paralelizar objetivo
      obj <- double()
      clusterExport(cluster,c("objetivo","obj","valores","p"))
      obj<-parSapply(cluster, 1:tam,objetivox)
      p<-cbind(p,obj)
      #FINAL
      mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
      p <- p[mantener,]
      tam <- dim(p)[1]
      assert(tam == init)
      factibles <- p[p$fact == TRUE,]
      mejor <- max(factibles$obj)
      mejores <- c(mejores, mejor)
    }
    datos<-rbind(datos,data.frame(Réplica=replicas,Optimo=optimo, Método="Sin ruleta",Objetivo=(max(mejores))))
  }  

stopCluster(cluster)




