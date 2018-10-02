####LIBRERIAS####
library(parallel)
library(testit)
####FUNCIONES####
inicio<-function(n,k){
  originales <- rnorm(k)
  cumulos <- originales - min(originales) + 1
  cumulos <- round(n * cumulos / sum(cumulos))
  diferencia <- n - sum(cumulos)
  if (diferencia > 0) {
    for (i in 1:diferencia) {
      p <- sample(1:k, 1)
      cumulos[p] <- cumulos[p] + 1
    }
  } else if (diferencia < 0) {
    for (i in 1:-diferencia) {
      p <- sample(1:k, 1)
      if (cumulos[p] > 1) {
        cumulos[p] <- cumulos[p] - 1
      }
    }
  }
return(cumulos)
}
rotura <- function(x) {
  return (1 / (1 + exp((c - x) / d)))
}
union <- function(x) {
  return (exp(-x / c))
}
romperse <- function(tam, cuantos) {
  romper <- round(rotura(tam) * cuantos) # independientes
  resultado <- rep(tam, cuantos - romper) # los demas
  if (romper > 0) {
    for (cumulo in 1:romper) { # agregar las rotas
      t <- 1
      if (tam > 2) { # sample no jala con un solo valor
        t <- sample(1:(tam-1), 1)
      }
      resultado <- c(resultado, t, tam - t)
    }
  }
  assert(sum(resultado) == tam * cuantos) # no hubo perdidas
  return(resultado)
}
unirse <- function(tam, cuantos) {
  unir <- round(union(tam) * cuantos) # independientes
  if (unir > 0) {
    division <- c(rep(-tam, unir), rep(tam, cuantos - unir))
    assert(sum(abs(division)) == tam * cuantos)
    return(division)
  } else {
    return(rep(tam, cuantos))
  }
}
fase1<-function(i){
  urna <- freq[i,]
  if (urna$tam > 1) { # no tiene caso romper si no se puede
    cumulos <- romperse(urna$tam, urna$num)
    return(cumulos)
    }
  else {
    cumulos <- rep(1, urna$num)
    return(cumulos)
  }
  
} 
fase2<-function(i){
  urna <- freq[i,]
  cumulos <- unirse(urna$tam, urna$num)
  return(cumulos)
  }
####VARIABLES###
k<-10000
n<-1000000
replicas<-30
duraciones<-c(5,10,15,30,60)
timexp<-numeric()
datos<-data.frame()
####PARALLEL###
cluster <- makeCluster(detectCores() - 1)

  for(duracion in duraciones){ 
    print(duracion)
    cumulos<-inicio(n,k)
    c<-median(cumulos)
    d <- sd(cumulos) / 4
    assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
    assert(sum(cumulos) == n)
    freq<- as.data.frame(table(cumulos))
    names(freq) <- c("tam", "num")
    freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
    for (replica in 1:replicas) {
      times<-0
      timeinicial<-Sys.time()
     for (paso in 1:duracion) {
      clusterExport(cluster, "freq")
      clusterExport(cluster, "romperse")
      clusterExport(cluster, "rotura")
      clusterExport(cluster, "cumulos")
      clusterExport(cluster, "c")
      clusterExport(cluster, "d")
      clusterExport(cluster,"assert")
      clusterExport(cluster, "union")
      clusterExport(cluster,"unirse")
      ############FASE 1##########
      assert(sum(cumulos) == n)
      cumulos <- (parSapply(cluster, 1:dim(freq)[1], fase1))  #FASE 1
      cumulos <- unlist(cumulos)
  
    assert(sum(abs(cumulos)) == n)
    assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
    freq <- as.data.frame(table(cumulos)) # actualizar urnas
    names(freq) <- c("tam", "num")
    freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
    assert(sum(freq$num * freq$tam) == n)
    ############FASE 2##########
    clusterExport(cluster, "freq")
    cumulos <- (parSapply(cluster, 1:dim(freq)[1], fase2))  #FASE 2
    cumulos <- unlist(cumulos)
    juntarse <- -cumulos[cumulos < 0]
    cumulos <- cumulos[cumulos > 0]
    assert(sum(cumulos) + sum(juntarse) == n)
    nt <- length(juntarse)
    if (nt > 0) {
      if (nt > 1) {
        juntarse <- sample(juntarse)
        for (i in 1:floor(nt / 2) ) {
          cumulos <- c(cumulos, juntarse[2*i-1] + juntarse[2*i])
        }
      }
      if (nt %% 2 == 1) {
        cumulos <- c(cumulos, juntarse[nt])
      }
    }
    assert(sum(cumulos) == n)
    freq <- as.data.frame(table(cumulos))
    names(freq) <- c("tam", "num")
    freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
    assert(sum(freq$num * freq$tam) == n)
     }
      timefinal<-Sys.time()
      times<-timefinal- timeinicial
      datos<-data.frame(rbind(datos,c(times,paso,replica)))
    
    }
  }
stopCluster(cluster)

names(datos)<-c("tiempo","duracion","replica")
