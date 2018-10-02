####LIBRERIAS
library(testit)
####FUNCIONES
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
  res <- integer()
  for (cumulo in 1:cuantos) {
    if (runif(1) < rotura(tam)) {
      primera <- sample(1:(tam-1), 1)
      segunda <- tam - primera
      res <- c(res, primera, segunda)
    } else {
      res <- c(res, tam)
    }
  }
  return(res)
}
unirse <- function(tam, cuantos) {
  res <- integer()
  for (cumulo in 1:cuantos) {
    if (runif(1) < union(tam)) {
      res <- c(res, -tam) # marcamos con negativo los que quieren unirse
    } else {
      res <- c(res, tam)
    }
  }
  return(res)
}
####VARIABLES
k<-10000
n<-1000000
replicas<-30
duraciones<-c(5,10,15,30,60)
timexp<-numeric()
datos1<-data.frame()
####EXPERIMENTO
for (duracion in duraciones) {
for (replica in 1:replicas) {
  times<-0
  timeinicial<-Sys.time()
  k<-10000
  n<-1000000
  cumulos<-inicio(n,k)
  c <- median(cumulos) # tamanio critico de cumulos
  d <- sd(cumulos) / 4
  assert(length(cumulos[cumulos == 0]) == 0)
  assert(sum(cumulos) == n)
  freq <- as.data.frame(table(cumulos))
  names(freq) <- c("tam", "num")
  freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
  for (paso in 1:duracion) {
    assert(sum(cumulos) == n)
    cumulos <- integer()
    for (i in 1:dim(freq)[1]) { # fase de rotura
      urna <- freq[i,]
      if (urna$tam > 1) { # no tiene caso romper si no se puede
        cumulos <- c(cumulos, romperse(urna$tam, urna$num))
      } else {
        cumulos <- c(cumulos, rep(1, urna$num))
      }
    }
    assert(sum(cumulos) == n)
    assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
    freq <- as.data.frame(table(cumulos)) # actualizar urnas
    names(freq) <- c("tam", "num")
    freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
    assert(sum(freq$num * freq$tam) == n)
    cumulos <- integer()
    for (i in 1:dim(freq)[1]) { # fase de union
      urna <- freq[i,]
      cumulos <- c(cumulos, unirse(urna$tam, urna$num))
    }
    assert(sum(abs(cumulos)) == n)
    assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
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
  datos1<-rbind(datos1,c(times,paso,replica))
  
 }
}
names(datos1)<-c("tiempo","duracion","replica")
