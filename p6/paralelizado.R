##################VARIABLES############################
l <- 1.5
proba<-c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
n <- 50
pr <- 0.02
v <- l / 30
replicas<-30
conta<-numeric()
r <- 0.1
tmax <- 100
timexp<-numeric()
library(parallel)
###################FUNCIONES############################
inicial<-function(i){
  e <- 1
  if (runif(1) < pi) {
    e <- 2
  }
  return(c(x = runif(1, 0, l), y = runif(1, 0, l),
           dx = runif(1, -v, v), dy = runif(1, -v, v),
           estado = e))
}

contagiar<-function(i){
  a1 <- agentes[i, ]
  if (a1$estado == 1) {# desde los susceptibles
    for (j in which(agentes$estado==2)) {
      a2 <- agentes[j, ]
      dx <- a1$x - a2$x
      dy <- a1$y - a2$y
      d <- sqrt(dx^2 + dy^2)
      if (d < r) { # umbral
        p <- (r - d) / r
        if (runif(1) < p) {
          return(TRUE)
          break()
        }
      }
    }
    return(FALSE)
  }else{
    return(FALSE)
  }
}

movientox<-function(i){
  # movimientos
  a <- agentes[i, ]
  if (contagios[i]) {
    a$estado <- 2
  } else if (a$estado == 2) { # ya estaba infectado
    if (runif(1) < pr) {
      a$estado <- 3 # recupera
    }
  }
  a$x <- a$x + a$dx
  a$y <- a$y + a$dy
  if (a$x > l) {
    a$x <- a$x - l
  }
  if (a$y > l) {
    a$y <- a$y - l
  }
  if (a$x < 0) {
    a$x <- a$x + l
  }
  if (a$y < 0) {
    a$y <- a$y + l
  }
  return(c(x=a$x,y=a$y,dx=a$dx,dy=a$dy,estado=a$estado))
}
################PARALLE###############################

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "l")
clusterExport(cluster, "v")
clusterExport(cluster, "r")
clusterExport(cluster, "pr")
for(i in 1:3){
  times<-0
  contap<-numeric()
  tinicial<-Sys.time() 
for(pi in proba){
  clusterExport(cluster, "pi")
  for (replica in 1:replicas) {
    
    agentes <- data.frame(t(parSapply(cluster, 1:n, inicial)))
    levels(agentes$estado) <- c(levels(agentes$estado), 3)
    epidemia <- integer()
    
    for (tiempo in 1:tmax) {
      clusterExport(cluster, "agentes")
      infectados <- dim(agentes[agentes$estado == 2,])[1]
      epidemia <- c(epidemia, infectados)
      if (infectados == 0) {
        break
      }
      
      contagios <- parSapply(cluster, 1:n, contagiar)
      clusterExport(cluster, "contagios")
      
      agentes <- data.frame(t(parSapply(cluster,1:n,movientox)))
    }
    contap <- c(contap, epidemia)
  }
}
  tfinal<-Sys.time()
  times<-tfinal-tinicial
  timexp<-c(timexp,times)
}
porcentajep<-numeric()
porcentajep<-(contap*100)/n
