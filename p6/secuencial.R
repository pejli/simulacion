l <- 1.5
n <- 50
pr <- 0.02
v <- l / 30
replicas<-30
timex<-numeric()
for (i in 1:3) {
  times<-0
  conta<-numeric()
  tinicial<-Sys.time()
  for (pi in c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)) {
    print(pi)
    for(r in 1:replicas){
      agentes <- data.frame(x = double(), y = double(), dx = double(), dy = double(), estado  = character())
      for (i in 1:n) {
        e <- "S"
        if (runif(1) < pi) {
          e <- "I"
        }
        agentes <- rbind(agentes, data.frame(x = runif(1, 0, l), y = runif(1, 0, l),
                                             dx = runif(1, -v, v), dy = runif(1, -v, v),
                                             estado = e))
      }
      levels(agentes$estado) <- c(levels(agentes$estado), "R")
      
      epidemia <- integer()
      r <- 0.1
      tmax <- 100
      for (tiempo in 1:tmax) {
        infectados <- dim(agentes[agentes$estado == "I",])[1]
        epidemia <- c(epidemia, infectados)
        if (infectados == 0) {
          break
        }
        contagios <- rep(FALSE, n)
        for (i in 1:n) { # posibles contagios
          a1 <- agentes[i, ]
          if (a1$estado == "I") { # desde los infectados
            for (j in 1:n) {
              if (!contagios[j]) { # aun sin contagio
                a2 <- agentes[j, ]
                if (a2$estado == "S") { # hacia los susceptibles
                  dx <- a1$x - a2$x
                  dy <- a1$y - a2$y
                  d <- sqrt(dx^2 + dy^2)
                  if (d < r) { # umbral
                    p <- (r - d) / r
                    if (runif(1) < p) {
                      contagios[j] <- TRUE
                    }
                  }
                }
              }
            }
          }
        }
        for (i in 1:n) { # movimientos y actualizaciones
          a <- agentes[i, ]
          if (contagios[i]) {
            a$estado <- "I"
          } else if (a$estado == "I") { # ya estaba infectado
            if (runif(1) < pr) {
              a$estado <- "R" # recupera
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
          agentes[i, ] <- a
        }
      }
      conta<-c(conta,epidemia)
    }
  }
  tfinal<-Sys.time()
  times<-tfinal-tinicial
  timex<-c(timex,times)
}

 


 #########################CONSTRUCCION DEL DATA#########################
  porcentaje<-numeric()
  porcentaje<-c(porcentaje,(conta/n)*100)
  tiempox<-numeric()
  replicax<-numeric()
  for(i in 1:replicas){
    replicax <- c(replicax, rep(i, tmax))
    tiempox <- c(tiempox, 1:tmax)
  }
  replicax<-rep(replicax, 9)
  tiempox<-rep(tiempox,9)
  probabilidad<-numeric()
  probabilidad<-c(rep(0.1,3000),rep(0.2,3000),rep(0.3,3000),rep(0.4,3000),rep(0.5,3000),rep(0.6,3000),rep(0.7,3000),rep(0.8,3000),rep(0.9,3000))
  noparalelizado<-data.frame()
  noparalelizado<-cbind(probabilidad,replicax,tiempox,conta,porcentaje)
  