library(lattice)
library(reshape2)
library(latticeExtra)
low <- -3
high<- -low
step<-0.3
g <- function(x,y) {
  z<-((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100
  return(z)
}
x <- seq(low, high, 0.05)
y <- x
z <- outer(x, y, g)
dimnames(z) <- list(x, y)
d <- melt(z)
names(d) <- c("x", "y", "z")

replicas<-15
tmax<-30
coordenadas<-data.frame()
 #generar las 15 réplicas(puntos)
 coordenadas<-data.frame()
  for(i in 1:replicas){
     i <- c(runif(1, low, high),runif(1, low, high))
     coordenadas<-rbind(coordenadas,i)
  }
 names(coordenadas)<-c("x","y")
 i <-0
 salida <- paste("p7_t", i, ".png", sep="")
 encabezado<- paste("Inicio")
 plano <- levelplot(z ~ x * y, data = d, main = encabezado)
 puntos <- (xyplot(coordenadas$x ~ coordenadas$y, col = "black"))
 graficapuntos <- plano + as.layer(puntos)
 png(salida, width = 500, height = 500)
 print(graficapuntos)
 graphics.off()
for(i in 1:tmax){
  for (j in 1:replicas) {
       curr <- coordenadas[j,]
       delta<-runif(1,0,step)
       vp <- data.frame(numeric(), numeric(), numeric())
    for (dx in c(curr[1]-delta, curr[1],curr[1]+delta) ) {
           for (dy in c(curr[2]-delta, curr[2],curr[2]+delta)) {
               if (dx != curr[1] | dy != curr[2]) { # descartar la posicion misma
                   vp <- rbind(vp, c(dx, dy,g(dx,dy)))
                   
               }
          }
  }
  names(vp)<-c("x","y","g")
  indice<-which(vp$g %in% max(vp$g))
  curr<-vp[indice,1:2]
  coordenadas[j,] <- curr
  }
  
  salida <- paste("p7_t", i, ".png", sep="")
  encabezado<- paste("paso ",i, sep = "")
  plano <- levelplot(z ~ x * y, data = d, main = encabezado)
  puntos <- (xyplot(coordenadas$x ~ coordenadas$y, col = "black"))
  graficapuntos <- plano + as.layer(puntos)
  png(salida, width = 500, height = 500)
  print(graficapuntos)
  graphics.off()
}
 names(coordenadas)<-c("x","y")

