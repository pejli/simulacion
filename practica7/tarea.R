#declaracion de variables 
low <- -3
high<- -low
tmax<-50
step<-0.3
replicas<-10
wolfram<-0.06822
#declaracion de funciones 
g <- function(x,y) {
  z<-((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100
  return(z)
}
camino <- data.frame(x = numeric(), y = numeric(), gxy = numeric(),
                     replica = integer(), tiempo = integer())

for(i in 1:10){
  curr <- c(runif(1, low, high),runif(1, low, high))
  best <- curr
  for(j in 1:tmax){
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
    if(g(curr[1], curr[2])>g(best[1],best[2])){
      best<-curr
    }
    camino <- rbind(camino, data.frame(x = best[1], y = best[2], gxy = g(best[1],best[2]),
                                       replica = i, tiempo = j))
  }
}

#GRAFICA
png("resultados.png",width = 500,height = 300)
ggplot(camino, aes(tiempo, gxy, group = replica, color = replica)) + geom_point() + geom_line()+labs(x="Paso",y="g(x,y)")    
graphics.off()
  