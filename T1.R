data<- data.frame()
d<-0
  for (dimension in 1:8){
    d<-0
    probabilidad <- 0
     for(replicas in 1:10){
      pos <- rep(0, dimension)
      for(duracion in 1:2**7){
        cambiar <- sample(1:dimension, 1)
        cambio <- 1
        if (runif(1) < 0.5) {
          cambio <- -1
        }
        pos[cambiar] <- pos[cambiar] + cambio
        #print(pos)
        if(all(pos==0)){
          d=d+1
        break()
      }
      }
     }
    probabilidad<-d/30
    data <- rbind(data, probabilidad)
  }
png("sinnombre7.png")
  plot(data.matrix(data), xlab="dimension", ylab="probabilidad", main="duracion")
