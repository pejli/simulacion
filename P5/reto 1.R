pi=3.1415
replicas<-30
muestras<-c(100, 1000,10000,100000)
numeros<-numeric()
errores<-numeric()
muestra<-numeric()
pis<-numeric()

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

calculando <- function() {
  xs <- runif(i,min=-0.5,max=0.5)
  ys <- runif(i,min=-0.5,max=0.5)
  in.circle <- xs^2 + ys^2 <= 0.5^2
  numero <- (sum(in.circle)/i)*4
  return(numero)
}
pis <- numeric()
replica<-numeric()
muestra<-numeric()
for(i in muestras){
  for(r in 1:replicas){
    montecarlo<-foreach(i = 1:500, .combine=c) %dopar% calculando()
    real <- sum(montecarlo) / 500
    numeros <- c(numeros, real)
    error<-((abs(pi-real))/(pi))*100
    errores<-c(errores,error)
  }
}
stopImplicitCluster()
total<-replicas*(length(muestras))
pis<-rep(3.1415,total)
replica<-rep(1:replicas,length(muestras))
for (i in muestras) {
  muestra <- c(muestra, c(rep(i, replicas)))
}
datos<-data.frame()
datos<-cbind(replica,muestra,numeros,pis,errores)
png("reto.png")
colores<-c("cadetblue", "cadetblue1","cadetblue2","cadetblue3")
boxplot(datos$errores ~ datos$muestra,xlab="muestra", ylab="porcentage de error", col=colores )
graphics.off()

png("reto1.png")
colores<-c("cadetblue", "cadetblue1","cadetblue2","cadetblue3")
boxplot(datos$numeros ~ datos$muestra,xlab="muestra", ylab="numeros", col=colores )
graphics.off()
