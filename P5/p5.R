#VARIABLEs
replicas<-30
muestras<-c(100, 1000,10000,100000,1000000)
desde <- 3
hasta <- 7
cuantos<-500
wolfram<-0.048834
#FUNCIONES
f <- function(x) { return(1 / (exp(x) + exp(-x))) }
g <- function(x) { return((2 / pi) * f(x)) }
suppressMessages(library(distr))
generador  <- r(AbscontDistribution(d = g)) # creamos un generador
parte <- function() {
  valores <- generador(n)
  return(p <- sum(valores >= desde & valores <= hasta))
}
datos<-data.frame()
digitos <- numeric()
wolframs <- numeric()
replica<-numeric()
muestra<-numeric()
numeros<-numeric()
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
for(n in muestras){
  for(r in 1:replicas){
    montecarlo <- foreach(i = 1:cuantos, .combine=c) %dopar% parte()
    stopImplicitCluster()
    integral <- sum(montecarlo) / (cuantos * n)
    numero=(pi / 2) * integral
    numeros<-c(numeros,numero)
    
      for(i in 1:6){
          if((signif(numero,digits = i))==(signif(wolfram,digits = i))){
            digito=i+1
          }
        else{
          break
        }
      }
    digitos<-c(digitos,digito)
  }
}
total<-replicas*(length(muestras))
wolframs<-rep(wolfram,total)
replica<-rep(1:replicas,length(muestras))
for (i in muestras) {
  muestra <- c(muestra, c(rep(i, replicas)))
}
datos<-cbind(replica,muestra,wolframs,numeros,digitos)
png("tarea.png")
colores<-c("cadetblue", "cadetblue1", "cadetblue2","cadetblue3", "cadetblue4")
boxplot(datos$digitos ~ datos$muestra,xlab="muestra", ylab="digitos", col=colores )
graphics.off()
