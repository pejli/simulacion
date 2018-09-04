#Práctica 4; Diagramas de Voronoi
#n tamaño de la zona, varía 50,100,150,200 
#k numero de semillas inicialmente 20,40,60,80

#Experimento

source("vp.R")
source("vc.R")
source("celda.R")
source("inicio.R")
source("propaga.R")

resultados <- numeric()


#n tamaño de la zona
for (n in c(100, 150, 200, 250)){
  print(paste("zona ", n))
  for(k in c(20,40,60,80)){
    print(paste("semilla igual a ", k))
    limite <- n
    zona <- matrix(rep(0, n * n), nrow = n, ncol = n)
    x <- rep(0, k) # ocupamos almacenar las coordenadas x de las semillas
    y <- rep(0, k) # igual como las coordenadas y de las semillas
    
    for (semilla in 1:k) {
      while (TRUE) { # hasta que hallamos una posicion vacia para la semilla
        fila <- sample(1:n, 1)
        columna <- sample(1:n, 1)
        if (zona[fila, columna] == 0) {
          zona[fila, columna] = semilla
          x[semilla] <- columna
          y[semilla] <- fila
          break
        }
      }
    }
    suppressMessages(library(doParallel))
    registerDoParallel(makeCluster(detectCores()))
    celdas <- foreach(p = 1:(n * n), .combine=c) %dopar% celda(p)
    stopImplicitCluster()
    voronoi <- matrix(celdas, nrow = n, ncol = n, byrow=TRUE)
   
     suppressMessages(library(doParallel))
    registerDoParallel(makeCluster(detectCores()))
    largos <- foreach(r = 1:200, .combine=c) %dopar% propaga(r)
    stopImplicitCluster()
    summary(largos)
    
    resultados <- c(resultados, largos)
  }
}


tablita<-matrix(resultados,nrow =3200,ncol=2,byrow = TRUE)
tablita<-as.data.frame(tablita)
names(tablita)<-c("grieta","manhatan")
replicas<-(rep(1:200,16))
matricula<-c(rep(100,800),rep(150,800),rep(200,800),rep(250,800))
p<-c(rep(20,200),rep(40,200),rep(60,200),rep(80,200))
semillas<-rep(p,4)
datos<-data.frame()
datos<-cbind(matricula,semillas,replicas,tablita)

png("zonas contra manhatan.png")
colores<-c("hotpink3","hotpink4","mediumorchid2","lightpink1")
boxplot(datos$manhatan ~datos$matricula, xlab="zonas", ylab="distancia Manhatan",col=colores)
png("zonas contra grieta.png")
boxplot(datos$grieta ~datos$matricula, xlab="zonas", ylab="largo de la grieta",col=colores)
graphics.off()

for (i in c(100, 150, 200, 250)) {
  png(paste("largogrieta", i, "semillas.png"))
  boxplot(datos$grieta[which(datos$matricula == i)] ~datos$semillas[which(datos$matricula == i)],
          ylab = "largo de la grieta", xlab = "semillas", col = colores,main=paste("Zona",i))
  graphics.off()
  
  png(paste("manhatan", i, "semillas.png"))
  boxplot(datos$manhatan[which(datos$matricula == i)] ~datos$semillas[which(datos$matricula == i)],
          ylab = "manhatan", xlab = "semillas", col = colores,main=paste("Zona",i))
  graphics.off()
}
